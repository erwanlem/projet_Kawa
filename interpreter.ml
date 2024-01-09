open Kawa
open Typechecker

type value =
  | VInt  of int
  | VBool of bool
  | VObj  of obj
  | VArray of value array
  | Null
and obj = {
  cls:    string;
  fields: (string, value) Hashtbl.t;
}

exception Error of string
exception Return of value

(* Initialise les tableaux à une ou plusieurs dimensions *)
let rec init_array a =
  match a with
  | TArray (Some TArray(t, len'), len) -> let arr = Array.make len Null 
                                    in for i=0 to len-1 do arr.(i) <- init_array (TArray(t, len')) done; 
                                    VArray arr
  | TArray (_, len)               -> VArray (Array.make len Null)
  | _ -> assert false




let exec_prog (p: program): unit =
  let env = Hashtbl.create 16 in

  (* Ajout des variables globales à l'environnement *)
  List.iter (fun (x, t, _, _) ->
                  match t with
                  (* Si on a un tableau on l'initialise avec Null -> on peut directement accéder aux cases *)
                  | TArray(_, len) -> Hashtbl.add env x (init_array t)
                  | _ -> Hashtbl.add env x Null) p.globals;
  (* Met les classes dans une table de hachage *)
  let cls = Hashtbl.create 16 in
  (* Ajoute les classes dans une table de hachage pour accéder facilement *)
  List.iter (fun o -> 
    (* On ajoute les variables statiques à l'environnement comme des variables globales *)
   List.iter (fun (x, t, _, _) -> 
      match t with
      (* Si on a un tableau on l'initialise avec Null -> on peut directement accéder aux cases *)
      | TArray(_, len) -> Hashtbl.add env (o.class_name ^ "." ^ x) (init_array t)
      | _ -> Hashtbl.add env (o.class_name ^ "." ^ x) Null) o.statics;
   Hashtbl.add cls o.class_name o) p.classes;
  
  let rec eval_call f this args =
    let call_env = Hashtbl.create 16 in
    let () = (Hashtbl.add call_env "this" (VObj this) ) in
    let () = List.iter (fun (x, t, _, _) ->
                match t with
                | TArray(_, len) -> Hashtbl.add call_env x (init_array t)
                | _ -> Hashtbl.add call_env x Null) f.locals;
    let rec set_args arg_name arg =
      match arg_name, arg with
      | [], [] -> ()
      | (n, t, _, _) :: l, v :: l' -> Hashtbl.add call_env n v; set_args l l'
      | _, _ -> ()
    in set_args f.params args
    in exec_seq f.code call_env


  and exec_seq s lenv =
    let rec evali e = match eval e with
      | VInt n -> n
      | _ -> assert false
    and evalb e = match eval e with
      | VBool b -> b
      | _ -> assert false
    and evalo e = match eval e with
      | VObj o -> o
      | _ -> assert false
    and evala e = match eval e with
      | VArray a -> a
      | _ -> assert false
    
        
    and eval (e: expr): value = match e with
      | Int n  -> VInt n
      | Bool b -> VBool b
      | Binop(Add, e1, e2, _) -> VInt(evali e1 + evali e2)
      | Binop(Sub, e1, e2, _) -> VInt(evali e1 - evali e2)
      | Binop(Mul, e1, e2, _) -> VInt(evali e1 * evali e2)
      | Binop(Div, e1, e2, _) -> VInt(evali e1 / evali e2)
      | Binop(Rem, e1, e2, _) -> VInt(evali e1 mod evali e2)
      | Binop(Lt , e1, e2, _) -> VBool(evali e1 < evali e2)
      | Binop(Le , e1, e2, _) -> VBool(evali e1 <= evali e2)
      | Binop(Gt , e1, e2, _) -> VBool(evali e1 > evali e2)
      | Binop(Ge , e1, e2, _) -> VBool(evali e1 >= evali e2)
      | Binop(Neq, e1, e2, _) -> VBool(evali e1 <> evali e2)
      | Binop(Eq, e1, e2, _)  -> VBool(evali e1 = evali e2)
      | Binop(And, e1, e2, _) -> VBool(evalb e1 && evalb e2)
      | Binop(Or, e1, e2, _)  -> VBool(evalb e1 || evalb e2)
      | Binop(Streq,e1,e2, _) -> (match eval e1 with
                              | VInt n   -> let n' = evali e2 in VBool (n = n')
                              | VBool b  -> let b' = evalb e2 in VBool(b = b')
                              | Null     -> VBool (eval e2 = Null)
                              (* Compare tous les éléments des deux objets *)
                              | VObj o   -> let o' = evalo e2 in
                                  VBool(Hashtbl.fold (fun a b acc -> let v = Hashtbl.find o'.fields a in acc && (b = v)) o.fields true)
                              (* Compare les éléments des deux tableaux *)
                              | VArray a -> let a' = evala e2 in
                                  VBool (let eq = ref true in (Array.length a = Array.length a') 
                                  && (for i = 0 to Array.length a-1 do eq := (!eq) && a.(i)=a'.(i) done; !eq))  )

      | Binop(Strneq,e1,e2,_) -> (match eval e1 with
                              | VInt n -> let n' = evali e2 in VBool (n <> n')
                              | VBool b -> let b' = evalb e2 in VBool(b <> b')
                              | Null  -> VBool (eval e2 <> Null)
                              (* Compare tous les éléments des deux objets *)
                              | VObj o  -> let o' = evalo e2 in
                                  VBool(Hashtbl.fold (fun a b acc -> let v = Hashtbl.find o'.fields a in acc || (b <> v)) o.fields false)
                              (* Compare les éléments des deux tableaux *)
                              | VArray a -> let a' = evala e2 in
                                VBool (let eq = ref true in not (Array.length a = Array.length a') 
                                || not (for i = 0 to Array.length a-1 do eq := (!eq) && a.(i)=a'.(i) done; !eq)) )

      | Unop(Opp, e, _)       -> VInt(- evali e)
      | Unop(Not, e, _)       -> VBool(Bool.not (evalb e))
      | New(i)             -> (try let o = Hashtbl.find cls i 
                                    in match o.parent with
                                        | None -> let envo = Hashtbl.create 16  (* Cas sans parent *)
                                                  in List.iter (fun (x, t, _, s) -> match s, t with (* ajout des attributs à l'environnement *)
                                                                                    (* Si on a un tableau on l'initialise avec Null -> 
                                                                                       on peut directement accéder aux cases *)
                                                                                    | None, TArray (_, len) -> Hashtbl.add envo x (init_array t)
                                                                                    | None, _ -> Hashtbl.add envo x Null
                                                                                    | Some e, _ -> Hashtbl.add envo x (eval e)) o.attributes; 
                                                  VObj({cls=i; fields=envo})
                                        | Some p -> let parent = evalo (New p) (* Cas avec parent *)
                                                  in let envo = parent.fields 
                                                  in List.iter (fun (x, t, _, s) -> match s, t with (* ajout des attributs à l'environnement *)
                                                                                    (* Si on a un tableau on l'initialise avec Null -> 
                                                                                       on peut directement accéder aux cases *)
                                                                                    | None, TArray (_, len) -> Hashtbl.add envo x (init_array t)
                                                                                    | None, _ -> Hashtbl.add envo x Null
                                                                                    | Some e, _ -> Hashtbl.add envo x (eval e)) o.attributes; 
                                                  VObj({cls=i; fields=envo})
                                with Not_found -> raise (Error "Invalid class name") )

      | Array(a, _)        -> VArray (Array.of_list (List.rev (List.fold_left (fun acc e -> (eval e)::acc) [] a)) )
      
      | Get(m)          -> (match m with
                            | Var (v, _) -> (try Hashtbl.find lenv v
                                        with Not_found -> 
                                          (try Hashtbl.find env v with Not_found -> VObj {cls=v; fields=Hashtbl.create 1}) )
                            | Field(e', i, _) -> let o = evalo e'
                                              in (try Hashtbl.find o.fields i 
                                                  with Not_found -> Hashtbl.find env (o.cls ^ "." ^ i) )
                            | ArrayId(e, i, _) -> let a = evala e
                                              in let id = evali i
                                              in a.(id) )

      | This _              -> Hashtbl.find lenv "this"

      | Super _             -> let obj = Hashtbl.find lenv "this" in
                              let o = (match obj with VObj o -> o | _ -> error "Not an object") in
                              let cls_o = Hashtbl.find cls o.cls in
                              (match cls_o.parent with
                              | Some p -> VObj {cls = p; fields = o.fields}
                              | None   -> error ("No superclass for object of type " ^ cls_o.class_name))

      | MethCall(e1, i, l, _)  ->  let o = evalo e1
                                in let obj_class = Hashtbl.find cls o.cls
                                in (* Recherche la méthode correspondante et l'appelle *)
                                let rec find_mtd c =
                                  (try List.find (fun a -> a.method_name = i) c.methods
                                  with Not_found -> (match c.parent with
                                                    | None -> raise (Error ("Method '" ^ i ^ "' not found"))
                                                    | Some p -> find_mtd (Hashtbl.find cls p)
                                                    ) )
                                in let mtd = find_mtd obj_class
                                (* évaluation des paramètres *)
                                in let l = List.map eval l in
                                (try let () = eval_call mtd o l in Null with Return v -> v) (* Appel *)

      | NewCstr(i, l, _)       ->  let o = evalo (New i)
                                in let obj_class = Hashtbl.find cls o.cls
                                in let methods = obj_class.methods
                                in (* Appel du constructeur avec les paramètres donnés *)
                                let constr =  List.find (fun a -> a.method_name = "constructor") methods
                                in let l = List.map eval l
                                in (try let () = eval_call constr o l in VObj(o)
                                    with Return v -> raise (Error "Invalid construct return value"))

      | InstanceOf(e, s, _)    ->  let o = evalo e in
                                let c = Hashtbl.find cls o.cls in
                                (* Recherche la classe dans les classes mères de l'objet *)
                                let rec find_instance c =
                                  if c.class_name = s then VBool(true)
                                  else match c.parent with
                                        | None -> VBool(false)
                                        | Some p -> find_instance (Hashtbl.find cls p)
                                in find_instance c
      | Cast (t, e, l)         ->  let o = evalo e
                              (* Récupération de la classe de destination *)
                              in let c = (match t with Get (Var (c, _)) -> c | _ -> assert false)
                              (* Vérifie que la classe d'arrivée est classe mère de la classe de l'objet *)
                              in let rec is_parent = function
                              | None -> raise (Error ("Can't cast from "^o.cls^" to "^c))
                              | Some p -> if p = c then o else is_parent (Hashtbl.find cls p).parent
                              in if o.cls = c then (VObj o) else VObj (is_parent (Hashtbl.find cls o.cls).parent)
    in
  
    let rec exec (i: instr): unit = match i with
      | Print e           ->let rec print_expr = (function
                            | VInt v  -> Printf.printf "%d" v
                            | VBool b -> Printf.printf "%b" b
                            | VArray a -> Printf.printf "[ "; Array.iter (fun e -> print_expr e; Printf.printf "; ") a; Printf.printf "]"
                            | Null    -> Printf.printf "Null"
                            | _       -> Printf.printf "Obj" ) 
                          in print_expr (eval e); Printf.printf "\n%!"
      | If(e, i1, i2, _)     -> if evalb e then exec_seq i1 else exec_seq i2
      | Expr(e, _)           -> let _ = eval e in ()
      | Set(m, e, _) | SetDef(m, e, _) 
                          -> (match m with
                              | Var (v, _)        -> (try let _ = Hashtbl.find lenv v in Hashtbl.replace lenv v (eval e) 
                                                with Not_found -> let _ = Hashtbl.find env v in Hashtbl.replace env v (eval e) )
                              | Field(e', i, _) -> let o = evalo e' 
                                                in (
                                                try let _ = Hashtbl.find o.fields i in Hashtbl.replace o.fields i (eval e) 
                                                with Not_found -> Hashtbl.replace env (o.cls ^ "." ^ i) (eval e) )
                              | ArrayId(e', i, _) -> let a = evala e'
                                              in (match eval e with
                                              | VInt n -> a.(evali i) <- VInt n
                                              | VBool b -> a.(evali i) <- VBool b
                                              | VObj o -> a.(evali i) <- VObj o
                                              | VArray a -> a.(evali i) <- VArray a
                                              | _ -> ()) )
      | While(e, s, _) as i  -> if evalb e then (exec_seq s; exec i)
      | Return(e, _)         -> raise (Return (eval e) )
    and exec_seq s = 
      List.iter exec s
    in
    
    exec_seq s
  in
  List.iter (fun c -> exec_seq c.static_decl (Hashtbl.create 1)) p.classes;
  exec_seq p.main (Hashtbl.create 1)
