open Kawa
open Lexing

exception Error of string
exception Final of typ * bool
let error s = raise (Error s)
let type_error ty_actual ty_expected =
  error (Printf.sprintf "expected %s, got %s"
           (typ_to_string ty_expected) (typ_to_string ty_actual))
let type_error_line ty_actual ty_expected pos =
  error (Printf.sprintf "Line %d : expected %s, got %s"
          pos.pos_lnum (typ_to_string ty_expected) (typ_to_string ty_actual))

module Env = Map.Make(String)
type tenv = typ * visibility Env.t

let add_env l tenv =
  List.fold_left (fun env (x, t, v, _) -> Env.add x (t, DEFAULT) env) tenv l 
  

let typecheck_prog p =
  let tenv = add_env [("this", TVoid, DEFAULT, None)] (add_env p.globals Env.empty) in
  let cls = List.fold_left (fun env o -> Env.add o.class_name o env) Env.empty p.classes in

  let rec check e typ tenv pos =
    let typ_e = type_expr e tenv in
    match pos with
    | None -> if typ_e <> typ then type_error typ_e typ (* Si pas d'information sur la position *)
    | Some p -> if typ_e <> typ then type_error_line typ_e typ p

  and type_expr e tenv = match e with
    | Int i  -> TInt
    | Bool _ -> TBool
    | New(i) -> TClass i
    (* Binop INT *)
    | Binop(Add, e1, e2, l) | Binop(Sub, e1, e2, l) | Binop(Mul, e1, e2, l) 
    | Binop(Div, e1, e2, l) | Binop(Rem, e1, e2, l) -> check e1 TInt tenv (Some l); check e2 TInt tenv (Some l); TInt
    (* Binop BOOL *)
    | Binop(Lt , e1, e2, l) | Binop(Le , e1, e2, l) 
    | Binop(Gt , e1, e2, l) | Binop(Ge , e1, e2, l) -> check e1 TInt tenv (Some l); check e2 TInt tenv (Some l); TBool
    | Binop(Neq, e1, e2, l) | Binop(Eq, e1, e2, l)  -> check e1 (type_expr e2 tenv) tenv (Some l); TBool
    | Binop(And, e1, e2, l) | Binop(Or, e1, e2, l)  -> check e1 TBool tenv (Some l); check e2 TBool tenv (Some l); TBool
    | Binop(Streq, e1, e2, l) 
    | Binop(Strneq, e1, e2, l) -> let t = type_expr e2 tenv in check e1 t tenv (Some l); TBool
    (* Unop *)
    | Unop(Opp, e, l)       -> check e TInt tenv (Some l); TInt
    | Unop(Not, e, l)       -> check e TBool tenv (Some l); TBool
    | Array(a, l)           -> (if List.length a = 0 then (TArray (None, 0)) (* Tableau vide*)
                            else let t0 = type_expr (List.hd a) tenv in 

                            (* Vérifie que tous les éléments sont du même type *)
                            List.iter (fun e -> check e t0 tenv (Some l)) a;
                            TArray (Some t0, List.length a) )
    | Get(m)             -> (try type_mem_access m tenv with Final (t, _) -> t) (* accès possible avec final *)
    | This(l)            -> (try let t, v = Env.find "this" tenv in t 
                            with Not_found -> error ("Line "^(string_of_int l.pos_lnum)^": Can't use keyword 'this' outside objects"))
    | Super(l)           -> let t, v = try Env.find "this" tenv 
                                      with Not_found -> error ("Line "^(string_of_int l.pos_lnum)^": Can't use keyword 'super' outside objects")  in

                            (* Recherche classe mère de this *)
                            (match t with
                            | TClass s -> let c =  Env.find s cls in
                                          (match c.parent with
                                          | None -> error ("Line "^(string_of_int l.pos_lnum)^": No superclass for object of type " ^ s)
                                          | Some p -> TClass p)
                            | _ -> (error "Not a class") )

    | NewCstr(i, lp, line)     -> let o = Env.find i cls in
                            (* Récupère le constructeur de la classe *)
                            let constr =  try List.find (fun a -> a.method_name = "constructor") o.methods
                                          with Not_found -> error ("Line "^(string_of_int line.pos_lnum)^": No constructor for class " ^ i)
                            in
                            (* On vérifie que les paramètres donnés correspondent à ceux attendus par le constructeur *)
                            let rec check_param l p =
                              match l, p with
                              | e :: ll, (s, t, _, _) :: pp -> check e t tenv (Some line); check_param ll pp
                              | [], [] -> ()
                              | _, _ -> error ("Line "^(string_of_int line.pos_lnum)^": Expected " ^ (string_of_int (List.length constr.params)) 
                                              ^ " parameters, got " ^ (string_of_int (List.length lp)) )
                            in check_param lp constr.params;
                            if constr.return = TVoid then TClass i else (type_error_line constr.return TVoid line)

    | MethCall(e1, i, lp, line) -> let o = match type_expr e1 tenv with
                            | TClass c -> c
                            | _        -> assert false
                            in

                            (* Récupère la classe de l'objet *)
                            let o = Env.find o cls in
                            (* Récupère la classe de l'objet courant ou Void si on est dans le main *)
                            let (typ_class, _) = try Env.find "this" tenv with Not_found -> (TVoid, DEFAULT) in
                            let this_cls = (match typ_class with TClass nom -> nom | TVoid -> "main" | _ -> "") in

                            (* Recherche de la méthode dans la classe de l'objet ou les classes mères *)
                            let rec get_method obj sub =
                              (try (List.find (fun m -> m.method_name = i) obj.methods, sub)
                              with Not_found -> (match obj.parent with
                                                  | None -> error ("Line "^(string_of_int line.pos_lnum)^": Method '" ^ i ^ "' not found")
                                                  | Some p -> get_method (Env.find p cls) true
                                                ))
                            in let mtd', sub = get_method o false

                            (* Vérification de la visibilité *)
                            in let mtd = if mtd'.visible = DEFAULT then mtd'
                                        else if mtd'.visible = PROTECTED && this_cls <> "main" then mtd'
                                        else if mtd'.visible = PRIVATE && this_cls = o.class_name && not sub then mtd'
                                        else error ("Line "^(string_of_int line.pos_lnum)^": Can't access method " ^ i)
                            in

                            (* Vérification des paramètres de la méthode *)
                            let rec check_param l p =
                              match l, p with
                              | e :: ll, (s, t, _, _) :: pp -> check e t tenv (Some line); check_param ll pp
                              | [], [] -> ()
                              | _, _ -> error ("Line "^(string_of_int line.pos_lnum)^": Expected " ^ (string_of_int (List.length mtd.params)) 
                                              ^ " parameters, got " ^ (string_of_int (List.length lp)) )
                            in
                            check_param lp mtd.params;
                            mtd.return (* Rend le type de retour *)

    | InstanceOf (e, s, l) -> let () = match type_expr e tenv with
                                    | TClass _ -> () (* Vérifie que l'expression est un objet *)
                                    | _        -> error ("Line "^(string_of_int l.pos_lnum)^": Invalid expression for operator instanceof") 
                          in 
                          (* On regarde si la chaîne de caractères correspond à une classe existante *)
                          (try let _ = Env.find s cls in TBool 
                            with Not_found -> error ("Line "^(string_of_int l.pos_lnum)^": "^s^ " is not a valid class name") )

    | Cast (t, e, l)       -> let m = match t with Get m -> m | _ -> error ("Line "^(string_of_int l.pos_lnum)^": Invalid cast Class name")
                          in
                          (* Vérifie que la classe de destination existe *)
                          let c = (match m with
                                  | Var (c, l) -> let v = (try Env.find c cls
                                      with Not_found -> error ("Line "^(string_of_int l.pos_lnum)^": "^c^" is not a valid class name")) in v
                                  | _          -> error ("Line "^(string_of_int l.pos_lnum)^": Invalid cast"))
                          in
                          (* Récupère classe de l'expression *)
                          let t = (match type_expr e tenv with
                                  | TClass t -> t
                                  | t ->error ("Line "^(string_of_int l.pos_lnum)^": Can't cast from "
                                  ^c.class_name^" to "^(typ_to_string t)))
                          in
                          let rec is_parent p = (* Recherche dans les classes parentes *)
                            match p.parent with
                            | None -> false
                            | Some n -> if c.class_name = n then true else is_parent (Env.find n cls)
                          in let rec is_child p' = (* Recherche dans les classes filles*)
                            let r = List.filter (fun a -> a.parent = (Some p'.class_name)) p.classes
                            in match r with
                            | [] -> false
                            | l -> List.fold_left (fun acc o -> if o.class_name = c.class_name then true else acc || is_child o) false l 
                          in
                          if t = c.class_name then (TClass t) (* Si même classe *)
                          else if is_parent (Env.find t cls) || is_child (Env.find t cls) then (TClass c.class_name)
                          else error ("Line "^(string_of_int l.pos_lnum)^": Can't cast from "^c.class_name^" to "^t)

  and type_mem_access m tenv = match m with
    | Var (v, l)       -> (try let t, _ = Env.find v tenv in t 
                      with Not_found -> 
                        (* Si ce n'est pas une variable valide on regarde si ça peut être un nom de classe *)
                        (try let _ = Env.find v cls in TStatic v (* Si on trouve on renvoie accès statique *)
                      with Not_found -> (* Sinon erreur *)
                        (error ("Line "^(string_of_int l.pos_lnum)^": Unknown identifiant '" ^ v 
                        ^ "', did you mean '" ^ Wordcomp.get_closest_word_map v tenv ^ "' ?"))))
    | Field(e, s, l) -> let cls_name, static = match type_expr e tenv with
                                    | TClass c -> (c, false)
                                    | TStatic c -> (c, true)
                                    | typ_err  -> type_error_line typ_err (TClass "class") l
                    in
                    (* Récupère le type de l'objet courant *)
                    let (typ_class, _) = try Env.find "this" tenv with Not_found -> (TVoid, DEFAULT) in
                    let this_cls = (match typ_class with TClass nom -> nom | TVoid -> "main" | _ -> "") in

                    (* On récupère l'attribut *)
                    let ((x, t, v, _), p) = 
                    (* Si variable non statique *)
                    (if not static then
                      let o = Env.find cls_name cls
                    in
                    (* Recherche de l'attribut *)
                    let rec get_att obj =
                      try let t = List.find (fun (x, t, v, _) -> x = s) obj.attributes in (t, obj)
                      with Not_found -> match obj.parent with
                                        | None -> error ("Line "^(string_of_int l.pos_lnum)^": Unknown attribute '" ^ s ^ "' for class " ^ cls_name
                                                  ^ ", did you mean '" ^ Wordcomp.get_closest_word_attributes s obj.attributes ^ "' ?")
                                        | Some p -> get_att (Env.find p cls)
                    in
                    let t, obj = get_att o in (t, obj.class_name<>this_cls)
                    
                    else (* Si variable statique *)
                      let o = Env.find cls_name cls in
                      let t = List.find (fun (x, t, v, _) -> x = s) o.statics in
                      let rec is_parent p =
                            match p.parent with
                            | None -> false
                            | Some n -> if cls_name = n then true else is_parent (Env.find n cls)
                      in (t, is_parent o) ) 
                    in  
                    
                    (* Vérification visibilité *)
                    if v = DEFAULT || (v = PROTECTED && this_cls <> "main") || (v = PRIVATE && this_cls = cls_name && not p) then t
                    else if (v = FDEFAULT || (v = FPROTECTED && this_cls <> "main") (* Si Final on lève une exception Final *)
                        || (v = FPRIVATE && this_cls = cls_name && not p)) then (if not static then raise (Final (t, p)) 
                        else raise (Final (t, true)) )
                    else error ("Line "^(string_of_int l.pos_lnum)^": Can't access attribute " ^ s)
    | ArrayId(e, i, l) -> check i TInt tenv (Some l);
                      match type_expr e tenv with
                      | TArray (Some t, _) -> t
                      | t               -> type_error_line t (TArray (None, 0)) l
  in

  let rec check_instr mname i ret tenv = match i with
    | Print (e) -> let _ = type_expr e tenv in () (* Accepte tous les types *)
    | If(e, i1, i2, l) -> check e TBool tenv (Some l); check_seq mname i1 ret tenv; check_seq mname i2 ret tenv
    | While(e, s, l)   -> check e TBool tenv (Some l); check_seq mname s ret tenv
    | Expr(e, l)       -> check e TVoid tenv (Some l)
    | Return(e, l)     -> check e ret tenv (Some l)
    | Set(m, e, l) -> let t = (try type_mem_access m tenv
                            with Final (t, b) -> (* Si Final on regarde si on est dans le constructeur de la classe sinon erreur *)
                              (if mname = "constructor" && not b then t 
                              else error ("Line "^(string_of_int l.pos_lnum)^": can't modify final attribute "))) in
                      let rec match_type tp typ_e =
                      match typ_e with
                      | TClass c -> if (TClass c) = tp then ()
                                    else
                                      let c' = Env.find c cls in
                                      (match (c'.parent) with
                                      | None -> (type_error_line (TClass c) tp l)
                                      | Some p' -> match_type tp (TClass p') )
                      | wrong_t -> type_error_line wrong_t tp l
                      in
                      (match t with
                      | TClass c | TStatic c -> match_type t (type_expr e tenv)
                      | TArray (Some t, len) -> if len <= 0 then error ("Line "^(string_of_int l.pos_lnum)^": Invalid array size")
                                                else (
                                                match type_expr e tenv with
                                                | TArray (Some t, len') ->
                                                  if len' <> len then (* Le tableau donné doit avoir la taille de celui de la variable *)
                                                    error ("Expected array of length " ^ (string_of_int len) 
                                                  ^ ", got array of length " ^ (string_of_int len'))
                                                | TArray (None, 0) -> error ("Expected array of length " ^ (string_of_int len) 
                                                                      ^ ", got array of length 0")
                                                | t' -> type_error t' (TArray (Some t, len)) )
                      | TArray (None, _)          -> ()
                      | TInt 		-> check e TInt tenv (Some l)
                      | TVoid    -> check e TVoid tenv (Some l)
                      | TBool    -> check e TBool tenv (Some l))
    | SetDef(m, e, l) -> let t = (try type_mem_access m tenv
                              with Final (t, b) -> t ) in (* Final ne pose pas de problème en définition directe *)
                            let rec match_type tp typ_e =
                            match typ_e with
                            | TClass c -> if (TClass c) = tp then ()
                                      else
                                        let c' = Env.find c cls in
                                        (match (c'.parent) with
                                        | None -> (type_error_line (TClass c) tp l)
                                        | Some p' -> match_type tp (TClass p') )
                            | wrong_t -> type_error_line wrong_t tp l
                            in
                            (match t with
                            | TClass c | TStatic c -> match_type t (type_expr e tenv)
                            | TArray (Some t, len) -> (match type_expr e tenv with
                                                    | TArray (Some t, len') -> 
                                                      if len' <> len then 
                                                        error ("Expected array of length " ^ (string_of_int len) 
                                                      ^ ", got array of length " ^ (string_of_int len'))
                                                    | TArray (None, 0) -> ()
                                                    | t' -> type_error t' (TArray (Some t, len)))
                            | TArray (None, _)          -> ()
                            | TInt 		-> check e TInt tenv (Some l)
                            | TVoid    -> check e TVoid tenv (Some l)
                            | TBool    -> check e TBool tenv (Some l))
  and check_seq name s ret tenv =
    List.iter (fun i -> check_instr name i ret tenv) s
  in

  let rec check_mdef m tenv =
    let menv = add_env m.locals (add_env m.params tenv) in
    check_seq m.method_name m.code m.return menv
  in

  let rec check_class c tenv =
    let cenv = add_env [("this", TClass c.class_name, DEFAULT, None)] (add_env c.attributes tenv) in
    List.iter (fun m -> check_mdef m cenv) c.methods;
    check_seq "" c.init_decl TVoid cenv;  (* Check initialisations attributs *)
    check_seq "" c.static_decl TVoid cenv (* Check initialisations statiques *)
  in
  List.iter (fun c -> check_class c tenv) p.classes;
  check_seq "main" p.main TVoid tenv
