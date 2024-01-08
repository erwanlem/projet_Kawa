open Kawa

exception Error of string
let error s = raise (Error s)
let type_error ty_actual ty_expected =
  error (Printf.sprintf "expected %s, got %s"
           (typ_to_string ty_expected) (typ_to_string ty_actual))

module Env = Map.Make(String)
type tenv = typ Env.t

let add_env l tenv =
  List.fold_left (fun env (x, t) -> Env.add x t env) tenv l

let typecheck_prog p =
  let tenv = add_env p.globals Env.empty in
  let cls = List.fold_left (fun env o -> Env.add o.class_name o env) Env.empty p.classes in

  let rec check e typ tenv =
    let typ_e = type_expr e tenv in
    if typ_e <> typ then type_error typ_e typ

  and type_expr e tenv = match e with
    | Int i  -> TInt
    | Bool _ -> TBool
    | New(i) -> TClass i
    (* Binop INT *)
    | Binop(Add, e1, e2) | Binop(Sub, e1, e2) | Binop(Mul, e1, e2) 
    | Binop(Div, e1, e2) | Binop(Rem, e1, e2) -> check e1 TInt tenv; check e2 TInt tenv; TInt
    (* Binop BOOL *)
    | Binop(Lt , e1, e2) | Binop(Le , e1, e2) 
    | Binop(Gt , e1, e2) | Binop(Ge , e1, e2) -> check e1 TInt tenv; check e2 TInt tenv; TBool
    | Binop(Neq, e1, e2) | Binop(Eq, e1, e2)  -> check e1 (type_expr e2 tenv) tenv; TBool
    | Binop(And, e1, e2) | Binop(Or, e1, e2)  -> check e1 TBool tenv; check e2 TBool tenv; TBool
    (* Unop *)
    | Unop(Opp, e)       -> check e TInt tenv; TInt
    | Unop(Not, e)       -> check e TBool tenv; TBool

    | Get(m)             -> type_mem_access m tenv
    | This               -> Env.find "this" tenv

    | NewCstr(i, lp)     -> let o = Env.find i cls in
                            let constr =  try List.find (fun a -> a.method_name = "constructor") o.methods
                                          with Not_found -> error ("No constructor for class " ^ i)
                            in
                            let rec check_param l p =
                              match l, p with
                              | e :: ll, (s, t) :: pp -> check e t tenv; check_param ll pp
                              | [], [] -> ()
                              | _, _ -> error ("Expected " ^ (string_of_int (List.length constr.params)) 
                                              ^ " parameters, got " ^ (string_of_int (List.length lp)) )
                            in check_param lp constr.params;
                            if constr.return = TVoid then TClass i else (type_error constr.return TVoid)

    | MethCall(e1, i, lp) -> let o = match type_expr e1 tenv with
                            | TClass c -> c
                            | _        -> error "Not a class"
                            in
                            let o = Env.find o cls in
                            let rec get_method obj = 
                              (try List.find (fun m -> m.method_name = i) obj.methods
                              with Not_found -> (match obj.parent with
                                                  | None -> error ("Method " ^ i ^ " not found")
                                                  | Some p -> get_method (Env.find p cls)
                                                ))
                            in let mtd = get_method o in
                            let rec check_param l p =
                              match l, p with
                              | e :: ll, (s, t) :: pp -> check e t tenv; check_param ll pp
                              | [], [] -> ()
                              | _, _ -> error ("Expected " ^ (string_of_int (List.length mtd.params)) 
                                              ^ " parameters, got " ^ (string_of_int (List.length lp)) )
                            in
                            check_param lp mtd.params;
                            mtd.return
                            
  and type_mem_access m tenv = match m with
    | Var v       -> Env.find v tenv
    | Field(e, s) -> let cls_name = match type_expr e tenv with
                                    | TClass c -> c
                                    | typ_err  -> type_error typ_err (TClass "class")
                    in let o = Env.find cls_name cls
                    in
                    let rec get_att obj =
                      try let t = List.find (fun (x, t) -> x = s) obj.attributes in (snd t)
                      with Not_found -> match obj.parent with
                                        | None -> error ("Unknown attribute " ^ s ^ " for class " ^ cls_name)
                                        | Some p -> get_att (Env.find p cls)
                    in get_att o
  in

  let rec check_instr i ret tenv = match i with
    | Print e -> () (* Accepte tous les types *)
    | If(e, i1, i2) -> check e TBool tenv; check_seq i1 ret tenv; check_seq i2 ret tenv
    | While(e, s)   -> check e TBool tenv; check_seq s ret tenv
    | Expr(e)       -> check e TVoid tenv
    | Return(e)     -> check e ret tenv
    | Set(m, e) ->  let t = type_mem_access m tenv in
												let rec match_type tp typ_e =
                        match typ_e with
                        | TClass c -> if (TClass c) = tp then () 
                                      else
                                        let c' = Env.find c cls in
                                        (match (c'.parent) with
                                        | None -> (type_error (TClass c) tp)
                                        | Some p' -> match_type tp (TClass p') )
                        | wrong_t -> type_error wrong_t tp
                       in
											 (match t with
											 | TClass c -> match_type t (type_expr e tenv)
											 | TInt 		-> check e TInt tenv
											 | TVoid    -> check e TVoid tenv
											 | TBool    -> check e TBool tenv)
  and check_seq s ret tenv =
    List.iter (fun i -> check_instr i ret tenv) s
  in

  let rec check_mdef m tenv =
    let menv = add_env m.locals (add_env m.params tenv) in
    check_seq m.code m.return menv
  in

  let rec check_class c tenv =
    let cenv = add_env [("this", TClass c.class_name)] (add_env c.attributes tenv) in
    List.iter (fun m -> check_mdef m cenv) c.methods
  in

  List.iter (fun c -> check_class c tenv) p.classes;
  check_seq p.main TVoid tenv
