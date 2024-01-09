(**
   Kawa : un petit langage à objets inspiré de Java
 *)

exception MissingSemi
exception MissingCurlyBracket


(* Types déclarés pour les attributs, pour les variables, et pour les 
   paramètres et résultats des méthodes. *)
type typ =
  | TVoid
  | TInt
  | TBool
  | TArray of typ option * int (* Type * taille *)
  | TClass of string
  | TStatic of string (* Type classe statique *)

(*
  Visibilité des attributs 
*)
type visibility =
  | DEFAULT
  | PROTECTED
  | PRIVATE
  | FDEFAULT (* FINAL DEFAULT *)
  | FPRIVATE (* FINAL PRIVATE *)
  | FPROTECTED (* FINAL PROTECTED *)
  | UNDEFINED (* Pour les variables globales *)

let rec typ_to_string = function
  | TVoid    -> "void"
  | TInt     -> "int"
  | TBool    -> "bool"
  | TArray (None, _) -> "Empty array"
  | TArray (Some t, _) -> (typ_to_string t)^" array"
  | TClass c -> c
  | TStatic c -> c

type unop  = Opp | Not
type binop = Add | Sub | Mul | Div | Rem
           | Lt  | Le  | Gt | Ge | Eq  | Neq
           | And | Or  | Streq   | Strneq

(* Expressions *)
type expr =
  (* Base arithmétique *)
  | Int    of int
  | Bool   of bool
  | Array  of expr list * Lexing.position
  | Unop   of unop * expr * Lexing.position
  | Binop  of binop * expr * expr * Lexing.position
  (* Accès à une variable ou un attribut *)
  | Get    of mem_access
  (* Objet courant *)
  | This   of Lexing.position
  (* Objet classe mère *)
  | Super  of Lexing.position
  (* Création d'un nouvel objet *)
  | New      of string
  | NewCstr  of string * expr list * Lexing.position
  (* Appel de méthode *)
  | MethCall   of expr * string * expr list * Lexing.position

  | InstanceOf of expr * string * Lexing.position
  | Cast       of expr * expr * Lexing.position

(* Accès mémoire : variable ou attribut d'un objet *)
and mem_access =
  | Var   of string * Lexing.position
  | Field of expr (* objet *) * string (* nom d'un attribut *) * Lexing.position
  | ArrayId of expr (* nom var/attribut *) * expr (* indice *) * Lexing.position

(* Instructions *)
type instr =
  (* Affichage d'un entier *)
  | Print  of expr
  (* Écriture dans une variable ou un attribut *)
  | Set    of mem_access * expr * Lexing.position
  | SetDef of mem_access * expr * Lexing.position
  (* Structures de contrôle usuelles *)
  | If     of expr * seq * seq * Lexing.position
  | While  of expr * seq * Lexing.position
  (* Fin d'une fonction *)
  | Return of expr * Lexing.position
  (* Expression utilisée comme instruction *)
  | Expr   of expr * Lexing.position

and seq = instr list

(* Définition de méthode 

   Syntaxe : method <type de retour> <nom> (<params>) { ... }

   Le corps de la méthode est similaire au corps d'une fonction. *)
type method_def = {
    method_name: string;
    visible: visibility;
    code: seq;
    params: (string * typ * visibility * expr option) list;
    locals: (string * typ * visibility * expr option) list;
    return: typ;
  }
      

(*
  Définition globale -> peut être une méthode / un attribut / un attribut statique
*)
type global_def =
| MethodDef    of method_def
| AttributeDef of (string * typ * visibility * expr option)
| StaticDef    of (string * typ * visibility * expr option)

(* Définition de classe 

   Syntaxe : class <nom de la classe> { ... }
        ou : class <nom de la classe> extends <nom de la classe mère> { ... }

   On considère que toute classe C contient une définition de méthode de nom
   "constructor" et de type de retour void, qui initialise les champs du 
   paramètre implicite this. *)

type class_def = {
  class_name: string;
  attributes: (string * typ * visibility * expr option) list;
  methods: method_def list;
  statics: (string * typ * visibility * expr option) list;
  init_decl: seq;
  static_decl: seq;
  parent: string option;
}


(* Programme complet : variables globales, classes, et une séquence 
   d'instructions *)
type program = {
    classes: class_def list;
    globals: (string * typ * visibility * expr option) list;
    main: seq;
  }
