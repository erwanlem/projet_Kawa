%{

  open Lexing
  open Kawa

%}


%token <int> INT
%token <bool> BOOL
%token <string> IDENT
%token MAIN
%token LPAR RPAR BEGIN END SEMI COMMA DOT
%token PLUS STAR SUB DIV REM LT LE GT GE EQ NEQ
%token STREQ STRNEQ
%token AND OR
%token NOT
%token EQUAL
%token IF ELSE
%token WHILE
%token RETURN
%token NEW THIS SUPER
%token TBOOL TINT TVOID
%token METHOD VAR ATTRIBUTE CLASS EXTENDS
%token PRINT
%token INSTANCEOF
%token PRIVATE PROTECTED FINAL STATIC
%token EOF
%token RBRA LBRA ARRAY

%nonassoc INSTANCEOF
%left OR
%left AND
%right NOT
%nonassoc LT LE GT GE EQ NEQ STREQ STRNEQ
%left PLUS SUB
%left STAR DIV REM
%nonassoc DOT
%nonassoc LBRA
%nonassoc RPAR

%start program
%type <Kawa.program> program

%%

program:
| decl=list(var_decl) cls=list(class_def) MAIN BEGIN m=list(instruction) END EOF
    { let glob = List.fold_left (fun acc v -> v @ acc) [] decl in
        {classes=cls; globals=glob; main=(List.fold_left (fun acc v -> match v with 
                                                                    | (x, _, _, Some e) -> Set(Var (x, $startpos), e, $startpos) :: acc 
                                                                    | _                 -> acc) m glob) } }
;

typ:
| TBOOL { TBool }
| TINT  { TInt }
| TVOID { TVoid }
| i=IDENT { TClass(i) }
| ARRAY LBRA t=typ RBRA LPAR len=INT RPAR { TArray(Some t, len) }
;

class_def:
| CLASS id=IDENT BEGIN g=list(global_def) END
    { let g = List.fold_left (fun acc l -> l@acc) [] g in
      let attri  = List.fold_left (fun l a -> match a with AttributeDef att -> att::l | _ -> l) [] g in
      let static = List.fold_left (fun l e -> match e with StaticDef s -> s::l | _ -> l) [] g in
      let decl_a = List.fold_left (fun l e -> match e with 
                                            | AttributeDef (x, _, _, Some e) -> SetDef (Var (x, $startpos), e, $startpos) :: l
                                            | _                              -> l) [] g in 
      let decl_s = List.fold_left (fun l e -> match e with 
                                            | StaticDef    (x, _, _, Some e) -> SetDef (Field ( Get (Var (id, $startpos)), x, $startpos), e, $startpos) :: l
                                            | _                              -> l) [] g in 
        { class_name=id;
        attributes=attri;
        methods=List.fold_left (fun l e -> match e with MethodDef m -> m::l | _ -> l) [] g;
        statics=static;
        init_decl=decl_a;
        static_decl=decl_s;
        parent=None } }
| CLASS id=IDENT EXTENDS idext=IDENT BEGIN g=list(global_def) END
    { let g = List.fold_left (fun acc l -> l@acc) [] g in
      let attri  = List.fold_left (fun l a -> match a with AttributeDef att -> att::l | _ -> l) [] g in
      let static = List.fold_left (fun l e -> match e with StaticDef s -> s::l | _ -> l) [] g in
      let decl_a = List.fold_left (fun l e -> match e with 
                                            | AttributeDef (x, _, _, Some e) -> Set (Var (x, $startpos), e, $startpos) :: l
                                            | _                              -> l) [] g in 
      let decl_s = List.fold_left (fun l e -> match e with 
                                            | StaticDef    (x, _, _, Some e) -> Set (Var ((id ^ "." ^ x), $startpos), e, $startpos) :: l
                                            | _                              -> l) [] g in 
        { class_name=id;
        attributes=attri;
        methods=List.fold_left (fun l e -> match e with MethodDef m -> m::l | _ -> l) [] g;
        statics=static;
        init_decl=decl_a;
        static_decl=decl_s;
        parent=Some idext } }
| CLASS id=IDENT BEGIN g=list(global_def) error { raise MissingCurlyBracket }
| CLASS id=IDENT error                          { raise MissingCurlyBracket }
| CLASS id=IDENT EXTENDS idext=IDENT error      { raise MissingCurlyBracket }
| CLASS id=IDENT EXTENDS idext=IDENT BEGIN g=list(global_def) error { raise MissingCurlyBracket }

;

var_decl:
| VAR t=typ l=separated_list(COMMA, IDENT) SEMI { List.fold_left (fun acc id -> (id, t, UNDEFINED, None)::acc) [] l }
| VAR t=typ id=IDENT EQUAL e=expression SEMI { [(id, t, UNDEFINED, Some e)] }
;

attr_visib:
| ATTRIBUTE                 {DEFAULT}
| PRIVATE ATTRIBUTE         {PRIVATE}
| PROTECTED ATTRIBUTE       {PROTECTED}
| FINAL ATTRIBUTE           {FDEFAULT}
| PRIVATE FINAL ATTRIBUTE   {FPRIVATE}
| PROTECTED FINAL ATTRIBUTE {FPROTECTED}
;

static_def:
| STATIC ATTRIBUTE                 {DEFAULT}
| PRIVATE STATIC ATTRIBUTE         {PRIVATE}
| PROTECTED STATIC ATTRIBUTE       {PROTECTED}
| STATIC FINAL ATTRIBUTE           {FDEFAULT}
| PRIVATE STATIC FINAL ATTRIBUTE   {FPRIVATE}
| PROTECTED STATIC FINAL ATTRIBUTE {FPROTECTED}

meth_visib:
| METHOD           {DEFAULT}
| PRIVATE METHOD   {PRIVATE}
| PROTECTED METHOD {PROTECTED}
;

global_def:
| visib=attr_visib t=typ l=separated_list(COMMA, IDENT) SEMI { List.fold_left (fun acc id -> (AttributeDef (id, t, visib, None))::acc) [] l }
| visib=attr_visib t=typ separated_list(COMMA, IDENT) error { raise MissingSemi }

| visib=attr_visib t=typ id=IDENT EQUAL e=expression SEMI { [AttributeDef (id, t, visib, Some e)] }
| visib=attr_visib t=typ id=IDENT EQUAL e=expression error { raise MissingSemi }

| stat=static_def  t=typ l=separated_list(COMMA, IDENT) SEMI { List.fold_left (fun acc id -> (StaticDef (id, t, stat, None))::acc) [] l }
| stat=static_def  t=typ separated_list(COMMA, IDENT) error { raise MissingSemi }

| stat=static_def  t=typ id=IDENT EQUAL e=expression SEMI { [StaticDef (id, t, stat, Some e)] }
| stat=static_def  t=typ id=IDENT EQUAL e=expression error { raise MissingSemi }

| visib=meth_visib t=typ name=IDENT LPAR l=separated_list(COMMA, param) RPAR BEGIN decl=list(var_decl) instr=list(instruction) END 
    { let loc = List.fold_left (fun acc v -> v @ acc) [] decl in
      [MethodDef {method_name=name; visible=visib; code=(List.fold_left (fun acc v -> match v with 
                                                                    | (x, _, _, Some e) -> Set(Var (x, $startpos), e, $startpos) :: acc 
                                                                    | (_, _, _, _)      -> acc) instr loc); params=l; locals=loc; return=t}] }
;

param:
| t=typ id=IDENT { (id, t, UNDEFINED, None) }
;

instruction:
| PRINT LPAR e=expression RPAR SEMI { Print(e) }
| PRINT LPAR e=expression RPAR error { raise MissingSemi }

| m=mem EQUAL e=expression SEMI           { Set(m, e, $startpos) }
| m=mem EQUAL e=expression error          { raise MissingSemi }

| IF LPAR e=expression RPAR BEGIN i1=list(instruction) END {If(e, i1, [], $startpos)}
| IF LPAR e=expression RPAR BEGIN i1=list(instruction) error { raise MissingCurlyBracket }
| IF LPAR e=expression RPAR error                            { raise MissingCurlyBracket }

| IF LPAR e=expression RPAR BEGIN i1=list(instruction) END ELSE BEGIN i2=list(instruction) END   {If(e, i1, i2, $startpos)}
| IF LPAR e=expression RPAR BEGIN i1=list(instruction) END ELSE BEGIN i2=list(instruction) error { raise MissingCurlyBracket }
| IF LPAR e=expression RPAR BEGIN i1=list(instruction) END ELSE error                            { raise MissingCurlyBracket }


| WHILE LPAR e=expression RPAR BEGIN i=list(instruction) END { While(e, i, $startpos) }
| WHILE LPAR e=expression RPAR error                           { raise MissingCurlyBracket }
| WHILE LPAR e=expression RPAR BEGIN i=list(instruction) error { raise MissingCurlyBracket }

| RETURN e=expression SEMI  { Return(e, $startpos) }
| RETURN e=expression error  { raise MissingSemi }

| e=expression SEMI         { Expr(e, $startpos) }
| e=expression error        { raise MissingSemi }
;

expression:
| n=INT  { Int(n) }
| b=BOOL { Bool(b) }
| LBRA a=separated_list(COMMA, expression) RBRA { Array(a, $startpos) }
| THIS   { This ($startpos) }
| SUPER  { Super ($startpos) }
| NEW i=IDENT { New(i) }
| NEW i=IDENT LPAR l=separated_list(COMMA, expression) RPAR {NewCstr(i, l, $startpos)}
| LPAR e=expression RPAR { e }
| m=mem   { Get(m) }
| e1=expression b=bop e2=expression { Binop(b, e1, e2, $startpos) }
| u=uop e=expression           { Unop(u, e, $startpos) }
| e=expression INSTANCEOF s=IDENT { InstanceOf(e, s, $startpos) }
| e1=expression DOT i=IDENT LPAR l=separated_list(COMMA, expression) RPAR { MethCall(e1, i, l, $startpos) }
| LPAR t=expression RPAR e=expression  { Cast(t, e, $startpos) }
;

mem:
| i=IDENT { Var(i, $startpos) }
| e=expression DOT i=IDENT { Field(e, i, $startpos) }
| e=expression LBRA i=expression RBRA { ArrayId(e, i, $startpos) }
;


%inline uop:
| SUB { Opp }
| NOT { Not }
;

%inline bop:
| PLUS { Add }
| STAR { Mul }
| SUB  { Sub }
| DIV  { Div }
| REM  { Rem }
| LT   { Lt }
| LE   { Le }
| GT   { Gt }
| GE   { Ge }
| EQ   { Eq }
| NEQ  { Neq }
| AND  { And }
| OR   { Or }
| STREQ  { Streq }
| STRNEQ { Strneq }
;