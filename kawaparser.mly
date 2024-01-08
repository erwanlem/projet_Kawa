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
%token AND OR
%token NOT
%token EQUAL
%token IF ELSE
%token WHILE
%token RETURN
%token NEW THIS
%token TBOOL TINT TVOID
%token METHOD VAR ATTRIBUTE CLASS EXTENDS
%token PRINT
%token EOF

%left OR
%left AND
%right NOT
%nonassoc LT LE GT GE EQ NEQ
%left PLUS SUB
%left STAR DIV REM
%nonassoc DOT

%start program
%type <Kawa.program> program

%%

program:
| decl=list(var_decl) cls=list(class_def) MAIN BEGIN main=list(instruction) END EOF
    { {classes=cls; globals=decl; main} }
;

typ:
| TBOOL { TBool }
| TINT  { TInt }
| TVOID { TVoid }
| i=IDENT { TClass(i) }
;

class_def:
| CLASS id=IDENT BEGIN attr=list(attr_decl) mtd=list(method_def) END 
    { { class_name=id; attributes=attr; methods=mtd; parent=None } }
| CLASS id=IDENT EXTENDS idext=IDENT BEGIN attr=list(attr_decl) mtd=list(method_def) END 
    { { class_name=id; attributes=attr; methods=mtd; parent=Some idext } }
;

var_decl:
| VAR t=typ id=IDENT SEMI { (id, t) }
;

attr_decl:
| ATTRIBUTE t=typ id=IDENT SEMI { (id, t) }
;

param:
| t=typ id=IDENT { (id, t) }
;

method_def:
| METHOD t=typ name=IDENT LPAR l=separated_list(COMMA, param) RPAR BEGIN decl=list(var_decl) instr=list(instruction) END 
    { {method_name=name; code=instr; params=l; locals=decl; return=t} }
;

instruction:
| PRINT LPAR e=expression RPAR SEMI { Print(e) }
| m=mem EQUAL e=expression SEMI           { Set(m, e) }
| IF LPAR e=expression RPAR BEGIN i1=list(instruction) END {If(e, i1, [])}
| IF LPAR e=expression RPAR BEGIN i1=list(instruction) END ELSE BEGIN i2=list(instruction) END {If(e, i1, i2)}
| WHILE LPAR e=expression RPAR BEGIN i=list(instruction) END { While(e, i) }
| RETURN e=expression SEMI  { Return(e) }
| e=expression SEMI         { Expr(e) }
;

expression:
| n=INT  { Int(n) }
| b=BOOL { Bool(b) }
| THIS   { This }
| NEW i=IDENT { New(i) }
| NEW i=IDENT LPAR l=separated_list(COMMA, expression) RPAR {NewCstr(i, l)}
| LPAR e=expression RPAR { e }
| m=mem   { Get(m) }
| e1=expression b=bop e2=expression { Binop(b, e1, e2) }
| u=uop e=expression           { Unop(u, e) }
| e1=expression DOT i=IDENT LPAR l=separated_list(COMMA, expression) RPAR { MethCall(e1, i, l) }
;

mem:
| i=IDENT { Var(i) }
| e=expression DOT i=IDENT { Field(e, i) }
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
;