{

  open Lexing
  open Kawaparser

  exception Error of string

  let keyword_or_ident =
  let h = Hashtbl.create 17 in
  List.iter (fun (s, k) -> Hashtbl.add h s k)
    [ "print",      PRINT;
      "main",       MAIN;
      "true",       BOOL(true);
      "false",      BOOL(false);
      "new",        NEW;
      "if",         IF;
      "else",       ELSE;
      "while",      WHILE;
      "bool",       TBOOL;
      "int",        TINT;
      "void",       TVOID;
      "method",     METHOD;
      "var",        VAR;
      "attribute",  ATTRIBUTE;
      "class",      CLASS;
      "this",       THIS;
      "return",     RETURN;
      "extends",    EXTENDS;
      "private",    PRIVATE;
      "protected",  PROTECTED;
      "final",      FINAL;
      "instanceof", INSTANCEOF;
      "super",      SUPER;
      "static",     STATIC;
      "array",      ARRAY;
    ] ;
  fun s ->
    try  Hashtbl.find h s
    with Not_found -> IDENT(s)
        
}

let digit = ['0'-'9']
let number = ['-']? digit+
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' '_'] (alpha | '_' | digit)*
  
rule token = parse
  | ['\n']            { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+  { token lexbuf }

  | "//" [^ '\n']* "\n"  { new_line lexbuf; token lexbuf }
  | "/*"                 { comment lexbuf; token lexbuf }

  | number as n  { INT(int_of_string n) }
  | ident as id  { keyword_or_ident id }

  | "+"  { PLUS }
  | "-"  { SUB }
  | "*"  { STAR }
  | "/"  { DIV }
  | "%"  { REM }
  | ";"  { SEMI }
  | ","  { COMMA }
  | "."  { DOT }
  | "("  { LPAR }
  | ")"  { RPAR }
  | "{"  { BEGIN }
  | "}"  { END }
  | "["  { LBRA }
  | "]"  { RBRA }
  | "==" { EQ }
  | "="  { EQUAL }
  | "<=" { LE }
  | "<"  { LT }
  | ">=" { GE }
  | ">"  { GT }
  | "!=" { NEQ }
  | "&&" { AND }
  | "||" { OR }
  | "!"  { NOT }
  | "==="{ STREQ }
  | "=/="{ STRNEQ }
  

  | _    { raise (Error ("unknown character : " ^ lexeme lexbuf)) }
  | eof  { EOF }

and comment = parse
  | "*/" { () }
  | _    { comment lexbuf }
  | eof  { raise (Error "unterminated comment") }
