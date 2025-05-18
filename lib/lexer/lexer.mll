{
  open Parser
}

  (* | ['a'-'z''A'-'Z'](['a'-'z' 'A'-'Z' '0'-'9' '_']*)(*) as id { IDENT id } *)

rule tokenize = parse
  | [' ' '\t' '\n']       { tokenize lexbuf }    (* Ignorer les espaces *)
  | "int"                 { INT }
  | "float"               { FLOAT }
  | "double"              { DOUBLE }
  | "void"                { VOID }
  | "return"              { RETURN }
  | "for"                 { FOR }
  | "if"                  { IF }
  (* | "else"                { ELSE }
  | "while"               { WHILE } *)
  | "malloc"              { MALLOC }
  | ['a'-'z''A'-'Z']+ as id { IDENT id }
  | ['0'-'9']+ as num     { NUM (int_of_string num) }
  | ['0'-'9']+ "." ['0'-'9']* as fnum { FLOAT_NUM (float_of_string fnum) }
  | "++"                  { PP }
  | "--"                  { MM }
  | "="                   { ASSIGN }
  | "+"                   { PLUS }
  | "-"                   { MINUS }
  | "*"                   { STAR }
  | "/"                   { SLASH }
  | "%"                   { MOD }
  | ";"                   { SEMICOLON }
  | ","                   { COMMA }
  | "["                   { LBRACKET }
  | "]"                   { RBRACKET }
  | "("                   { LPAREN }
  | ")"                   { RPAREN }
  | "{"                   { LBRACE }
  | "}"                   { RBRACE }
  | ">"                   { GT }
  | ">="                  { GE }
  | "<"                   { LT }
  | "<="                  { LE }
  | "=="                  { EQ }
  | "!="                  { NE }
  | eof                   { EOF }
  | _                     { failwith "Unknown character" }
