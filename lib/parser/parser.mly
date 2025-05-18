%{
  open Ast
%}

%token <string> IDENT
%token <int> NUM
%token <float> FLOAT_NUM
%token INT FLOAT DOUBLE VOID ASSIGN RETURN
%token FOR MALLOC
%token IF 
// ELSE WHILE
%token LT LE GT GE NE EQ
%token PLUS MINUS STAR SLASH MOD
%token PP MM 
%token SEMICOLON COMMA LBRACKET RBRACKET LPAREN RPAREN LBRACE RBRACE
%token EOF

// %left PLUS MINUS
// %left STAR SLASH

%start program
%type <Ast.program> program

%%
  
type_spec:
  | INT { IntType }
  | FLOAT { FloatType }
  | DOUBLE { DoubleType }
  | VOID { VoidType }

binope: |PLUS {Add} |MINUS {Sub} |STAR {Mul} |SLASH {Div} |MOD {Mod}

uniope: |PP {Pp} |MM {Mm}

expressions: 
  | expression COMMA expressions { $1 :: $3 }
  | expression { [$1] }

expression:
  | NUM { Num $1 }
  | FLOAT_NUM { FloatNum $1 }
  | IDENT { Var $1 }
  | expression binope expression { BinOpE ($2, $1, $3) }
  | LPAREN expression RPAREN { $2 }
  | MALLOC LPAREN expression RPAREN { Malloc $3 }
  // | IDENT LBRACKET expression RBRACKET { Array ($1, $3) }

binopc: |LT {Lt} |LE {Le} |GT {Gt} |GE {Ge} |EQ {Eq} |NE {Ne}

condition:
  |expression binopc expression {BinOpC($2, $1, $3)}

ifs:
  | IF LPAREN condition RPAREN LBRACE statements RBRACE { If ( $6 )}

loop:
  | FOR LPAREN declaration SEMICOLON condition SEMICOLON assignment RPAREN LBRACE statements RBRACE
      { ForLoop ($3, $5, $7, $10) }

assignment:
  | IDENT ASSIGN expression { Assignment ($1, $3) }
  | IDENT LBRACKET expression RBRACKET ASSIGN expression { ArrayAssign ($1, $3, $6) }
  | uniope IDENT { Assignment ($2, UniOpE($1))}
  | IDENT uniope { Assignment ($1, UniOpE($2))}

declaration:
  | type_spec IDENT { Declaration ($2, $1) }
  | type_spec IDENT ASSIGN expression { InitDeclaration ($2, $1, $4) }
  | type_spec IDENT LBRACKET NUM RBRACKET ASSIGN LBRACE expressions RBRACE { ArrayDeclaration ($2, $4, $1, $8) }
  | type_spec STAR IDENT { PointerDeclaration ($3, $1) }

declarations: //arguments de fonctions
  | declaration COMMA declarations { $1 :: $3 }
  | declaration { [$1] }
  | { [] }

statement:
  | ifs { $1 }
  | declaration { $1 }
  | assignment { $1 }
  | loop { $1 }
  | RETURN expression { Return $2 }

statements:
  | statement SEMICOLON statements { $1 :: $3 }
  | statement SEMICOLON { [$1] }

func:
  | type_spec IDENT LPAREN declarations RPAREN LBRACE statements RBRACE { Function ($2, $7) }

functions:
  | func functions { $1 :: $2 }
  | func { [$1] }

program:
  | functions EOF { Program $1 }