type typ =
  | IntType
  | FloatType
  | DoubleType
  | VoidType

type binopE = Add | Sub | Mul | Div |Mod 

type uniopE = Pp | Mm

type expr =
  | Num of int
  | FloatNum of float
  | Var of string
  | BinOpE of binopE * expr * expr
  | UniOpE of uniopE
  | Malloc of expr

type binopC = Lt | Le | Gt | Ge | Eq | Ne

type cond = 
  |BinOpC of binopC * expr * expr

type stmt =
  | Declaration of string * typ
  | InitDeclaration of string * typ * expr
  | ArrayDeclaration of string * int * typ * expr list
  | PointerDeclaration of string * typ
  | Assignment of string * expr
  | ArrayAssign of string * expr * expr
  | ForLoop of stmt * cond * stmt * stmt list
  | If of stmt list 
  | Return of expr

type func = Function of string * stmt list

type program = Program of func list