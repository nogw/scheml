type var = string

type expr 
  = Symbol of var
  | Int of int
  | True
  | False
  | Null
  | Pair of expr * expr
  | Macro of string * (expr -> expr)
  | Builtin of string * (environment -> expr -> expr)
  | Procedure of environment * var list * expr

and environment = (var * expr) list

type command 
  = Expr of expr
  | Define of var * expr
  | Use of string
  | Quit

let rec string_of_expr = 
  function
  | Symbol s -> s
  | Int i -> string_of_int i 
  | Macro (m, _) -> "#<primitive-macro " ^ m ^ ">"
  | Builtin (b, _) -> "#<primitive-procedure " ^ b ^ ">"
  | Procedure (_, args, _) -> "#<procedure (" ^ (String.concat " " args) ^ ")>"
  | True -> "#true"
  | False -> "#false"
  | Null -> "()"
  | Pair (a, b) -> "(" ^ (string_of_pair a b) ^ ")"

and string_of_pair e = 
  function
  | Null -> string_of_expr e
  | Pair (a, b) -> (string_of_expr a) ^ " " ^ (string_of_expr b)
  | b -> (string_of_expr e) ^ " " ^ (string_of_expr b)