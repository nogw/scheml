type var = string [@@deriving show { with_path = false }]

type expr =
  | Symbol of var
  | Int of int
  | True
  | False
  | Null
  | Pair of expr * expr
  | Macro of string * (environment -> expr -> expr)
  | Builtin of string * (expr -> expr)
  | Procedure of environment * var list * expr
[@@deriving show { with_path = false }]

and environment = (var * expr) list [@@deriving show { with_path = false }]

type command = Expr of expr | Define of var * expr | Use of string | Quit
[@@deriving show { with_path = false }]

let rec is_list = function
  | Null -> true
  | Pair (_, e) -> is_list e
  | _ -> false

let rec equal (e1, e2) =
  match e1 with
  | True | False | Symbol _ | Int _ | Null -> e1 = e2
  | Pair (e1', e2') -> (
      match e2' with
      | Pair (e1'', e2'') -> equal (e1', e1'') && equal (e2', e2'')
      | _ -> false)
  | Procedure _ | Macro _ | Builtin _ -> e1 == e2

let rec string_of_expr = function
  | Symbol s -> s
  | Int i -> string_of_int i
  | Macro (m, _) -> "#<primitive-macro " ^ m ^ ">"
  | Builtin (b, _) -> "#<primitive-procedure " ^ b ^ ">"
  | Procedure (_, args, _) -> "#<procedure (" ^ String.concat " " args ^ ")>"
  | True -> "#true"
  | False -> "#false"
  | Null -> "()"
  | Pair (a, b) -> "(" ^ string_of_pair a b ^ ")"

and string_of_pair e = function
  | Null -> string_of_expr e
  | Pair (a, b) -> string_of_expr a ^ " " ^ string_of_expr b
  | b -> string_of_expr e ^ " " ^ string_of_expr b
