open Ast

exception RuntimeError of string

let definitions = ref []

let error msg = raise (RuntimeError msg)

let define x e = definitions := (x, e) :: !definitions

let lookup x env =
  try List.assoc x env
  with Not_found -> (
    try List.assoc x !definitions
    with Not_found -> error ("unknown variable " ^ x))

let rec list_of_args = function
  | Null -> []
  | Pair (Symbol s, e) -> s :: list_of_args e
  | _ -> error "malformed formals"

let rec expand_env args e env =
  match (args, e) with
  | [], Null -> env
  | x :: xs, Pair (e, e1) -> expand_env xs e1 ((x, e) :: env)
  | _, _ -> error "wrong number of arguments"

and eval env = function
  | Symbol s -> lookup s env
  | (Int _ | Procedure _ | Macro _ | Builtin _ | True | False) as e -> e
  | Null -> error "missing expression"
  | Pair (e, e1) -> (
      match eval env e with
      | Builtin (_, f) -> f (eval_args env e1)
      | Macro (_, f) -> f env e1
      | Procedure (env', args, e) ->
          eval (expand_env args (eval_args env e1) env') e
      | e -> error ("wrong type to apply: " ^ string_of_expr e))

and eval_args env = function
  | Null -> Null
  | Pair (e, e1) -> Pair (eval env e, eval_args env e1)
  | _ -> error "malformed arguments"

let arg1 = function
  | Pair (e, Null) -> e
  | _ -> error "wrong number of arguments"

let arg2 = function
  | Pair (e, Pair (e1, Null)) -> (e, e1)
  | _ -> error "wrong number of arguments"

let rec list_of_pair pair =
  match pair with
  | Pair (Int i, Null) -> [ Int i ]
  | Pair (Int i, Pair (i', p)) -> Int i :: list_of_pair (Pair (i', p))
  | _ -> failwith "error to parse pair"

let is_int check = match check with Int _ -> true | _ -> failwith "wrong type"

let int_of_Int = function Int i -> i | _ -> failwith "error"

let show_result expr = expr |> string_of_expr |> print_endline
