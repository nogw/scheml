open Ast
open Interprete;;

define "symbol?"
  (Builtin
     ("symbol?", fun e -> match arg1 e with Symbol _ -> True | _ -> False))
;;

define "number"
  (Builtin ("number", fun e -> match arg1 e with Int _ -> True | _ -> False))
;;

(* define "+"
     (Builtin
        ( "+",
          fun e ->
            match arg2 e with
            | Int a, Int b -> Int (a + b)
            | _ -> error "wrong type" ))
   ;; *)

define "+"
  (Builtin
     ( "+",
       fun e ->
         let rest = list_of_pair e in
         let check = List.filter (fun a -> is_int a <> true) rest in
         if List.length check = 0 then
           List.fold_left ( + ) 0 (List.map int_of_Int rest) |> fun a -> Int a
         else error "error" ))
;;

define "-"
  (Builtin
     ( "-",
       fun e ->
         match arg2 e with
         | Int a, Int b -> Int (a - b)
         | _ -> error "wrong type" ))
;;

define "*"
  (Builtin
     ( "*",
       fun e ->
         match arg2 e with
         | Int a, Int b -> Int (a * b)
         | _ -> error "wrong type" ))
;;

define "/"
  (Builtin
     ( "/",
       fun e ->
         match arg2 e with
         | Int a, Int 0 -> error "division by zero"
         | Int a, Int b -> Int (a / b)
         | _ -> error "wrong type" ))
;;

define "="
  (Builtin
     ( "=",
       fun e ->
         match arg2 e with
         | Int a, Int b -> if a = b then True else False
         | _ -> error "wrong type" ))
;;

define "<"
  (Builtin
     ( "<",
       fun e ->
         match arg2 e with
         | Int a, Int b -> if a < b then True else False
         | _ -> error "wrong type" ))
;;

define ">"
  (Builtin
     ( ">",
       fun e ->
         match arg2 e with
         | Int a, Int b -> if a > b then True else False
         | _ -> error "wrong type" ))
;;

define "lambda"
  (Macro
     ( "lambda",
       fun env e ->
         match e with
         | Pair (args, Pair (body, Null)) ->
             Procedure (env, list_of_args args, body)
         | _ -> error "malformed lambda" ))
;;

define "macro?"
  (Builtin ("macro?", fun e -> match arg1 e with Macro _ -> True | _ -> False))
;;

define "boolean?"
  (Builtin
     ("boolean?", fun e -> match arg1 e with True | False -> True | _ -> False))
;;

define "if"
  (Macro
     ( "if",
       fun env e ->
         match e with
         | Pair (p, Pair (e1, Pair (e2, Null))) -> (
             match eval env p with False -> eval env e2 | _ -> eval env e1)
         | _ -> error "malformed if" ))
;;

define "equal?"
  (Builtin ("equal?", fun e -> if equal (arg2 e) then True else False))
;;

define "list"
  (Builtin ("list", fun e -> if is_list e then e else error "wrong number"))
;;

define "cons"
  (Builtin
     ( "cons",
       fun e ->
         let e1, e2 = arg2 e in
         Pair (e1, e2) ))
;;

define "car"
  (Builtin
     ( "car",
       fun e -> match arg1 e with Pair (_, e) -> e | _ -> error "wrong type" ))
;;

define "cdr"
  (Builtin
     ( "cdr",
       fun e -> match arg1 e with Pair (e, _) -> e | _ -> error "wrong type" ))
;;

define "null?"
  (Builtin ("null?", fun e -> if arg1 e = Null then True else False))
;;

define "pair?"
  (Builtin ("pair?", fun e -> match arg1 e with Pair _ -> True | _ -> False))
;;

define "list?"
  (Builtin ("list?", fun e -> if is_list (arg1 e) then True else False))
;;

define "quote" (Macro ("quote", fun env e -> arg1 e))

let exec () = ()
