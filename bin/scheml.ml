let () = Lib.Toplevel.cli () 

(* let () =
  let t =
    Lib.Ast.Pair (Lib.Ast.Int 1, Lib.Ast.Pair (Lib.Ast.Int 2, Lib.Ast.Null))
  in
  let is = Lib.Ast.is_list t in
  is |> string_of_bool |> print_endline *)

(* type op = Add | Subt [@@deriving show { with_path = false }]

   let operation =
     Lib.Ast.Pair
       ( Lib.Ast.Symbol "+",
         Lib.Ast.Pair
           ( Lib.Ast.Int 5,
             Lib.Ast.Pair
               ( Lib.Ast.Int 5,
                 Lib.Ast.Pair
                   ( Lib.Ast.Int 5,
                     Lib.Ast.Pair
                       ( Lib.Ast.Int 5,
                         Lib.Ast.Pair
                           ( Lib.Ast.Int 5,
                             Lib.Ast.Pair (Lib.Ast.Int 5, Lib.Ast.Null) ) ) ) ) )
       )

   let operation' =
     Lib.Ast.Pair
       ( Lib.Ast.Symbol "-",
         Lib.Ast.Pair
           ( Lib.Ast.Int 1,
             Lib.Ast.Pair
               ( Lib.Ast.Int 2,
                 Lib.Ast.Pair
                   ( Lib.Ast.Int 3,
                     Lib.Ast.Pair
                       ( Lib.Ast.Int 4,
                         Lib.Ast.Pair
                           ( Lib.Ast.Int 5,
                             Lib.Ast.Pair (Lib.Ast.Int 6, Lib.Ast.Null) ) ) ) ) )
       )

   let test_type = Lib.Ast.Pair ((Lib.Ast.Int 1), (Lib.Ast.Pair ((Lib.Ast.Int 2), (Lib.Ast.Pair ((Lib.Ast.Int 3), Lib.Ast.Null)))))

   let get_op_type pair =
     match pair with
     | Lib.Ast.Pair (a, b) when a = Symbol "+" -> (Add, b)
     | Lib.Ast.Pair (a, b) when a = Symbol "-" -> (Subt, b)
     | _ -> failwith "malformed operator"

   let int_of_Int =
     function
     | Lib.Ast.Int i -> i
     | _ -> failwith "error"

   let pp_list lst =
     let lst = lst |> List.map (fun a -> int_of_Int a |> string_of_int) |> String.concat ";"
     in Printf.printf "[%s]\n" lst

   let rec list_of_pair pair =
       match pair with
       | Lib.Ast.Pair (Lib.Ast.Int i, Lib.Ast.Null) -> [ Lib.Ast.Int i ]
       | Lib.Ast.Pair (Lib.Ast.Int i, Lib.Ast.Pair (i', p)) ->
           Int i :: list_of_pair (Lib.Ast.Pair (i', p))
       | _ -> failwith "error to parse pair"

   let t () =
     let _op', rest = operation' |> get_op_type in
     rest |> list_of_pair |> pp_list

   let is_int check =
     match check with
     | Lib.Ast.Int _ -> true
     | _ -> failwith "wrong type"

   let () =
     let rest = list_of_pair test_type in
     let f = List.filter (fun a -> is_int a <> true) rest in
     let int_list = List.map int_of_Int rest in
     if List.length f = 0 then
       List.fold_left (+) 0 int_list |> print_int
     else
       failwith "error"; *)
