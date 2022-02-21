let pair = Lib.Ast.Pair (Lib.Ast.Int 1, Lib.Ast.Int 2)

let () = Lib.Ast.string_of_expr pair |> print_endline