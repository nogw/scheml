open Lexing

let lexer_from_channel fn ch =
  let lex = Lexing.from_channel ch in
  let pos = lex.lex_curr_p in
  lex.lex_curr_p <- { pos with pos_fname = fn; pos_lnum = 1 };
  lex

let lexer_from_string str =
  let lex = Lexing.from_string str in
  let pos = lex.lex_curr_p in
  lex.lex_curr_p <- { pos with pos_fname = ""; pos_lnum = 1 };
  lex

let string_of_position { pos_fname = fn; pos_lnum = ln; pos_bol; pos_cnum = cn }
    =
  let character = cn - pos_bol in
  if fn = "" then "Character " ^ string_of_int character
  else
    "File \"" ^ fn ^ "\", line " ^ string_of_int ln ^ ", character "
    ^ string_of_int character

let string_of msg pos = string_of_position pos ^ ":\n" ^ msg

let syntax_error { lex_curr_p = pos } = string_of "Syntax error" pos

let show_error = print_endline

let pp_definitions list' =
  let rec aux list =
    match list with
    | [] -> ()
    | (_, b) :: xs ->
        b |> Ast.show_expr |> print_string;
        print_endline ";";
        aux xs
  in
  aux !list'
