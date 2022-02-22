open Ast
open Utils

exception FatalError of string

let fatal_error msg = raise (FatalError msg)

let rec execute_command = function
  | Expr e ->
      let v = Interprete.eval [] e in
      Interprete.show_result v
  | Define (x, e) -> Interprete.define x (Interprete.eval [] e)
  | Quit -> raise End_of_file
  | Use fn -> execute_file fn

and execute_file fn =
  let fh = open_in fn in
  let lex = Utils.lexer_from_channel fn fh in
  try
    let commands = Parser.toplevel Lexer.token lex in
    close_in fh;
    execute_commands commands
  with
  | Interprete.RuntimeError msg -> fatal_error msg
  | Sys.Break -> fatal_error "Interrupted"
  | Parsing.Parse_error -> fatal_error (Utils.syntax_error lex)

and execute_commands commands = List.iter execute_command commands

let shell ast () =
  print_endline "Welcome to Scheml REPL";
  print_endline "Press CTRL-C to exit";
  try
    while true do
      try
        print_string "user> ";
        let str = read_line () in
        let lex = Utils.lexer_from_string str in
        let commands =
          try Parser.toplevel Lexer.token lex
          with Parsing.Parse_error -> fatal_error (Utils.syntax_error lex)
        in
        if not ast then execute_commands commands
        else commands |> List.map show_command |> List.iter print_endline
      with
      | FatalError msg -> Utils.show_error msg
      | Interprete.RuntimeError msg -> Utils.show_error msg
      | Sys.Break -> Utils.show_error "Interrupted"
    done
  with End_of_file -> print_endline "nothing here"

let execute file interactive ast =
  Standard.exec ();

  match file with
  | Some f -> (
      try
        execute_file f;
        if interactive then shell ast ()
      with FatalError msg -> Utils.show_error msg)
  | None -> shell ast ()

let cli () =
  let open Cmdliner in
  let file = Arg.(value & pos 0 (some string) None & info [] ~doc:"<file>")
  and interactive =
    Arg.(
      value & flag & info [ "r"; "repl" ] ~doc:"-ln <file> load file in repl")
  and ast =
    Arg.(value & flag & info [ "a"; "ast" ] ~doc:"display ast instant result")
  in

  let man =
    [
      `S Manpage.s_synopsis;
      `P "Scheml [ARG] [FILENAME]";
      `S Manpage.s_bugs;
      `P "Report bugs to <github.com/nogw/ocaml-scheme/issues>";
    ]
  in
  Term.exit
  @@ Term.eval
       ( Term.(const execute $ file $ interactive $ ast),
         Term.info "Scheml" ~version:"v1.0.0" ~man )
