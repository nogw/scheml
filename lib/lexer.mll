{
  open Parser
  open Lexing
  open Ast
}

let letter = ['a'-'z' 'A'-'Z' '<' '>' '/' '?' '!' '@' '#' '%' '^' '&' '+' '*' '_' '=' '|' ':']

let number = ['0'-'9']

let alpha = letter | number

let symbol = '-' alpha* (letter | '-')+ alpha* | '-' | letter alpha*

rule token = parse
    ';' [^'\n']* '\n' { Lexing.new_line lexbuf; token lexbuf }
  | '\n'              { Lexing.new_line lexbuf; token lexbuf }
  | [' ' '\t']        { token lexbuf }
  | '-'? ['0'-'9']+   { INT (int_of_string(lexeme lexbuf)) }
  | "("               { LPAREN }
  | ")"               { RPAREN }
  | "#true"           { TRUE }
  | "#false"          { FALSE }
  | '.'               { DOT }
  | '\''              { QUOTE }
  | "$quit"           { QUIT }
  | "$use"            { USE }
  | '\"' [^'\"'] '\"' { let str = lexeme lexbuf in STRING (String.sub str 1 (String.length str - 2)) }
  | "define"          { DEFINE }
  | symbol            { SYMBOL (lexeme lexbuf) }
  | eof               { EOF }

{
}