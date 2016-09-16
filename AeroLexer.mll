{
  open AeroParser
  open Lexing

  let incr lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }
}

let var = ['_' 'a'-'z' 'A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*

rule token = parse
    '#' [^'\n']* '\n' { incr lexbuf; token lexbuf }
  | '\n'            { incr lexbuf; token lexbuf }
  | [' ' '\t']      { token lexbuf }
  | "bool"          { BOOL }
  | "int"           { INTEGER }
  | "false"         { FALSE }
  | "true"          { TRUE }
  | ['0'-'9']+      { INT (int_of_string(lexeme lexbuf)) }
  | "fun"           { FUN }
  | "for"           { FOR }
  | "while"         { WHILE }
  | "do"            { DO }
  | "if"            { IF }
  | "else"          { ELSE }
  | "then"          { THEN }
  | "in"            { IN }
  | "is"            { IS }
  | "let"           { LET }
  | "not"           { NOT }
  | "||"            { OR }
  | "&&"            { AND }
  | ";;"            { SEMICOLON }
  | ";"             { EOL }
  | "##"            { incr lexbuf; token lexbuf }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | '*'             { TIMES }
  | '+'             { PLUS }
  | ','             { COMMA }
  | '.'             { PERIOD }
  | "length"        { LENGTH }
  | "hd"            { HEAD }
  | "print"         { PRINT }
  | "printnl"       { PRINTNEWLINE }
  | "printstring"   { PRINTSTRING }
  | "import"        { GET }
  | "tl"            { TAIL }
  | "match"         { MATCH }
  | "with"          { WITH }
  | "++"            { INCR }
  | "--"            { DECR }
  | '-'             { MINUS }
  | "::"            { CONS }
  | "->"            { APPLY }
  | '/'             { DIVIDE }
  | ':'             { COLON }
  | '<'             { LESS }
  | '>'             { GREATER }
  | '"'             { QUOTE }
  | '='             { EQUAL }
  | '{'             { LBRACE }
  | '}'             { RBRACE }
  | '['             { LBLOCK }
  | ']'             { RBLOCK }
  | var             { VAR (lexeme lexbuf) }
  | eof             { EOF }
