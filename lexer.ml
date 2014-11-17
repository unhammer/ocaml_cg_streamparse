open Sedlex_menhir

let lex_blank lexbuf =
  let buf = lexbuf.stream in
  match%sedlex buf with
  | Star (Compl '\n'), Opt "\n" -> update lexbuf ; new_line lexbuf ; Parser.BLANK (lexeme lexbuf)
  | eof -> update lexbuf ; Parser.EOF
  | _ -> raise_ParseError lexbuf

let lex_bol lexbuf =
  assert Lexing.(lexbuf.pos.pos_cnum = lexbuf.pos.pos_bol);
  (* At beginning of line, we decide between WF, LEMMA (reading) or blank *)
  let buf = lexbuf.stream in
  match%sedlex buf with
  | "\"<", Star (Compl '\n'), ">\"\n" -> update lexbuf ; new_line lexbuf ; Parser.WF (lexeme lexbuf)
  | Plus "\t", "\"", Star (Compl '"'), "\"" -> update lexbuf ; Parser.LEMMA (lexeme lexbuf)
  | eof -> update lexbuf ; Parser.EOF
  | _ -> lex_blank lexbuf

let lex_tags lexbuf =
  let buf = lexbuf.stream in
    match%sedlex buf with
    | Plus " ", Plus (Compl (Chars " \n")) -> update lexbuf ; Parser.TAG (lexeme lexbuf)
    | "\n" -> update lexbuf ; new_line lexbuf ; Parser.NEWLINE
    | eof -> update lexbuf ; Parser.EOF
    | _ -> raise_ParseError lexbuf

let lex lexbuf =
  if Lexing.(lexbuf.pos.pos_cnum = lexbuf.pos.pos_bol) then
    lex_bol lexbuf
  else
    lex_tags lexbuf
