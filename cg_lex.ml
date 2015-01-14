open Batteries

(* ------------------------------------------------------------------------ *)
(* Lexer *)
exception MalformedCGReading of string

(* These regexes have first a group matching a delimiter, then a group
   matching the content; used by line_type *)
let sf_pat = "^(\"<)((?:.|[\\$]\")*)>\"$"
let reading_pat = "^(\t+)(\")((?:.|[\\$]\")*)\"(.*)$"
let line_rex = Pcre.regexp_or [sf_pat; reading_pat]

let match_groups =
  List.filter_map (function Pcre.Group (n, s) -> Some s | _ -> None)

let split_tags tags =
  List.remove (String.nsplit ~by:" " tags ) ""

(* Used internally between the lexer and the "parser": *)
type line_type = Sf of string | Subreading of int * string * (string list) | Blank of string

(* Parse a single CG line: *)
let lex_line line =
  let res = Pcre.full_split ~rex:line_rex line in
  match match_groups res with
  | ["\"<"; sf] -> Sf sf
  | [tabs; "\""; lemma; tags] ->
    let depth = String.length tabs - 1 in
    Subreading (depth, lemma, split_tags tags)
  | [] -> Blank line
  | _ -> raise (MalformedCGReading line)

