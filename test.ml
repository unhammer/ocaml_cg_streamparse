open Sedlex_menhir

let tokenise filename =
  let inx = open_in filename in
  let lexbuf = create_lexbuf ~file:filename (Sedlexing.Utf8.from_channel inx) in
  let rec loop () =
    match Lexer.lex lexbuf with
    | Parser.EOF -> print_endline "EOF"
    | Parser.NEWLINE -> print_endline "NEWLINE"; loop ()
    | Parser.BLANK b -> print_endline ("BLANK "^b);loop ()
    | Parser.TAG b -> print_endline ("TAG "^b);loop ()
    | Parser.LEMMA b -> print_endline ("LEMMA "^b);loop ()
    | Parser.WF wf -> print_endline ("WF "^wf);loop ()
  in
  loop (); close_in inx

let print_cg stream =
  let open Cg in
  let rec print_subreadings = function
    | (Lemma lm, tags)::rest -> Printf.printf "LM: <%s> T: %s " lm (List.fold_left (fun a (Tag t) -> a^",<"^t^">") "" tags); print_subreadings rest
    | [] -> Printf.printf "\n"
  in
  let rec print_stream = function
    | Cohort (Wf wf, readings)::rest -> Printf.printf "WF:<%s> " wf;List.iter print_subreadings readings;print_stream rest
    | [] -> print_endline "EOF"
  in
  print_stream stream

let parse filename =
  let inx = open_in filename in
  let lexbuf = create_lexbuf ~file:filename (Sedlexing.Utf8.from_channel inx) in
  let results = sedlex_with_menhir Lexer.lex Parser.main lexbuf in
  print_cg results;
  close_in inx

let () =
  match Sys.argv with
  | [| _ ; path |] ->
    print_endline "---tokenise---"; tokenise path;
    print_endline "---parse---"; parse path
  | _ -> raise (Failure "pathmissing")
