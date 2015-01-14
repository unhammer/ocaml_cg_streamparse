open Batteries
open Cg_lex

type subreading = (string * string list)
type reading = subreading list

type cohort = {
  sf : string;
  readings : reading list;
  preblank : string
}

(* ------------------------------------------------------------------------ *)
(* Lexer to parser types and helpers *)

(* Remove apertium superblanks: any [] after a non-escape (or an
   even number of escapes) can be removed: *)
let blank_rex = Pcre.regexp "((^|[^\\\\])(\\\\\\\\)*)[][]+"
let double_escape_rex = Pcre.regexp "\\\\"
let blanks_of_group group =
  List.filter_map (function Blank b -> Some b | _ -> None) group
  |> String.join "\n"
  |> Pcre.replace ~rex:blank_rex ~templ:"$1"
  |> Pcre.replace ~rex:double_escape_rex ~templ:"\\"

let test_blank_rex () =
  List.iter (fun (input,expected) ->
      assert (expected = (Pcre.replace ~rex:blank_rex ~templ:"$1" input));
      assert (("foo"^expected) = ("foo"^(Pcre.replace ~rex:blank_rex ~templ:"$1" input))))
    [("[", "");
     ("\\[", "\\[");
     ("\\\\[", "\\\\");
     ("\\\\\\[", "\\\\\\[");
     ("\\\\\\\\[", "\\\\\\\\");
     ("\\\\\\\\\\[", "\\\\\\\\\\[");
     ("\\\\\\\\\\\\[", "\\\\\\\\\\\\");
    ]

let sf_of_group group =
  List.filter_map (function Sf b -> Some b | _ -> None) group
  |> String.join "\n"

let readings_of_group group =
  let rec loop prev acc = function
    | (0, l, t)::rest ->
      if prev = [] then
        loop [(l,t)] acc rest
      else
        let group = List.rev prev in
        loop [(l,t)] (group::acc) rest
    | (_,l,t)::rest ->
      loop ((l,t)::prev) acc rest
    | [] ->
      let group = List.rev prev in
      List.rev (group::acc)
  in
  List.filter_map (function Subreading (n,l,t) -> Some (n,l,t) | _ -> None) group
  |> loop [] []

let cohort_of_group group =
  (* TODO: what to do on no wf? expected right before EOF, so
     exceptions out of the question. Options would work, but messy
     since then you have options on all of sf/wf/readings. Could
     have a sum type of cohort|justblanks ... *)
  { sf = sf_of_group group;
    readings = readings_of_group group;
    preblank = blanks_of_group group }


(* ------------------------------------------------------------------------ *)
(* "Public" functions: *)

let list_of_lex lines =
  let rec group_lines prev acc = function
    | (Subreading _ as s)::(Blank _ as b)::rest
    | (Subreading _ as s)::(Sf _ as b)::rest ->
      let group = List.rev (s::prev) in
      group_lines [] (group::acc) (b::rest)
    | x::rest ->
      group_lines (x::prev) acc rest
    | [] ->
      let group = List.rev prev in
      List.rev (group::acc)
  in
  let groups = group_lines [] [] lines in
  let rec cohorts_of_groups acc groups =
    match groups with
    | group::rest ->
      cohorts_of_groups (cohort_of_group group::acc) rest
    | [] -> List.rev acc
  in
  cohorts_of_groups [] groups

let list_of_lines lines =
  List.map lex_line lines |> list_of_lex


let enum_of_lex lex =
  let cur = ref (Enum.get lex) in
  let next = ref (Enum.get lex) in
  let skip () =
    cur := !next;
    next := Enum.get lex
  in
  let return group =
    cohort_of_group (List.rev group)
  in
  let rec loop acc =
    match !cur,!next with
    | Some (Subreading _ as a), Some (Blank _)
    | Some (Subreading _ as a), Some (Sf _) ->
      skip (); return (a::acc)

    | Some a, Some _ ->
      skip (); loop (a::acc)

    | Some a, None ->
      skip (); return (a::acc)

    | None, _ ->
      raise BatEnum.No_more_elements
  in
  BatEnum.from (fun () -> loop [])

let enum_of_lines lines =
  Enum.map lex_line lines |> enum_of_lex


(* ------------------------------------------------------------------------ *)
(* Tag/reading helpers: *)

let flat_subtags sub =
  List.map snd sub |> List.flatten

let rehtml_of_list cohorts =
  List.map (fun c -> c.preblank^c.sf) cohorts
  |> BatString.join ""
