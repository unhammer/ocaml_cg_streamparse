open Batteries

let expected =
  let open Cg in
  [{sf = "ord";
    readings =
      [[("hei", ["kake"])];
       [("noko", ["meir"; "enn"; "dette"]); ("sub", ["lesing"])]];
    preblank = "ads"};
   {sf = "ord2"; readings = [[("der", ["ost"])]]; preblank = "sa\nhi"};
   {sf = "eller ord2"; readings = [[("med der", ["oste"])]]; preblank = ""};
   {sf = ""; readings = [[]]; preblank = "hi"}]

let test () =
  let result = open_in "test.cg"
               |> IO.lines_of
               |> Cg.enum_of_lines
               |> List.of_enum
  in
  assert (result = expected)


let () =
  test ()
