type subreading = (string * string list)
type reading = subreading list

type cohort = {
  sf : string;
  readings : reading list;
  preblank : string
}

val list_of_lines : string list -> cohort list

val enum_of_lines : string BatEnum.t -> cohort BatEnum.t


val rehtml_of_list : cohort list -> string


val flat_subtags : reading -> string list

