open Printf

type wf = Wf of string
type lemma = Lemma of string
type tag = Tag of string

type subreading = lemma * (tag list)
type reading = subreading list
type cohort = Cohort of wf * (reading list)
type stream = cohort list

