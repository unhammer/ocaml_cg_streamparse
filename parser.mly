%{ open Cg %}

%token <string> WF
%token <string> LEMMA
%token <string> TAG
%token <string> BLANK
%token <string> SINGLECHAR
%token NEWLINE
%token EOF

%start <Cg.stream> main

%%
main:
  | s = nonempty_list(cohort); EOF { s }
  ;

cohort:
  | list(blank); wf = WF; r = list(reading); list(blank) { Cohort (Wf wf, r) }
  ;

reading:
  | lemma = LEMMA; t = list(tag); option(NEWLINE) { [Lemma lemma, t] }
  ;

tag:
  | t = TAG { Tag t }
  ;

blank:
  | b = BLANK { b }
  | b = SINGLECHAR { b }
  ;
