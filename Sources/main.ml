let _ = 
  Utils.begin_time := Utils.get_time () ;
  let print_help () =
    Utils.eprint "\nUsage: %s DOMAIN PROBLEM [ENCODING | CONSTRAINTS]\n
DOMAIN: strips planning domain expressed in (typed) PDDL
PROBLEM: strips planning problem expressed in (typed) PDDL
ENCODING: one of the following:
\t- qbf-efa : QBFPLAN with Explanatory Frame-Axioms (default)
\t- qbf-noop : QBFPLAN with No-op Actions
\t- sat-efa : SATPLAN with Explanatory Frame-Axioms
CONSTRAINTS: strips planning constraints expressed in (typed) Extended-PDDL

\n" Sys.argv.(0) ;
    exit 0
  in
  let nb_args = Array.length Sys.argv in
    if nb_args < 3 then print_help ()
    else


let encoding = if nb_args = 3 then 0 else match Sys.argv.(3) with
  | "qbf-efa" -> 0
  | "qbf-noop" -> 1
  | "qbf-efa-nfla" -> 2
  | "sat" -> 100
  | "sat-efa" -> 100
  | _ -> -1
in

if encoding == -1 then
(new Tlpgp.t (if nb_args = 4 then 0 else (int_of_string Sys.argv.(4))))#search
else
(new Touistplan.t (if nb_args = 3 then 0 else encoding))#search
