let _ = 
  Utils.begin_time := Utils.get_time () ;
  let print_help () =
    Utils.eprint "\nUsage: %s DOMAIN PROBLEM CONSTRAINTS [ALGORITHM]\n
DOMAIN: strips temporal planning domain expressed in (typed) PDDL
PROBLEM: strips temporal planning problem expressed in (typed) PDDL
CONSTRAINTS: strips temporal planning constraints expressed in (typed) PDDL
ALGORITHM: one of the following:
\t- tlp-gp : Temporally Lifted Progression GraphPlan (TLP-GP-1)
\t- smtplan : Sat Modulo Theory temporal PLANner (TLP-GP-2)

\n" Sys.argv.(0) ;
    exit 0
  in
  let nb_args = Array.length Sys.argv in
(*  let algo = *)
    if nb_args < 5 then print_help ()
    else
(*      if nb_args = 3 then "tlp-gp" else Sys.argv.(3) in
    match algo with
      | "tlp-gp" -> *)

(new Tlpgp.t (if nb_args = 4 then 0 else (int_of_string Sys.argv.(4))))#search

(*      | "smtplan" -> (new Smtplan.t)#search
      | "tsp2" -> (new Tsp2.t)#print 
      | "tsp" -> (new Tsp.t)#print 
      | x -> Utils.eprint "\n%s : search procedure unknown.\n" x ; print_help () *)
