let encoding_list = ["qbf-efa";"qbf-noop";"qbf-efa-nfla";"qbf-open";"sat";"sat-efa";"sat-open";"sat-efa-select";"smt-open"]
and solvers_list = ["depqbf"; "rareqs"; "caqe"; "qute"; "minisat"; "glucose"; "glucose-syrup"; "picosat"; "lingeling"]
and mode = ref "touistplan"
and encoding = ref "qbf-efa"
and domain = ref ""
and problem = ref ""
and constraints = ref ""
and gpminlevel = ref 0
and options = ref ""
and solver = ref "depqbf"
and incrmode = ref 1
and incmin = ref 1
and timeout = ref 0
and verbose = ref 0 (* 0 = not verbose *)
and count = ref None

let () =
  let usage = "\
Usage: "^Sys.argv.(0)^" [-e ENC] [-s SOLVER] DOMAIN PROBLEM [-c CONSTR] [--timeout SECONDS]

This is the TouISTPLAN planner.

Note that the constraints file '-c CONSTR' is only usable with '-e sat-efa'.

DOMAIN: strips planning domain expressed in (typed) PDDL
PROBLEM: strips planning problem expressed in (typed) PDDL
"
  and argspecs = [ (* This list enumerates the different flags (-x,-f...)*)
    ("-e", Arg.Symbol (encoding_list, fun s -> encoding:=s), (
        ": TouISTPLAN encoding [default: "^ !encoding ^"]\n"^
        "\t- qbf-efa : QBFPLAN with Explanatory Frame-Axioms (default)\n"^
        "\t- qbf-noop : QBFPLAN with No-op Actions\n"^
        "\t- sat-efa : SATPLAN with Explanatory Frame-Axioms"));
    ("-s", Arg.Symbol (solvers_list, fun s -> solver:=s), ": solver choice");
    ("-c", Arg.Set_string constraints,
     "CONSTR: strips planning constraints expressed in (typed) Extended-PDDL");

    ("-incr", Arg.Set_int incrmode,
     "2: double plan length at each increment with SAT/SMT encodings");

    ("-incmin", Arg.Set_int incmin,
     "2: double plan length at each increment with SAT/SMT encodings");

    ("--gpminlevel", Arg.Set_int gpminlevel,
     ("N: set the gpminlevel (mode (2) only) [default: "^ string_of_int !gpminlevel ^"]"));

    ("--timeout", Arg.Set_int timeout,
     ("SECONDS: search & extract are both given SECONDS to complete. 0=infinite \
      [default: "^ string_of_int !timeout ^"]"));

    ("-v", Arg.Unit (fun _ -> verbose := 1), ("a bit verbose"));
    ("-vv", Arg.Unit (fun _ -> verbose := 2), ("more verbose"));

    ("--options", Arg.Set_string options,
     ("OPTIONS: ??? [default: "^ !options ^"]"));

    ("--count", Arg.Int (fun i -> count := Some i),
     ("count the branch and node constraints"));
  ]

  (* The 'alone' arguments (not preceeded by a '--something') are going to be
     processed by this function in the order they appear. The first alone
     argument is interpreted as DOMAIN, the second is PROBLEM. *)
  and process_arg_alone arg =
    match !domain, !problem with
    | "", _ -> domain := arg (*  *)
    | _, "" -> problem := arg
    | _, _  -> (Printf.eprintf "Usage: %s [opts] DOMAIN PROBLEM (see --help).\n"
                  Sys.argv.(0); exit 1)
  in
  Arg.parse argspecs (process_arg_alone) usage; (* Parse command-line args *)
  Utils.begin_time := Utils.get_time ();

  (* Check that the user entered DOMAIN and PROBLEM *)
  if !domain = "" || !problem = "" then
    (Printf.eprintf "Usage: %s [opts] DOMAIN PROBLEM (see --help).\n" Sys.argv.(0); exit 1);
  (* If -c CONSTR has been given, check that it is using 'sat-efa' *)
  if !constraints <> "" && !encoding <> "sat-efa" then
    (Printf.eprintf "Usage: -c must be used with -e sat-efa (see --help).\n"; exit 1);

  if !incmin < 1 then 
    (Printf.eprintf "Usage: -incmin N\n N must be greater than 0 (see --help).\n"; exit 1)
  else incmin := !incmin - 1;


  let solver_code = match !solver with
    | "depqbf" -> 0
    | "rareqs" -> 1
    | "caqe" -> 2
    | "qute" -> 3
    | "minisat" -> 0
    | "glucose" -> 101
    | "glucose-syrup" -> 102
    | "picosat" -> 103
    | "lingeling" -> 104
    | v -> failwith "solver unknown (tell the dev)"
  in
  match !count with
  | Some i -> (new CountConstraints.t !problem !domain i)#search
  | _ ->
    match !encoding with
    | "qbf-efa" -> (new Touistplan.t !problem !domain !options 0 solver_code !incrmode !incmin !timeout !verbose)#search
    | "qbf-noop" -> (new Touistplan.t !problem !domain !options 1 solver_code !incrmode !incmin !timeout !verbose)#search
    | "qbf-efa-nfla" -> (new Touistplan.t !problem !domain !options 2 solver_code !incrmode !incmin !timeout !verbose)#search
    | "qbf-open" -> (new Touistplan.t !problem !domain !options 3 solver_code !incrmode !incmin !timeout !verbose)#search
    | "sat" -> (new Touistplan.t !problem !domain !options 100 solver_code !incrmode !incmin !timeout !verbose)#search
    | "sat-efa" -> (new Touistplan.t !problem !domain !options 100 solver_code !incrmode !incmin !timeout !verbose)#search
    | "sat-open" -> (new Touistplan.t !problem !domain !options 103 solver_code !incrmode !incmin !timeout !verbose)#search
    | "sat-efa-select" -> (new Touistplan.t !problem !domain !options 150 solver_code !incrmode !incmin !timeout !verbose)#search
    | "smt-open" -> (new Touistplan.t !problem !domain !options 203 solver_code !incrmode !incmin !timeout !verbose)#search
    | _ -> failwith "encoding impossible (tell the dev)"
