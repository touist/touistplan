let encoding_list = ["qbf-efa";"qbf-noop";"qbf-efa-nfla";"sat";"sat-efa"]
and mode = ref "touistplan"
and encoding = ref "qbf-efa"
and domain = ref ""
and problem = ref ""
and constraints = ref ""
and gpminlevel = ref 0
and options = ref ""

let () =
  let usage = "\
Usage: "^Sys.argv.(0)^" [-e ENC] DOMAIN PROBLEM [-c CONSTR]

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
    ("-c", Arg.Set_string constraints,
     "CONSTR: strips planning constraints expressed in (typed) Extended-PDDL");

    ("--gpminlevel", Arg.Set_int gpminlevel,
     ("N: set the gpminlevel (mode (2) only) [default: "^ string_of_int !gpminlevel ^"]"));

    ("--options", Arg.Set_string options,
     ("OPTIONS: ??? [default: "^ !options ^"]"));
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

  match !encoding with
  | "qbf-efa" -> (new Touistplan.t !problem !domain !options 0)#search
  | "qbf-noop" -> (new Touistplan.t !problem !domain !options 1)#search
  | "qbf-efa-nfla" -> (new Touistplan.t !problem !domain !options 2)#search
  | "qbf-open" -> (new Touistplan.t !problem !domain !options 3)#search
  | "sat" -> (new Touistplan.t !problem !domain !options 100)#search
  | "sat-efa" -> (new Touistplan.t !problem !domain !options 100)#search
  | "sat-open" -> (new Touistplan.t !problem !domain !options 103)#search
  | "smt-open" -> (new Touistplan.t !problem !domain !options 203)#search
  | _ -> failwith "encoding impossible (tell the dev)"