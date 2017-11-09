class node_common =
object (self)

  val mutable n = Array.make 256 0
  method n level = n.(level)
  method set_n level nn = n.(level) <- nn

  val mutable level = max_int
  method level = level
  method set_level nlevel = level <- nlevel

end

class fluent atom =
object (self)
  inherit node_common
  inherit [action] Node.fluent atom

  val string = 
    let s = atom#pred#to_string ^ 
    (if atom#nb_terms = 0 then "" else "_" ^  (Utils.string_of_array "_" Utils.to_string atom#terms))
(*    ^ "[" ^ (string_of_int (fst atom#timeset)) ^ ";" ^ (string_of_int (snd atom#timeset)) ^ "]" *) in
    for i = 0 to (String.length s) - 1 do
     if (String.get s i) = '-' then (String.set s i '_');
    done;
    s

  val mutable neglevel = max_int
  method neglevel = neglevel
  method set_neglevel nneglevel = neglevel <- nneglevel

  method to_string = string
end


and action name params duration quality prec nprec add del =
object (self)
  inherit node_common
  inherit [fluent] Node.action name params duration quality prec nprec add del

  val mutable maxlevel = -1

  val string = 
    let s = name ^ if Array.length params = 0 then "" else "_" ^ (Utils.string_of_array "_" Utils.to_string params) in
    for i = 0 to (String.length s) - 1 do
     if (String.get s i) = '-' then (String.set s i '_');
    done;
    s

  method maxlevel = maxlevel
  method set_maxlevel maxl = maxlevel <- maxl

  method to_string = string

  method to_complete_string = 
    let string_of_fluent_array fluents =
      Utils.string_of_array "#" Utils.to_string fluents in
      "s$Prec$" ^ string ^ "#" ^ string_of_fluent_array prec ^ "#\n" ^
      "s$Add$" ^ string ^ "#" ^ string_of_fluent_array add ^ "#\n" ^
      "s$Del$" ^ string ^ "#" ^ string_of_fluent_array del ^ "#\n"
end


class plan succes = 
object 
  inherit [fluent, action] SequentialPlan.t succes
end

class ['fluent, 'action, 'plan] tsp_common =
object
  method create_fluent = new fluent
  method create_action = new action
  val plan_succes = new plan true
  val plan_fail = new plan false
  method plan_succes = plan_succes
  method plan_fail = plan_fail
end


class t problem domain =
object (self)
  inherit [fluent, action, plan] PlanningData.t problem domain "" as pdata
  inherit [fluent, action, plan] tsp_common

  val mutable solved = false
  val mutable nb = 0
  val mutable nbc = 0
  val mutable rpg_max_level = 0

  method print_statistics = ()
  method run = self#plan_fail

(* outil de création de domaines TEMPO, supprimer _create_tempo *)

method search_create_tempo =
  let rec strmulti str num = match num with
   | 0 -> ""
   | n -> (strmulti str (pred n)) ^ str
  in
  for cpt=2 to 20 do
let i=(cpt*10) in
  let smtfile = Unix.openfile ("generator/tempo-matrix/domain-tempo-matrix-" ^ (string_of_int i) ^ ".pddl") [Unix.O_TRUNC;Unix.O_CREAT;Unix.O_WRONLY] 0o640 in
   let smtwrite s = ignore (Unix.write smtfile s 0 (String.length s)) in
   smtwrite
       (Printf.sprintf "(define (domain domain-tempo-matrix-%d)\n  (:requirements :strips :durative-actions)\n  (:predicates " i);
  for j=1 to i do smtwrite (Printf.sprintf "(RessourceA-n%d ?ra)\n               (RessourceB-n%d ?rb)
               (GoalA-n%d ?ga)\n               (GoalB-n%d ?gb)\n               (GoalC-n%d ?gc)\n               (ActiveRessource-n%d ?r)\n               (ActiveGoal-n%d ?g)\n\n               " j j j j j j j); done;
     smtwrite ")\n";
     smtwrite ("  (:durative-action A-n1\n      :parameters (?ra ?ga ?gb)\n      :duration (= ?duration 5)\n      :condition (and (over all (RessourceA-n1 ?ra))\n                      (over all (GoalA-n1 ?ga))\n                      (over all (GoalB-n1 ?gb)))\n      :effect (and (at start (ActiveRessource-n1 ?ra))\n                   (at end (not (ActiveRessource-n1 ?ra)))\n                   (at end (ActiveGoal-n1 ?ga))\n                   (at end (not (ActiveGoal-n1 ?gb)))))\n  (:durative-action B-n1\n      :parameters (?ra ?rb ?gb)\n      :duration (= ?duration 4)\n      :condition (and (over all (RessourceA-n1 ?ra))\n                      (over all (RessourceB-n1 ?rb))\n                      (over all (GoalB-n1 ?gb))\n                      (at start (ActiveRessource-n1 ?ra)))\n      :effect (and (at start (ActiveRessource-n1 ?rb))\n                   (at end (not (ActiveRessource-n1 ?rb)))\n                   (at end (ActiveGoal-n1 ?gb))))\n  (:durative-action C-n1\n      :parameters (?rb ?ga ?gc)\n      :duration (= ?duration 1)\n      :condition (and (over all (RessourceB-n1 ?rb))\n                      (over all (GoalA-n1 ?ga))\n                      (over all (GoalC-n1 ?gc))\n                      (at start (ActiveRessource-n1 ?rb)))\n      :effect (and (at end (not (ActiveGoal-n1 ?ga)))\n                   (at end (ActiveGoal-n1 ?gc))))\n\n");
   for j=2 to i do
      smtwrite (Printf.sprintf ";; n%d\n\n  (:durative-action A-n%d\n      :parameters (?ga0 ?gb0 ?gc0 ?ra ?ga ?gb)\n      :duration (= ?duration 5)\n      :condition (and (over all (RessourceA-n%d ?ra))\n                      (over all (GoalA-n%d ?ga))\n                      (over all (GoalB-n%d ?gb))\n                      (over all (GoalA-n%d ?ga0))\n                      (over all (GoalB-n%d ?gb0))\n                      (over all (GoalC-n%d ?gc0))\n                      (at start (ActiveGoal-n%d ?ga0))\n                      (at start (ActiveGoal-n%d ?gb0))\n                      (at start (ActiveGoal-n%d ?gc0)))\n      :effect (and (at start (ActiveRessource-n%d ?ra))\n                   (at end (not (ActiveRessource-n%d ?ra)))\n                   (at end (ActiveGoal-n%d ?ga))\n                   (at end (not (ActiveGoal-n%d ?gb)))))\n  (:durative-action B-n%d\n      :parameters (?ra ?rb ?gb)\n      :duration (= ?duration 4)\n      :condition (and (over all (RessourceA-n%d ?ra))\n                      (over all (RessourceB-n%d ?rb))\n                      (over all (GoalB-n%d ?gb))\n                      (at start (ActiveRessource-n%d ?ra)))\n      :effect (and (at start (ActiveRessource-n%d ?rb))\n                   (at end (not (ActiveRessource-n%d ?rb)))\n                   (at end (ActiveGoal-n%d ?gb))))\n  (:durative-action C-n%d\n      :parameters (?rb ?ga ?gc)\n      :duration (= ?duration 1)\n      :condition (and (over all (RessourceB-n%d ?rb))\n                      (over all (GoalA-n%d ?ga))\n                      (over all (GoalC-n%d ?gc))\n                      (at start (ActiveRessource-n%d ?rb)))\n      :effect (and (at end (not (ActiveGoal-n%d ?ga)))\n                   (at end (ActiveGoal-n%d ?gc))))\n" j j j j j (j-1) (j-1) (j-1) (j-1) (j-1) (j-1) j j j j j j j j j j j j j j j j j j j);
    done;
    smtwrite ")\n";
    Unix.close smtfile;
for j =1 to 1 do (* nombre de pbs par domaine *)
  let smtfile = Unix.openfile ("generator/tempo-matrix/tempo-matrix" ^ (string_of_int i) ^ "x" ^ (string_of_int j) ^ ".pddl") [Unix.O_TRUNC;Unix.O_CREAT;Unix.O_WRONLY] 0o640 in
   let smtwrite s = ignore (Unix.write smtfile s 0 (String.length s)) in
    smtwrite (Printf.sprintf "(define (problem tempo-matrix%dx%d)\n  (:domain domain-tempo-matrix-%d)\n  (:objects ra rb\n            " i j i);
   for k=1 to i do
   for l=1 to j do
     smtwrite (Printf.sprintf "ga%d-%d gb%d-%d gc%d-%d\n            " l k l k l k);
   done; done;
    smtwrite ")\n;; g<number_in_step><step>\n  (:init ";
   for k=1 to i do
     smtwrite (Printf.sprintf "(RessourceA-n%d ra) (RessourceB-n%d rb)\n         " k k);
   done;
   for k=1 to i do
   for l=1 to j do
     smtwrite (Printf.sprintf "(GoalA-n%d ga%d-%d) (GoalB-n%d gb%d-%d) (GoalC-n%d gc%d-%d)\n         " k l k k l k k l k);
   done; done;
    smtwrite ")\n  (:goal (and ";
   for k=1 to i do
   for l=1 to j do
     smtwrite (Printf.sprintf "(ActiveGoal-n%d ga%d-%d) (ActiveGoal-n%d gb%d-%d) (ActiveGoal-n%d gc%d-%d)\n              " k l k k l k k l k);
   done; done;
    smtwrite "))\n    )\n";
    Unix.close smtfile;
done;
  done

(*method search =
  let rec strmulti str num = match num with
   | 0 -> ""
   | n -> (strmulti str (pred n)) ^ str
  in
  for i=1 to 10 do
  let smtfile = Unix.openfile ("tempotests/domain-3tempo" ^ (string_of_int i) ^ ".pddl") [Unix.O_TRUNC;Unix.O_CREAT;Unix.O_WRONLY] 0o640 in
   let smtwrite s = ignore (Unix.write smtfile s 0 (String.length s)) in
   smtwrite
       (Printf.sprintf "(define (domain domain-3tempo%d)
  (:requirements :strips :durative-actions)
  (:predicates (b) (d) (e)" i);
  for j=1 to i do smtwrite (Printf.sprintf " (a%s) (b%s) (c%s) (d%s) (e%s)\n" (strmulti "a" j) (strmulti "b" j) (strmulti "c" j) (strmulti "d" j) (strmulti "e" j)); done;
     smtwrite "  )\n";
    for j=1 to i do
      smtwrite (Printf.sprintf "  (:durative-action A%s
      :duration (= ?duration 5)
      :precondition (and (at start (b%s))
                         (at start (d%s))
                         (at start (e%s)))
      :effect (and (at start (a%s))
              (at end (not (a%s)))
              (at end (b%s))
              (at end (not (d%s)))))
  (:durative-action B%s
      :duration (= ?duration 4)
      :condition
                    (at start (a%s))
      :effect
                    (and (at start (c%s))
                         (at end (not (c%s)))
                         (at end (d%s))))
  (:durative-action C%s
      :duration (= ?duration 1)
      :condition
                    (at start (c%s))
      :effect
                    (and (at end (not (b%s)))
                         (at end (e%s))))\n"
    (strmulti "A" j) (strmulti "b" (j-1)) (strmulti "d" (j-1)) (strmulti "e" (j-1))
    (strmulti "a" j)(strmulti "a" j)(strmulti "b" j)(strmulti "d" j)
    (strmulti "B" j)(strmulti "a" j)(strmulti "c" j)(strmulti "c" j)(strmulti "d" j)
    (strmulti "C" j)(strmulti "c" j)(strmulti "b" j)(strmulti "e" j));
    done;
    smtwrite ")\n";
    Unix.close smtfile;
  let smtfile = Unix.openfile ("tempotests/pfile-3tempo" ^ (string_of_int i) ^ ".pddl") [Unix.O_TRUNC;Unix.O_CREAT;Unix.O_WRONLY] 0o640 in
   let smtwrite s = ignore (Unix.write smtfile s 0 (String.length s)) in
    smtwrite (Printf.sprintf "(define (problem 3tempo%d)
    (:domain domain-3tempo%i)
  (:init (b) (d) (e))
  (:goal (and " i i);
    (*for k=0 to j do*) smtwrite (Printf.sprintf "(b%s) (d%s) (e%s)" (strmulti "b" i)(strmulti "d" i)(strmulti "e" i)); (*done;*)
    smtwrite "))
    )\n";
    Unix.close smtfile;
  done *)

  method search = (* search_real : supprimer _real pour obtenir SMTPLAN original *)
    let (search_time,_) = Utils.my_time "Searching plan (SMT-PLAN algorithm)" (fun () -> self#notimed_search) in
      Utils.eprint "Total search time : %.2f\n" search_time

  method notimed_search =

    let setminlevel = if Array.length Sys.argv > 4 then int_of_string Sys.argv.(4) else 1 in

    let f_exists (f : 'fluent) (a_set : 'fluent array) = 
      Array.fold_left (fun t (fluent : 'fluent) ->
        (fluent#atom#equal f#atom) || t
      ) false a_set
    in

    let get_f_timedata (f : 'fluent) (a_iset : (('fluent*Timedata.t) array)) = 
    (*let timedata_null = (new Timedata.t) in*)
      Array.fold_left (fun t ((fluent,timedata) : ('fluent*Timedata.t)) ->
        if (fluent#atom#equal f#atom)
        then timedata
        else t
      ) (snd (Array.get a_iset 0)) a_iset
    in

    let get_f_time f a_iset = 
      let r = (Array.fold_left (fun t x -> if ((fst x)#atom#equal f#atom) then (snd x)#timeset else t) (-1.0,-1.0) a_iset) in
      if r == (-1.0,-1.0) then begin Utils.eprint "STM-PLAN Fatal Error : timelabel not found\n"; exit 0 end
      else r
    in
    let get_f f a_iset =
      Array.fold_left (fun c x -> if ((fst x)#atom#equal f#atom) then (fst x) else c) f a_iset
    in
    let current_level = ref 0 in
    let build_rpg = (* Building the Temporal Relaxed Planning Graph *)
      Utils.print "\nBuilding the Temporal Relaxed Planning Graph ...\n";
      Array.iter (fun f ->
                   (if (List.exists (fun p ->
                                        (p#atom#equal f#atom))
                        (Array.to_list pdata#init_state))
                    then f#set_level 0))
      pdata#fluents;
      while (not (List.for_all (fun p -> 
                    (List.exists (fun f ->
                       (f#atom#equal p#atom) && (f#level <= !current_level))
                    (Array.to_list pdata#fluents)))
                  (Array.to_list pdata#goal)))
      && (!current_level<=255) do
 (*     while (Array.fold_left (fun a f -> a &&
                    (Array.fold_left (fun b p -> b ||
                       ((p#atom#equal f#atom) && (f#level <= !current_level)))
                    false pdata#goal))
                  true pdata#goal)
      && (!current_level<=255) do *)
        Array.iter (fun a -> (if (a#level > !current_level) &&
          (Array.fold_left (fun b prec -> b &&
            Array.fold_left (fun c f -> c ||
              ((f#atom#equal prec#atom) && (f#level <= !current_level)))
            false pdata#fluents)
          true a#prec)
          then begin
               a#set_level (succ !current_level);
               Array.iter (fun f ->
                   if (f#level > !current_level) && (List.exists (fun add ->
                                 (f#atom#equal add#atom))
                                 (Array.to_list a#add))
                   then f#set_level (succ !current_level))
               pdata#fluents;
               Array.iter (fun f ->
                   if (f#neglevel > !current_level) && (List.exists (fun del ->
                                 (f#atom#equal del#atom))
                                 (Array.to_list a#del))
                   then f#set_neglevel (succ !current_level))
               pdata#fluents;
               end))
          pdata#actions;
        (*Array.iter (fun a -> if (a#level > !current_level) &&
               (List.for_all (fun prec ->
                    (List.exists (fun f ->
                       (f#atom#equal prec#atom) && (f#level <= !current_level))
                    (Array.to_list pdata#fluents)))
                  (Array.to_list a#prec))
               then a#set_level (succ !current_level))
        pdata#actions;*)
 
       current_level := succ !current_level
      done;
      rpg_max_level <- !current_level;
Utils.print "Goal found at level %d.\n" !current_level;
(*Array.iter (fun a -> Utils.print "%s(level[%d])\n" a#to_string a#level) pdata#actions;
Array.iter (fun f -> Utils.print "%s(level[%d],neglevel[%d])\n" f#to_istring f#level f#neglevel) pdata#fluents;*)

    in

(* 
    let solver = (new Smtsolver.t) in
    let smtwrite = solver#smtwrite in

      solver#set_smtfilename "smtplan.smt";  *)
      build_rpg;

(* TEST - RECHERCHE COMPLETE : *) (* rpg_max_level <- 1; *)

while not solved do

if setminlevel <= rpg_max_level then begin (* Begin search if current maxlevel >= minlevel *)

(* Reducing Graph *)
  let subgoals = ref [] in
  let newsubgoals = ref [] in
    subgoals := (Array.to_list pdata#goal);
    for i = rpg_max_level downto 1 do
      List.iter (fun (f : 'fluent) ->
        Array.iter (fun (a : 'action) ->
          if (a#maxlevel < i) then
          begin
            a#set_maxlevel i;
            Array.iter (fun (p : 'fluent) ->
              newsubgoals := p :: !newsubgoals;
            ) a#prec
          end
        ) f#producers
      ) !subgoals;
      subgoals := (List.append !newsubgoals !subgoals);
      newsubgoals := [];
    done;

      Utils.print "\nSearching plan with SMT encoding at level %d\n" rpg_max_level;
      

let encode_float x = "(/ " ^ string_of_int (int_of_float (x*.1000000.0)) ^ " 1000000)"
in


    let solver = (new Smtsolver.t) in
    let smtwrite = solver#smtwrite in

     (* solver#set_smtfilename "smtplan.smt"; *)
      solver#open_smtwrite;

(*     smtwrite (Printf.sprintf "%s\n" solver#stringtest);
      solver#close_smtwrite;
      if solver#launch then Utils.print "STM_SAT\n";
      exit 0; *)

      solver#smtwrite (Printf.sprintf "(benchmark default.smt\n:source {\nSMT-PLAN_GP1 automated Planning to SMT-LIB encoding\nby F.Maris and P.Regnier, IRIT - Universite Paul Sabatier, Toulouse\nBenchmark encoded from planning problem\ndomain : %s, problem : %s\n}\n:logic QF_RDL\n:extrafuns ((St_spy_variable Real))\n:extrafuns ((t_Init Real))\n:extrafuns ((t_Goal Real))\n" pdata#domain_name pdata#problem_name);
      nb <- 2;

      for i = 1 to rpg_max_level do
	Array.iter (fun a -> if (a#level <= i) && (a#maxlevel >= i) then begin nb <- nb + 1 ; a#set_n i nb ; smtwrite (Printf.sprintf ":extrafuns ((t_%s%i Real))\n" a#to_string i) end) pdata#actions ;
      done ;
     Utils.print "  Number of real variables : %d\n" nb;
     nb <- 2;
(*     for i = 0 to level do
	Array.iter (fun f -> nb <- nb + 1 ; f#set_n i nb ; smtwrite (Printf.sprintf ":extrapreds ((%s.%i))\n" f#to_string i)) pdata#fluents
      done ; *)
      smtwrite ":extrapreds ((Init))\n:extrapreds ((Goal))\n";
      for i = 1 to rpg_max_level do
	Array.iter (fun a -> if (a#level <= i) && (a#maxlevel >= i) then begin nb <- nb + 1 ; a#set_n i nb ; smtwrite (Printf.sprintf ":extrapreds ((%s%i))\n" a#to_string i) end) pdata#actions ;
      done ;

      for i = 0 to (rpg_max_level - 1) do
        Array.iter (fun b -> if (b#level <= succ i) && (b#maxlevel >= succ i) then
          Array.iter (fun f -> let init_exists = (Array.fold_left (fun c x -> (x#atom#equal f#atom) || c) false pdata#init_state) in
            Array.iter (fun a ->
              for j = 1 to i do
                if (a#level <= j) && (a#maxlevel >= j) then begin
                 nb <- succ nb;
                 smtwrite (Printf.sprintf ":extrapreds ((Link_%s%d.%s.%s%d))\n" a#to_string j f#to_string b#to_string (succ i))
                end
              done
            ) f#producers;
            if init_exists then
              smtwrite (Printf.sprintf ":extrapreds ((Link_Init.%s.%s%d))\n" f#to_string b#to_string (succ i));
          ) b#prec
        ) pdata#actions
      done;
          Array.iter (fun f -> let init_exists = (Array.fold_left (fun c x -> (x#atom#equal f#atom) || c) false pdata#init_state) in
            Array.iter (fun a ->
              for j = 1 to rpg_max_level do
                if (a#level <= j) && (a#maxlevel >= j) then begin
                 nb <- succ nb;
                 smtwrite (Printf.sprintf ":extrapreds ((Link_%s%d.%s.Goal))\n" a#to_string j f#to_string)
                end
              done
            ) f#producers;
            if init_exists then
              smtwrite (Printf.sprintf ":extrapreds ((Link_Init.%s.Goal))\n" f#to_string);
          ) pdata#goal;


      Utils.print "  Number of propositional variables : %d\n" nb;

(* Encoding rule n°1 : Init state and Goal *)

smtwrite ":formula\n( and\n(= St_spy_variable (+ 1 t_Init))\nInit Goal\n";
nbc <- 2;

(* Encoding rule n°2 : preconditions and causal links *)

      for i = 0 to (rpg_max_level - 1) do
        Array.iter (fun b -> if (b#level <= succ i) && (b#maxlevel >= succ i) then begin
          Array.iter (fun f -> let init_exists = (Array.fold_left (fun c x -> (x#atom#equal f#atom) || c) false pdata#init_state) in
            if ((Array.length f#producers) <> 0) || init_exists then begin
            nbc <- succ nbc;
            smtwrite (Printf.sprintf "(or (not %s%i)" b#to_string (succ i));
            Array.iter (fun a ->
              for j = 1 to i do
                if (a#level <= j) && (a#maxlevel >= j) then begin
                 smtwrite (Printf.sprintf " Link_%s%d.%s.%s%d" a#to_string j f#to_string b#to_string (succ i))
                end
              done
            ) f#producers;
            if init_exists then
              smtwrite (Printf.sprintf " Link_Init.%s.%s%d" f#to_string b#to_string (succ i));
            smtwrite ")\n";
            end
          ) b#prec end
        ) pdata#actions
      done;
      Array.iter (fun f -> let init_exists = (Array.fold_left (fun c x -> (x#atom#equal f#atom) || c) false pdata#init_state) in
        if ((Array.length f#producers) <> 0) || init_exists then begin
        nbc <- succ nbc;
        smtwrite "(or (not Goal)";
        Array.iter (fun a ->
          for j = 1 to rpg_max_level do
            if (a#level <= j) && (a#maxlevel >= j) then begin
             smtwrite (Printf.sprintf " Link_%s%d.%s.Goal" a#to_string j f#to_string)
            end
          done
        ) f#producers;
        if init_exists then
          smtwrite (Printf.sprintf " Link_Init.%s.Goal" f#to_string);
        smtwrite ")\n";
        end
      ) pdata#goal;

(* Encoding rule n°3 : actions and partial order *)

      for i = 0 to (rpg_max_level - 1) do
        Array.iter (fun b -> if (b#level <= succ i) && (b#maxlevel >= succ i) then begin
          Array.iter (fun f -> if ((Array.length f#producers) <> 0) then begin
            Array.iter (fun a ->
              for j = 1 to i do
                if (a#level <= j) && (a#maxlevel >= j) then begin
                 nbc <- nbc + 3;
                 smtwrite (Printf.sprintf "(or (not Link_%s%d.%s.%s%d) %s%d)\n" a#to_string j f#to_string b#to_string (succ i) a#to_string j);
                 smtwrite (Printf.sprintf "(or (not Link_%s%d.%s.%s%d) %s%d)\n" a#to_string j f#to_string b#to_string (succ i) b#to_string (succ i));
                 let aadd_tdata = (get_f_timedata f a#iadd) in
                 let bprec_tdata = (get_f_timedata f b#iprec) in
                 let (ft_add,closed_add) =
                   match aadd_tdata#code with
                    | 0 -> ((fst aadd_tdata#timeset),aadd_tdata#closed_left)
                    | 1 -> ((snd aadd_tdata#timeset),aadd_tdata#closed_right)
                    | _ -> (0.0,true)
                 in
                 let ft_prec =  fst bprec_tdata#timeset  in
                   if (ft_prec > ft_add) then
                   smtwrite (Printf.sprintf "(or (not Link_%s%d.%s.%s%d) (<= (- t_%s%d t_%s%d) %s))\n" a#to_string j f#to_string b#to_string (succ i) a#to_string j b#to_string (succ i) (encode_float (ft_prec -. ft_add)))
                   else
                   smtwrite (Printf.sprintf "(or (not Link_%s%d.%s.%s%d) (>= (- t_%s%d t_%s%d) %s))\n" a#to_string j f#to_string b#to_string (succ i) b#to_string (succ i) a#to_string j (encode_float (ft_add -. ft_prec)));
(*let tfluent1 = get_f_time f a#iadd in
let tfluent2 = get_f_time f b#iprec in
 Utils.print "%s%d[%d;%d] -> %s -> %s%d[%d;%d]\n" a#to_string j (fst (tfluent1)) (snd (tfluent1)) f#to_string b#to_string (succ i) (fst (tfluent2)) (snd (tfluent2));*)
                end
              done
            ) f#producers;
            end;
            if (Array.fold_left (fun c x -> (x#atom#equal f#atom) || c) false pdata#init_state)
             then begin
                 nbc <- nbc + 3;
                 smtwrite (Printf.sprintf "(or (not Link_Init.%s.%s%d) Init)\n" f#to_string b#to_string (succ i));
                 smtwrite (Printf.sprintf "(or (not Link_Init.%s.%s%d) %s%d)\n" f#to_string b#to_string (succ i) b#to_string (succ i));
                 let ft_prec =  fst (get_f_time f b#iprec)  in
                   smtwrite (Printf.sprintf "(or (not Link_Init.%s.%s%d) (<= (- t_Init t_%s%d) %s))\n" f#to_string b#to_string (succ i) b#to_string (succ i) (encode_float ft_prec));
             end
          ) b#prec end
        ) pdata#actions
      done;

          Array.iter (fun f -> if ((Array.length f#producers) <> 0) then begin
            Array.iter (fun a ->
              for j = 1 to rpg_max_level do
                if (a#level <= j) && (a#maxlevel >= j) then begin
                 nbc <- nbc + 3;
                 smtwrite (Printf.sprintf "(or (not Link_%s%d.%s.Goal) Goal)\n" a#to_string j f#to_string);
                 smtwrite (Printf.sprintf "(or (not Link_%s%d.%s.Goal) %s%d)\n" a#to_string j f#to_string a#to_string j);
                 let aadd_tdata = (get_f_timedata f a#iadd) in
                 let ft_add =
                   match aadd_tdata#code with
                    | 0 -> (fst aadd_tdata#timeset)
                    | 1 -> (snd aadd_tdata#timeset)
                    | _ -> 0.0
                 in
                   smtwrite (Printf.sprintf "(or (not Link_%s%d.%s.Goal) (>= (- t_Goal t_%s%d) %s))\n" a#to_string j f#to_string a#to_string j (encode_float ft_add));
                end
              done
            ) f#producers;
            end;
            if (Array.fold_left (fun c x -> (x#atom#equal f#atom) || c) false pdata#init_state)
             then begin
                 nbc <- nbc + 3;
                 smtwrite (Printf.sprintf "(or (not Link_Init.%s.Goal) Goal)\n" f#to_string);
                 smtwrite (Printf.sprintf "(or (not Link_Init.%s.Goal) Init)\n" f#to_string);
                 smtwrite (Printf.sprintf "(or (not Link_Init.%s.Goal) (>= (- t_Goal t_Init) 0))\n" f#to_string);
             end
          ) pdata#goal;



(* Encoding rule n°4 : temporally extended mutexes *)

  (* Link protection for user defined actions *)
      for i = 1 to (rpg_max_level - 1) do
        Array.iter (fun b -> if (b#level <= succ i) && (b#maxlevel >= succ i) then begin
          Array.iter (fun f -> if ((Array.length f#producers) <> 0) && ((Array.length f#deleters) <> 0) then begin
            Array.iter (fun a ->
              for j = 1 to i do
                if (a#level <= j) && (a#maxlevel >= j) then begin
                  Array.iter (fun c ->
                    for k = 1 to rpg_max_level do
                    if (c#level <= k) && (c#maxlevel >= k) then begin
                    nbc <- succ nbc;
                    let aadd_tdata = (get_f_timedata f a#iadd) in
                    let bprec_tdata = (get_f_timedata f b#iprec) in
                    let cdel_tdata = (get_f_timedata f c#idel) in
                    let (ft_aadd,closed_aadd) =
                      match aadd_tdata#code with
                       | 0 -> ((fst aadd_tdata#timeset),aadd_tdata#closed_left)
                       | 1 -> ((fst aadd_tdata#timeset),aadd_tdata#closed_left)
                       | _ -> (0.0,true)
                    in
                    let (ft_bprec,closed_bprec) =  ((snd bprec_tdata#timeset),bprec_tdata#closed_right)  in
                    let ((ft_cdel_left,closed_cdel_left),(ft_cdel_right,closed_cdel_right)) =
                      match cdel_tdata#code with
                       | 0 -> (((fst cdel_tdata#timeset),cdel_tdata#closed_left),
                               ((snd cdel_tdata#timeset),cdel_tdata#closed_right))
                       | 1 -> (((fst cdel_tdata#timeset),cdel_tdata#closed_left),
                               ((snd cdel_tdata#timeset),cdel_tdata#closed_right))
                       | _ -> ((0.0,true),(0.0,true))
                    in
                    begin
(*Utils.print "Gestion du conflit [%s%d](%d)->%s->(%d)[%s%d] et [%s%d](%d)->not_%s\n" a#to_string j ft_aadd f#to_string ft_bprec b#to_string (succ i) c#to_string k ft_cdel f#to_string;*)
                      smtwrite (Printf.sprintf "(or (not Link_%s%d.%s.%s%d) (not %s%d) " a#to_string j f#to_string b#to_string (succ i) c#to_string k);
                      let (comp1,comp2) = if closed_aadd && closed_cdel_right then ("<",">") else ("<=",">=") in
                      if (ft_cdel_right < ft_aadd) then
                       smtwrite (Printf.sprintf "(%s (- t_%s%d t_%s%d) %s) " comp1 c#to_string k a#to_string j (encode_float (ft_aadd -. ft_cdel_right)))
                      else
                       smtwrite (Printf.sprintf "(%s (- t_%s%d t_%s%d) %s) " comp2 a#to_string j c#to_string k (encode_float (ft_cdel_right -. ft_aadd)));
                      let (comp1,comp2) = if closed_bprec && closed_cdel_left then (">","<") else (">=","<=") in
                      if (ft_cdel_left < ft_bprec) then
                       smtwrite (Printf.sprintf "(%s (- t_%s%d t_%s%d) %s))\n" comp1 c#to_string k b#to_string (succ i) (encode_float (ft_bprec -. ft_cdel_left)))
                      else
                       smtwrite (Printf.sprintf "(%s (- t_%s%d t_%s%d) %s))\n" comp2 b#to_string (succ i) c#to_string k (encode_float (ft_cdel_left -. ft_bprec)));
                    end end
                    done
                  ) f#deleters
                end
              done
            ) f#producers;
            end
          ) b#prec end
        ) pdata#actions
      done;
  (* Link protection for special action Init *)
          Array.iter (fun f -> if ((Array.length f#consumers) <> 0) && ((Array.length f#deleters) <> 0) then begin
            Array.iter (fun b ->
              for i = 1 to rpg_max_level do
                if (b#level <= i) && (b#maxlevel >= i) then begin
                  Array.iter (fun c ->
                    for k = 1 to rpg_max_level do
                    if (c#level <= k) && (c#maxlevel >= k) then begin
                    nbc <- succ nbc;
                    let bprec_tdata = (get_f_timedata f b#iprec) in
                    let cdel_tdata = (get_f_timedata f c#idel) in
                    let (ft_bprec,closed_bprec) =  ((snd bprec_tdata#timeset),bprec_tdata#closed_right)  in
                    let ((ft_cdel_left,closed_cdel_left),(ft_cdel_right,closed_cdel_right)) =
                      match cdel_tdata#code with
                       | 0 -> (((fst cdel_tdata#timeset),cdel_tdata#closed_left),
                               ((snd cdel_tdata#timeset),cdel_tdata#closed_right))
                       | 1 -> (((fst cdel_tdata#timeset),cdel_tdata#closed_left),
                               ((snd cdel_tdata#timeset),cdel_tdata#closed_right))
                       | _ -> ((0.0,true),(0.0,true))
                    in
                    begin
                      smtwrite (Printf.sprintf "(or (not Link_Init.%s.%s%d) (not %s%d) " f#to_string b#to_string i c#to_string k);
(*Utils.print "Gestion du conflit [Init](0)->%s->(%d)[%s%d] et [%s%d](%d)->not_%s\n" f#to_string ft_bprec b#to_string i c#to_string k ft_cdel f#to_string;*)
                      let (comp1,comp2) = if closed_cdel_right then ("<",">") else ("<=",">=") in
                      if (ft_cdel_right < 0.0) then
                       smtwrite (Printf.sprintf "(%s (- t_%s%d t_Init) %s) " comp1 c#to_string k (encode_float (-. ft_cdel_right)))
                      else
                       smtwrite (Printf.sprintf "(%s (- t_Init t_%s%d) %s) " comp2 c#to_string k (encode_float ft_cdel_right));
                      let (comp1,comp2) = if closed_bprec && closed_cdel_left then (">","<") else (">=","<=") in
                      if (ft_cdel_left < ft_bprec) then
                       smtwrite (Printf.sprintf "(%s (- t_%s%d t_%s%d) %s))\n" comp1 c#to_string k b#to_string i (encode_float (ft_bprec -. ft_cdel_left)))
                      else
                       smtwrite (Printf.sprintf "(%s (- t_%s%d t_%s%d) %s))\n" comp2 b#to_string i c#to_string k (encode_float (ft_cdel_left -. ft_bprec)));
                    end end
                    done
                  ) f#deleters
                end
              done
            ) f#consumers;
            end
          ) pdata#init_state;
  (* Link protection for special action Goal *)
          Array.iter (fun f -> if ((Array.length f#producers) <> 0) && ((Array.length f#deleters) <> 0) then begin
            Array.iter (fun a ->
              for j = 1 to rpg_max_level do
                if (a#level <= j) && (a#maxlevel >= j) then begin
                  Array.iter (fun c ->
                    for k = 1 to rpg_max_level do
                    if (c#level <= k) && (c#maxlevel >= k) then begin
                    nbc <- succ nbc;
                    let aadd_tdata = (get_f_timedata f a#iadd) in
                    let cdel_tdata = (get_f_timedata f c#idel) in
                    let (ft_aadd,closed_aadd) =
                      match aadd_tdata#code with
                       | 0 -> ((fst aadd_tdata#timeset),aadd_tdata#closed_left)
                       | 1 -> ((fst aadd_tdata#timeset),aadd_tdata#closed_left)
                       | _ -> (0.0,true)
                    in
                    let ((ft_cdel_left,closed_cdel_left),(ft_cdel_right,closed_cdel_right)) =
                      match cdel_tdata#code with
                       | 0 -> (((fst cdel_tdata#timeset),cdel_tdata#closed_left),
                               ((snd cdel_tdata#timeset),cdel_tdata#closed_right))
                       | 1 -> (((fst cdel_tdata#timeset),cdel_tdata#closed_left),
                               ((snd cdel_tdata#timeset),cdel_tdata#closed_right))
                       | _ -> ((0.0,true),(0.0,true))
                    in
                    begin
                      smtwrite (Printf.sprintf "(or (not Link_%s%d.%s.Goal) (not %s%d) " a#to_string j f#to_string c#to_string k);
                      let (comp1,comp2) = if closed_aadd && closed_cdel_right then ("<",">") else ("<=",">=") in
                      if (ft_cdel_right < ft_aadd) then
                       smtwrite (Printf.sprintf "(%s (- t_%s%d t_%s%d) %s) " comp1 c#to_string k a#to_string j (encode_float (ft_aadd -. ft_cdel_right)))
                      else
                       smtwrite (Printf.sprintf "(%s (- t_%s%d t_%s%d) %s) " comp2 a#to_string j c#to_string k (encode_float (ft_cdel_right -. ft_aadd)));
                      let (comp1,comp2) = if closed_cdel_left then (">","<") else (">=","<=") in
                      if (ft_cdel_left < 0.0) then
                       smtwrite (Printf.sprintf "(%s (- t_%s%d t_Goal) %s))\n" comp1 c#to_string k (encode_float (-. ft_cdel_left)))
                      else
                       smtwrite (Printf.sprintf "(%s (- t_Goal t_%s%d) %s))\n" comp2 c#to_string k (encode_float ft_cdel_left));
                    end end
                    done
                  ) f#deleters
                end
              done
            ) f#producers;
            end
          ) pdata#goal;
  (* Link protection for both special actions Init and Goal *)
          Array.iter (fun f -> if (f_exists f pdata#goal) && ((Array.length f#deleters) <> 0) then begin
                  Array.iter (fun c ->
                    for k = 1 to rpg_max_level do
                    if (c#level <= k) && (c#maxlevel >= k) then begin
                    nbc <- succ nbc;
                    let cdel_tdata = (get_f_timedata f c#idel) in
                    let ((ft_cdel_left,closed_cdel_left),(ft_cdel_right,closed_cdel_right)) =
                      match cdel_tdata#code with
                       | 0 -> (((fst cdel_tdata#timeset),cdel_tdata#closed_left),
                               ((snd cdel_tdata#timeset),cdel_tdata#closed_right))
                       | 1 -> (((fst cdel_tdata#timeset),cdel_tdata#closed_left),
                               ((snd cdel_tdata#timeset),cdel_tdata#closed_right))
                       | _ -> ((0.0,true),(0.0,true))
                    in
                    begin
                      smtwrite (Printf.sprintf "(or (not Link_Init.%s.Goal) (not %s%d) " f#to_string c#to_string k);
(*Utils.print "Gestion du conflit [Init](0)->%s->(0)[Goal] et [%s%d](%d)->not_%s\n" f#to_string c#to_string k ft_cdel f#to_string;*)
                      let (comp1,comp2) = if closed_cdel_right then ("<",">") else ("<=",">=") in
                      if (ft_cdel_right < 0.0) then
                       smtwrite (Printf.sprintf "(%s (- t_%s%d t_Init) %s) " comp1 c#to_string k (encode_float (-. ft_cdel_right)))
                      else
                       smtwrite (Printf.sprintf "(%s (- t_Init t_%s%d) %s) " comp2 c#to_string k (encode_float ft_cdel_right));
                      let (comp1,comp2) = if closed_cdel_left then (">","<") else (">=","<=") in
                      if (ft_cdel_left < 0.0) then
                       smtwrite (Printf.sprintf "(%s (- t_%s%d t_Goal) %s))\n" comp1 c#to_string k (encode_float (-. ft_cdel_left)))
                      else
                       smtwrite (Printf.sprintf "(%s (- t_Goal t_%s%d) %s))\n" comp2 c#to_string k (encode_float ft_cdel_left));
                    end end
                    done
                  ) f#deleters
                end
          ) pdata#init_state;


  (* Negative effects separation for user defined actions *)
      Array.iter (fun f -> if ((Array.length f#producers) <> 0) && ((Array.length f#deleters) <> 0) then for i = 1 to rpg_max_level do begin
            Array.iter (fun a ->
                if (a#level <= i) && (a#maxlevel >= i) then begin
                  for k = 1 to rpg_max_level do
                  Array.iter (fun c ->
                    if (c#level <= k) && (c#maxlevel >= k) && ((a <> c)||(i <> k)) then begin
                    nbc <- succ nbc;
                    let aadd_tdata = (get_f_timedata f a#iadd) in
                    let cdel_tdata = (get_f_timedata f c#idel) in
                    let ((ft_aadd_left,closed_aadd_left),(ft_aadd_right,closed_aadd_right)) =
                      match aadd_tdata#code with
                       | 0 -> (((fst aadd_tdata#timeset),aadd_tdata#closed_left),
                               ((snd aadd_tdata#timeset),aadd_tdata#closed_right))
                       | 1 -> (((fst aadd_tdata#timeset),aadd_tdata#closed_left),
                               ((snd aadd_tdata#timeset),aadd_tdata#closed_right))
                       | _ -> ((0.0,true),(0.0,true))
                    in
                    let ((ft_cdel_left,closed_cdel_left),(ft_cdel_right,closed_cdel_right)) =
                      match cdel_tdata#code with
                       | 0 -> (((fst cdel_tdata#timeset),cdel_tdata#closed_left),
                               ((snd cdel_tdata#timeset),cdel_tdata#closed_right))
                       | 1 -> (((fst cdel_tdata#timeset),cdel_tdata#closed_left),
                               ((snd cdel_tdata#timeset),cdel_tdata#closed_right))
                       | _ -> ((0.0,true),(0.0,true))
                    in
                    begin
                      smtwrite (Printf.sprintf "(or (not %s%d) (not %s%d) " a#to_string i c#to_string k);
                      let (comp1,comp2) = if closed_aadd_left && closed_cdel_right then ("<",">") else ("<=",">=") in
                      if (ft_cdel_right < ft_aadd_left) then
                       smtwrite (Printf.sprintf "(%s (- t_%s%d t_%s%d) %s) " comp1 c#to_string k a#to_string i (encode_float (ft_aadd_left -. ft_cdel_right)))
                      else
                       smtwrite (Printf.sprintf "(%s (- t_%s%d t_%s%d) %s) " comp2 a#to_string i c#to_string k (encode_float (ft_cdel_right -. ft_aadd_left)));
                      let (comp1,comp2) = if closed_aadd_right && closed_cdel_left then (">","<") else (">=","<=") in
                      if (ft_cdel_left < ft_aadd_right) then
                       smtwrite (Printf.sprintf "(%s (- t_%s%d t_%s%d) %s))\n" comp1 c#to_string k a#to_string i (encode_float (ft_aadd_right -. ft_cdel_left)))
                      else
                       smtwrite (Printf.sprintf "(%s (- t_%s%d t_%s%d) %s))\n" comp2 a#to_string i c#to_string k (encode_float (ft_cdel_left -. ft_aadd_right)));
                    end

(*                    if (ft_cdel < ft_aadd) then
                      smtwrite (Printf.sprintf "(or (not %s%d) (not %s%d) (distinct (- t_%s%d t_%s%d) %d))\n" a#to_string i c#to_string k c#to_string k a#to_string i (ft_aadd - ft_cdel))
                    else
                      smtwrite (Printf.sprintf "(or (not %s%d) (not %s%d) (distinct (- t_%s%d t_%s%d) %d))\n" a#to_string i c#to_string k a#to_string i c#to_string k (ft_cdel - ft_aadd))
*)
                    end
                  ) f#deleters
                done end
            ) f#producers;
            end done
      ) pdata#fluents;

(* Negative effects separation for special action Init *)
      Array.iter (fun f -> if ((Array.length f#deleters) <> 0) then begin
                  for k = 1 to rpg_max_level do
                  Array.iter (fun c ->
                    if (c#level <= k) && (c#maxlevel >= k) then begin
                    nbc <- succ nbc;
                    let cdel_tdata = (get_f_timedata f c#idel) in
                    let ((ft_cdel_left,closed_cdel_left),(ft_cdel_right,closed_cdel_right)) =
                      match cdel_tdata#code with
                       | 0 -> (((fst cdel_tdata#timeset),cdel_tdata#closed_left),
                               ((snd cdel_tdata#timeset),cdel_tdata#closed_right))
                       | 1 -> (((fst cdel_tdata#timeset),cdel_tdata#closed_left),
                               ((snd cdel_tdata#timeset),cdel_tdata#closed_right))
                       | _ -> ((0.0,true),(0.0,true))
                    in
                    begin
 (* Utils.print "Neg_Eff_Sep : Init -> %s and %s%d -> not_%s\n" f#to_string c#to_string k f#to_string; *)
                      smtwrite (Printf.sprintf "(or (not Init) (not %s%d) " c#to_string k);
                      let (comp1,comp2) = if closed_cdel_right then ("<",">") else ("<=",">=") in
                      if (ft_cdel_right < 0.0) then
                       smtwrite (Printf.sprintf "(%s (- t_%s%d t_Init) %s) " comp1 c#to_string k (encode_float (-. ft_cdel_right)))
                      else
                       smtwrite (Printf.sprintf "(%s (- t_Init t_%s%d) %s) " comp2 c#to_string k (encode_float ft_cdel_right));
                      let (comp1,comp2) = if closed_cdel_left then (">","<") else (">=","<=") in
                      if (ft_cdel_left < 0.0) then
                       smtwrite (Printf.sprintf "(%s (- t_%s%d t_Init) %s))\n" comp1 c#to_string k (encode_float (-. ft_cdel_left)))
                      else
                       smtwrite (Printf.sprintf "(%s (- t_Init t_%s%d) %s))\n" comp2 c#to_string k (encode_float ft_cdel_left));
                    end
                    end
                  ) f#deleters
                done end
      ) pdata#init_state;


(* Encoding rule n°5 : plan boundaries  *)

smtwrite "(>= t_Goal t_Init)\n";
nbc <- succ nbc;
for i = 1 to rpg_max_level do
Array.iter (fun a -> if (a#level <= i) && (a#maxlevel >= i) then begin nbc <- nbc + 2 ; (*a#set_n i nb ;*) let last_effect_time = a#duration (* a modifier pour prendre en compte les effets hors intervalle *) in smtwrite (Printf.sprintf "(or (not %s%i) (>= t_%s%i t_Init))\n(or (not %s%i) (>= (- t_Goal t_%s%i) %s))\n" a#to_string i a#to_string i a#to_string i a#to_string i (encode_float last_effect_time)) end) pdata#actions ;
done ;


Utils.print "  Number of clauses : %d\n" nbc;

smtwrite "))\n";
solver#close_smtwrite;
solved <- solver#launch; (* SOLVER TEST : *) (* if rpg_max_level > 2 then solved <- true; *)

(* TEST STOP 1 : *) (*solved <- true;*)

end; (* End search if current maxlevel >= minlevel *)

if solved then begin
 Utils.print "Floating plan found at level %d (SAT).\n" rpg_max_level;
end else begin
 if setminlevel <= rpg_max_level then Utils.print "No floating plan (UNSAT).\n";
    let expand_rpg = (* Expanding the Temporal Relaxed Planning Graph to next level *)
      Utils.print "Expanding the Temporal Relaxed Planning Graph from level %d to level %d ...\n" rpg_max_level (succ rpg_max_level);
        Array.iter (fun a -> (if (a#level > rpg_max_level) &&
          (Array.fold_left (fun b prec -> b &&
            Array.fold_left (fun c f -> c ||
              ((f#atom#equal prec#atom) && (f#level <= rpg_max_level)))
            false pdata#fluents)
          true a#prec)
          then begin
               a#set_level (succ rpg_max_level);
               Array.iter (fun f ->
                   if (f#level > rpg_max_level) && (List.exists (fun add ->
                                 (f#atom#equal add#atom))
                                 (Array.to_list a#add))
                   then f#set_level (succ rpg_max_level))
               pdata#fluents;
               Array.iter (fun f ->
                   if (f#neglevel > rpg_max_level) && (List.exists (fun del ->
                                 (f#atom#equal del#atom))
                                 (Array.to_list a#del))
                   then f#set_neglevel (succ rpg_max_level))
               pdata#fluents;
               end))
          pdata#actions;
      rpg_max_level <- succ rpg_max_level;
 in
 expand_rpg;
end;

done

(*Utils.print "\ndone.\n"*)
(*
      Utils.eprint "Number of clauses : %i\n" (nbc - 1)
*)
end
