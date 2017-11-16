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
    let bs = Bytes.of_string ("F_" ^ atom#pred#to_string ^ 
    (if atom#nb_terms = 0 then "" else "_" ^  (Utils.string_of_array "_" Utils.to_string atom#terms)))
 (*   ^ "[" ^ (string_of_int (fst atom#timeset)) ^ ";" ^ (string_of_int (snd atom#timeset)) ^ "]" *) in
    for i = 0 to (Bytes.length bs) - 1 do
     if (Bytes.get bs i) = '-' then (Bytes.set bs i '_');
    done;
    Bytes.to_string bs

  val mutable agenda_pos = []
  val mutable agenda_neg = []
  val mutable neglevel = max_int
  method neglevel = neglevel
  method set_neglevel nneglevel = neglevel <- nneglevel

  method to_string = string
end


and action name params duration quality prec nprec add del =
object (self)
  inherit node_common
  inherit [fluent] Node.action name params duration quality prec nprec add del

  val mutable ident_num = 0
  
  val mutable maxlevel = -1
  
  method ident_num = ident_num
  method set_ident_num idn = ident_num <- idn
  
  method maxlevel = maxlevel
  method set_maxlevel maxl = maxlevel <- maxl


  val string =
    let bs = Bytes.of_string ("A_" ^ name ^ if Array.length params = 0 then "" else "_" ^ (Utils.string_of_array "_" Utils.to_string params)) in
    for i = 0 to (Bytes.length bs) - 1 do
     if (Bytes.get bs i) = '-' then (Bytes.set bs i '_');
    done;
    Bytes.to_string bs

  method to_string = string

  method presentation_string = "(" ^ name ^ if Array.length params = 0 then ")" else " " ^ (Utils.string_of_array " " Utils.to_string params) ^ ")"

  method to_complete_string = 
  let changedash s = (String.map (fun c -> if c=='-' then '_' else c) s) in
    let string_of_fluent_array fluents =
      (String.map (fun c -> if c=='-' then '_' else c) (Utils.string_of_array "," Utils.to_string fluents)) in
      "$Cond(" ^ (changedash string) ^ ") = [" ^ string_of_fluent_array prec ^ "]\n" ^
      "$Add(" ^ (changedash string) ^ ") = [" ^ string_of_fluent_array add ^ "]\n" ^
      "$Del(" ^ (changedash string) ^ ") = [" ^ string_of_fluent_array del ^ "]\n" (* ^
      "$minlevel(" ^ (changedash string) ^ ") = " ^ (string_of_int (self)#level) ^ "\n" ^
      "$maxlevel(" ^ (changedash string) ^ ") = " ^ (string_of_int (self)#maxlevel) ^ "\n" *)

(*
  method to_complete_string = 
    let string_of_fluent_array fluents =
      Utils.string_of_array "#" Utils.to_string fluents in
      "s$Prec$" ^ string ^ "#" ^ string_of_fluent_array prec ^ "#\n" ^
      "s$Add$" ^ string ^ "#" ^ string_of_fluent_array add ^ "#\n" ^
      "s$Del$" ^ string ^ "#" ^ string_of_fluent_array del ^ "#\n" *)
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


let positive_print_int i node = Utils.print "%i " (node#n i)
let positive_print_string i node = Utils.print "%s(%i) " node#to_string i
let negative_print_int i node = Utils.print "-%i " (node#n i)
let negative_print_string i node = Utils.print "-%s(%i) " node#to_string i
let endl_int () = Utils.print "0\n"
let endl_string () = Utils.print "\n"



class t (problem:string) (domain:string) (options:string) (encoding : int) (solvernum : int) (incrmode : int) =
object (self)
  inherit [fluent, action, plan] PlanningData.t problem domain "" as pdata
  inherit [fluent, action, plan] tsp_common

  (* val mutable solver = (new Smtsolver.t) *)
  val debug_mode = false
  val mutable solved = false
  val mutable nb = 0
  val mutable nbc = 0
  val mutable rpg_max_level = 0
  val mutable plan = []
  val mutable goals = []
  val mutable search_level = 0
val mutable depth_counter = ref 0

  method print_statistics = ()
  method run = self#plan_fail

  method virtual create_action : string -> Symb.constant array -> float -> int -> ('fluent*Timedata.t) array -> ('fluent*Timedata.t) array -> ('fluent*Timedata.t) array -> ('fluent*Timedata.t) array -> 'action

(*  method search =
    let (search_time,_) = Utils.my_time "Searching plan (TLP-GP algorithm)" (fun () -> self#notimed_search) in
      Utils.eprint "Total search time : %.2f\n" search_time *)

  method search = (* notimed_search *)

    (* Utils.print "Searching plan (TLP-GP algorithm) .....\n"; *)

    let get_f_time_bound (f : 'fluent) (a_iset : (('fluent*Timedata.t) array)) = 
      Array.fold_left (fun t ((fluent,timedata) : ('fluent*Timedata.t)) ->
        if (fluent#atom#equal f#atom)
        then (timedata#closed_left,timedata#closed_right)
        else t
      ) (true,true) a_iset
    in

    let get_f_timedata_code (f : 'fluent) (a_iset : (('fluent*Timedata.t) array)) = 
      Array.fold_left (fun t ((fluent,timedata) : ('fluent*Timedata.t)) ->
        if (fluent#atom#equal f#atom)
        then timedata#code
        else t
      ) (-1) a_iset
    in

    let get_f_time f a_iset = 
      Array.fold_left (fun t x -> if ((fst x)#atom#equal f#atom) then (snd x)#timeset else t) (-1.0,-1.0) a_iset
    in
    (*let get_f f a_iset =
      Array.fold_left (fun c x -> if ((fst x)#atom#equal f#atom) then (fst x) else c) f a_iset
    in*)

flush stdout; flush stderr;
Utils.eprint "Select TouIST\n";
flush stdout; flush stderr;
let returncode = (Sys.command "touist --version") in
flush stdout; flush stderr;
if returncode == 0 then Utils.eprint "\n"
else begin Utils.eprint "[Error %d] please install TouIST with : brew install touist\n" returncode; exit returncode; end;

let stringrules = match encoding with
  | 0 -> "[QBF] Explanatory Frame-axioms"
  | 1 -> "[QBF] No-ops"
  | 2 -> "[QBF] Explanatory Frame-axioms NFLA"
  | 3 -> "[QBF] Open Conditions"
  | 100 -> "[SAT] Explanatory Frame-axioms"
  | 103 -> "[SAT] Open Conditions"
  | 203 -> "[SMT QF_RDL] Open Conditions (temporal)"
in
Utils.eprint "Select [LANGUAGE] EncodingRules: %s\n\n" stringrules;

let solvername = match solvernum with
  | 0 -> if encoding<100 then "DepQBF" else if encoding<200 then "MiniSat" else "Yices"
  | 1 -> "RAReQS"
  | 2 -> "CAQE"
  | 3 -> "Qute"
  | 101 -> "Glucose"
  | 102 -> "Glucose (multicore)"
  | 103 -> "PicoSat"
  | 104 -> "Lingeling"
in
Utils.eprint "Select %s SOLVER: %s\n\n" (if encoding<100 then "QBF" else if encoding<200 then "SAT" else "SMT") solvername;
    
(** NO GRAPH for QBFPLAN **)    
(***    
    let current_level = ref 0 in
    let build_rpg = (* Building the Temporal Relaxed Planning Graph *)
      Utils.eprint "Building the Temporal Relaxed Planning Graph ...\n";
      Array.iter (fun f ->
                   (if (List.exists (fun p ->
                                        (p#atom#equal f#atom))
                        (Array.to_list pdata#init_state))
                    then f#set_level 0))
      pdata#fluents;
      while (not (!current_level>=gpminlevel && (List.for_all (fun p -> 
                    (List.exists (fun f ->
                       (f#atom#equal p#atom) && (f#level <= !current_level))
                    (Array.to_list pdata#fluents)))
                  (Array.to_list pdata#goal))))
      && (!current_level<=1024) do
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
 
       current_level := succ !current_level
      done;
      rpg_max_level <- !current_level;
Utils.eprint "Goal found at level %d.\n" !current_level;
(*Array.iter (fun a -> Utils.print "%s(level[%d])\n" a#to_string a#level) pdata#actions;
Array.iter (fun f -> Utils.print "%s(level[%d],neglevel[%d])\n" f#to_istring f#level f#neglevel) pdata#fluents;*)

    in

      build_rpg;



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

***)
(* END NO GRAPH *)



(* QBFPLAN-SATPLAN / options : (0) QBF-EFA (1) QBF-NOOP (2) QBF-EFA-NFLA (100) SAT-EFA *)

if encoding == 0 then
begin (* option QBF-EFA Nodes-Fluents-Actions Leafs-Fluents-Actions *)
Utils.eprint "Searching solution with QBFPLAN (Explanatory Frame-Axioms)...\n\n";
end else if encoding == 1 then
begin (* option QBF-NOOP *)
Utils.eprint "Searching solution with QBFPLAN (Noop Actions [Cashmore et al., 2012])...\n\n";
end else if encoding == 2 then
begin (* option QBF-EFA-NFLA Nodes-Fluents Leafs-Actions *)
Utils.eprint "Searching solution with QBFPLAN (Explanatory Frame-Axioms, NFLA)...\n\n";
end else if encoding == 100 then
begin (* option SAT-EFA *)
Utils.eprint "Searching solution with SATPLAN (Explanatory Frame-Axioms)...\n\n";
end;
ignore (Sys.command "rm solvedata/*.txt");

(* Génération des quantificateurs *)

let genquantifiers n nexistsb =
if encoding == 0 then (* QBF-EFA *)
begin
 let quantifiersfile = Unix.openfile "solvedata/in.quantifiers.txt" [Unix.O_TRUNC;Unix.O_CREAT;Unix.O_WRONLY] 0o640 in
 let quantifierswrite s =
   ignore (Unix.write quantifiersfile (Bytes.of_string s) 0 (String.length s)) in
  for i = n downto nexistsb do
    quantifierswrite (Printf.sprintf "exists $A(%d) for $A in $O:\nexists $f(%d) for $f in $F:\nexists b(%d):\n" i i i);
  done;
  for i = nexistsb - 1 downto 1 do
    quantifierswrite (Printf.sprintf "exists $A(%d) for $A in $O:\nexists $f(%d) for $f in $F:\nforall b(%d):\n" i i i);
  done;
  quantifierswrite "exists $A(0) for $A in $O:\nexists $f(0) for $f in $F:\n";
 Unix.close quantifiersfile;
end
else if encoding == 1 then (* QBF-NOOP *)
begin
 let quantifiersfile = Unix.openfile "solvedata/in.quantifiers.txt" [Unix.O_TRUNC;Unix.O_CREAT;Unix.O_WRONLY] 0o640 in
 let quantifierswrite s =
   ignore (Unix.write quantifiersfile (Bytes.of_string s) 0 (String.length s)) in
  for i = n downto nexistsb do
    quantifierswrite (Printf.sprintf "exists $A(%d) for $A in $O:\nexists Noop($f,%d) for $f in $F:\nexists b(%d):\n" i i i);
  done;
  for i = nexistsb - 1 downto 1 do
    quantifierswrite (Printf.sprintf "exists $A(%d) for $A in $O:\nexists Noop($f,%d) for $f in $F:\nforall b(%d):\n" i i i);
  done;
  quantifierswrite "exists $A(0) for $A in $O:\nexists Noop($f,0) for $f in $F:\n";
 Unix.close quantifiersfile;
 end
else if encoding == 2 then (* QBF-EFA-NFLA *)
begin
 let quantifiersfile = Unix.openfile "solvedata/in.quantifiers.txt" [Unix.O_TRUNC;Unix.O_CREAT;Unix.O_WRONLY] 0o640 in
 let quantifierswrite s =
   ignore (Unix.write quantifiersfile (Bytes.of_string s) 0 (String.length s)) in
  for i = n downto nexistsb do
    quantifierswrite (Printf.sprintf "exists $f(%d) for $f in $F:\nexists b(%d):\n" i i);
  done;
  for i = nexistsb - 1 downto 1 do
    quantifierswrite (Printf.sprintf "exists $f(%d) for $f in $F:\nforall b(%d):\n" i i);
  done;
  quantifierswrite "exists $A(0) for $A in $O:\n";
 Unix.close quantifiersfile;
end
else if encoding == 3 then (* QBF-OPEN *)
begin
 let quantifiersfile = Unix.openfile "solvedata/in.quantifiers.txt" [Unix.O_TRUNC;Unix.O_CREAT;Unix.O_WRONLY] 0o640 in
 let quantifierswrite s =
   ignore (Unix.write quantifiersfile (Bytes.of_string s) 0 (String.length s)) in
  for i = n downto nexistsb do
    quantifierswrite (Printf.sprintf "exists $A(%d) for $A in $O:\nexists open($f,%d) for $f in $F:\nexists b(%d):\n" i i i);
  done;
  for i = nexistsb - 1 downto 1 do
    quantifierswrite (Printf.sprintf "exists $A(%d) for $A in $O:\nexists open($f,%d) for $f in $F:\nforall b(%d):\n" i i i);
  done;
  quantifierswrite "exists $A(0) for $A in $O:\nexists open($f,0) for $f in $F:\n";
 Unix.close quantifiersfile;
 end;
in

(* Génération des formules non quantifiées *)

if encoding == 0 then (* option QBF-EFA *)
begin
 let qfformulafile = Unix.openfile "solvedata/in.qfformula.txt" [Unix.O_TRUNC;Unix.O_CREAT;Unix.O_WRONLY] 0o640 in
 let qfformulawrite s =
   ignore (Unix.write qfformulafile (Bytes.of_string s) 0 (String.length s)) in  
    let efaprint fsrc fdst adddel adst bi bj = "\\\and\nbigand $i in [1..$depth]:\n  bigand $f in $F:\n      " ^ fsrc ^ " or " ^ fdst ^ "\n      or bigor $A in $O when $f in $" ^ adddel ^ "($A):\n         " ^ adst ^ "\n      end\n      or " ^ bi ^ "\n      or bigor $j in [1..$i-1]:\n         " ^ bj ^ "\n      end\n  end\nend\n" in
(**     qfformulawrite "\n;; (QBF-EFA1.1) Etat initial vérifié pour la feuille la plus à gauche\n\nbigand $f in $I:\n  $f(0)\n  or bigor $i in [1..$depth]:\n    b($i)\n  end\nend\n\nand\nbigand $f in ($F diff $I):\n  not $f(0)\n  or bigor $i in [1..$depth]:\n    b($i)\n  end\nend\n"; **)
     qfformulawrite "\n;; (QBF-EFA1.2) But vérifié pour la feuille la plus à droite (propagation si but atteint avant)\n\nbigand $f in $G:\n  $f(0)\n  or bigor $i in [1..$depth]:\n    not b($i)\n  end\nend\n";
    qfformulawrite "\n;; (QBF-EFA2.0) Conditions des actions de la première étape\n\n\\\and bigand $A in $O when not ($Cond($A) subset $I):\n  not $A(0)\n  or bigor $i in [1..$depth]:\n    b($i)\n  end\nend\n";
    qfformulawrite "\n;; (QBF-EFA2.1) Effets des actions\n\n\\\and\nbigand $i in [0..$depth]:\n  bigand $A in $O:\n    bigand $f in $Add($A):\n      not $A($i) or $f($i)\n    end\n    and\n    bigand $f in $Del($A):\n      not $A($i) or not $f($i)\n    end\n  end\nend\n";
(**     qfformulawrite "\nand\nbigand $A in $O:\n  not $A(0)\n  or bigor $i in [1..$depth]:\n    b($i)\n  end\nend\n"; **)
    qfformulawrite "\n;; (QBF-EFA2.2) Conditions des actions (transition feuille -> noeud)\n\n\\\and\nbigand $i in [1..$depth]:\n  bigand $A in $O:\n    bigand $f in $Cond($A):\n      not $A($i)\n      or $f(0)\n      or b($i)\n      or bigor $j in [1..$i-1]:\n         not b($j)\n      end\n    end\n  end\nend\n";
    qfformulawrite "\n;; (QBF-EFA2.3) Conditions des actions (transition noeud -> feuille)\n\n\\\and\nbigand $i in [1..$depth]:\n  bigand $A in $O:\n    bigand $f in $Cond($A):\n      not $A(0)\n      or $f($i)\n      or not b($i)\n      or bigor $j in [1..$i-1]:\n         b($j)\n      end\n    end\n  end\nend\n";
    qfformulawrite "\n;; (QBF-EFA3.1.0) Frames-Axiomes d'ajout (état initial)\n\n\\\and bigand $f in ($F diff $I):\n  not $f(0)\n  or bigor $A in $O when ($f in $Add($A)) and ($Cond($A) subset $I):\n     $A(0)\n  end\n  or bigor $i in [1..$depth]:\n    b($i)\n  end\nend\n";
    qfformulawrite (Printf.sprintf "\n;; (QBF-EFA3.1.1) Frames-axiomes d'ajout (transition feuille -> noeud)\n\n%s" (efaprint "$f(0)" "not $f($i)" "Add" "$A($i)" "b($i)" "not b($j)"));
    qfformulawrite (Printf.sprintf "\n;; (QBF-EFA3.1.2) Frames-axiomes d'ajout (transition noeud -> feuille)\n\n%s" (efaprint "$f($i)" "not $f(0)" "Add" "$A(0)" "not b($i)" "b($j)"));
    qfformulawrite "\n;; (QBF-EFA3.2.0) Frames-Axiomes de retrait (état initial)\n\n\\\and bigand $f in $I:\n  $f(0)\n  or bigor $A in $O when ($f in $Del($A)) and ($Cond($A) subset $I):\n     $A(0)\n  end\n  or bigor $i in [1..$depth]:\n    b($i)\n  end\nend\n";
    qfformulawrite (Printf.sprintf "\n;; (QBF-EFA3.2.1) Frames-axiomes de retrait (transition feuille -> noeud)\n\n%s" (efaprint "not $f(0)" "$f($i)" "Del" "$A($i)" "b($i)" "not b($j)"));
    qfformulawrite (Printf.sprintf "\n;; (QBF-EFA3.2.2) Frames-axiomes de retrait (transition noeud -> feuille)\n\n%s" (efaprint "not $f($i)" "$f(0)" "Del" "$A(0)" "not b($i)" "b($j)"));
    qfformulawrite "\n;; (QBF-EFA4) Mutex (Forall-step semantics)\n\n\\\and\nbigand $i in [0..$depth]:\n  bigand $A in $O:\n    bigand $f in $Cond($A):\n      bigand $B in $O when $A!=$B and $f in $Del($B):\n        not $A($i) or not $B($i)\n      end\n    end\n  end\nend\n";
(*    qfformulawrite "\n;; (QBF-EFA4bis) Mutex (Exists-step semantics)\n\n\\\and\nbigand $i in [0..$depth]:\n  bigand $A in $O:\n    bigand $B in $O when $A!=$B\n                         and inter($Cond($A),$Del($B))!=[]\n                         and inter($Cond($B),$Del($A))!=[]\n                         and inter($Add($A),$Del($B))==[]\n                         and inter($Add($B),$Del($A))==[]:\n      not $A($i) or not $B($i)\n    end\n  end\nend\n"; *)
 Unix.close qfformulafile;
end else if encoding == 1 then (* option QBF-NOOP *)
begin
 let qfformulafile = Unix.openfile "solvedata/in.qfformula.txt" [Unix.O_TRUNC;Unix.O_CREAT;Unix.O_WRONLY] 0o640 in
 let qfformulawrite s =
   ignore (Unix.write qfformulafile (Bytes.of_string s) 0 (String.length s)) in
    qfformulawrite "\n;; (QBF-NOOP1.1) Etat initial vérifié pour la feuille la plus à gauche\n\nbigand $A in $O when not ($Cond($A) subset $I):\n  not $A(0)\n  or (bigor $i in [1..$depth]:\n    b($i)\n  end)\nend\n";
    qfformulawrite "\n\\\and bigand $f in ($F diff $I):\n  not Noop($f,0)\n  or (bigor $i in [1..$depth]:\n    b($i)\n  end)\nend\n";
    qfformulawrite "\n;; (QBF-NOOP1.2) But vérifié pour la feuille la plus à droite\n\n\\\and bigand $f in $G:\n  (bigor $A in $O when $f in $Add($A):\n    $A(0)\n  end)\n  or Noop($f,0)\n  or (bigor $i in [1..$depth]:\n    not b($i)\n  end)\nend\n";
    qfformulawrite ";; (QBF-NOOP2.1) Liens causaux : Feuille -> Noeud\n\n\\\and bigand $i in [1..$depth]:\n  bigand $A in $O:\n    bigand $f in $Cond($A):\n      not $A($i)\n      or (bigor $B in $O when $f in $Add($B):\n        $B(0)\n      end)\n      or Noop($f,0)\n      or b($i)\n      or (bigor $j in [1..$i-1]:\n        not b($j)\n      end)\n    end\n  end\nend\n";
    qfformulawrite "\\\and bigand $i in [1..$depth]:\n  bigand $f in $F:\n    not Noop($f,$i)\n    or (bigor $B in $O when $f in $Add($B):\n      $B(0)\n    end)\n    or Noop($f,0)\n    or b($i)\n    or (bigor $j in [1..$i-1]:\n      not b($j)\n    end)\n  end\nend\n";
    qfformulawrite ";; (QBF-NOOP2.2) Liens causaux : Noeud -> Feuille\n\n\\\and bigand $i in [1..$depth]:\n  bigand $A in $O:\n    bigand $f in $Cond($A):\n      not $A(0)\n      or (bigor $B in $O when $f in $Add($B):\n        $B($i)\n      end)\n      or Noop($f,$i)\n      or not b($i)\n      or (bigor $j in [1..$i-1]:\n        b($j)\n      end)\n    end\n  end\nend\n";
    qfformulawrite "\n\\\and bigand $i in [1..$depth]:\n  bigand $f in $F:\n    not Noop($f,0)\n    or (bigor $B in $O when $f in $Add($B):\n      $B($i)\n    end)\n    or Noop($f,$i)\n    or not b($i)\n    or (bigor $j in [1..$i-1]:\n      b($j)\n    end)\n  end\nend\n";
    qfformulawrite ";; (QBF-NOOP3) Mutex\n\n\\\and bigand $i in [0..$depth]:\n  bigand $A in $O:\n    bigand $f in ($Cond($A) union $Add($A)):\n      bigand $B in $O when ($A != $B) and ($f in $Del($B)):\n        (not $A($i) or not $B($i))\n      end\n    end\n  end\nend\n";
    qfformulawrite "\n\\\and bigand $i in [0..$depth]:\n  bigand $f in $F:\n    bigand $A in $O when ($f in $Del($A)):\n      (not Noop($f,$i) or not $A($i))\n    end\n  end\nend\n";    
 Unix.close qfformulafile;
end else if encoding == 3 then (* option QBF-OPEN *)
begin
 let qfformulafile = Unix.openfile "solvedata/in.qfformula.txt" [Unix.O_TRUNC;Unix.O_CREAT;Unix.O_WRONLY] 0o640 in
 let qfformulawrite s =
   ignore (Unix.write qfformulafile (Bytes.of_string s) 0 (String.length s)) in
    qfformulawrite "\n;; Ouverture des conditions des actions\n\nbigand $i in [0..$depth]:\n  bigand $A in $O:\n    $A($i) => bigand $f in $Cond($A): open($f,$i) end\n  end\nend\n";
    qfformulawrite "\n;; Ouverture des buts\n\n\\\and (bigand $i in [1..$depth]:\n  b($i) end => bigand $f in $G:\n    open($f,0)\n    or bigor $A in $O when $f in $Add($A):\n      $A(0)\n    end\n  end)\n";
    qfformulawrite "\n;; Fermeture des conditions qui ne sont pas dans I\n\n\\\and (bigand $i in [1..$depth]:\n  not b($i) end => bigand $f in $F diff $I: not open($f,0) end)\n";
    qfformulawrite "\n;; Fermeture des conditions ouvertes\n\n\\\and bigand $i in [1..$depth]:\n  bigand $f in $F:\n    (open($f,$i)\n    and not b($i) \n    and bigand $j in [1..$i-1]:\n      b($j)\n    end)\n    => (open($f,0)\n    or bigor $A in $O when $f in $Add($A):\n      $A(0)\n    end)\n  end\nend\n";
    qfformulawrite "\n\n\\\and bigand $i in [1..$depth]:\n  bigand $f in $F:\n    (open($f,0)\n    and b($i) \n    and bigand $j in [1..$i-1]:\n      not b($j)\n    end)\n    => (open($f,$i)\n    or bigor $A in $O when $f in $Add($A):\n      $A($i)\n    end)\n  end\nend\n";
    qfformulawrite "\n;; Protection des conditions ouvertes\n\n\\\and bigand $i in [1..$depth]:\n  bigand $f in $F:\n    (open($f,$i)\n    and not b($i) \n    and bigand $j in [1..$i-1]:\n      b($j)\n    end)\n    => bigand $A in $O when $f in $Del($A):\n      not $A(0)\n    end\n  end\nend\n";
    qfformulawrite "\n\n\\\and bigand $i in [1..$depth]:\n  bigand $f in $F:\n    (open($f,0)\n    and b($i) \n    and bigand $j in [1..$i-1]:\n      not b($j)\n    end)\n    => bigand $A in $O when $f in $Del($A):\n      not $A($i)\n    end\n  end\nend\n";
    qfformulawrite "\n;; Mutex\n\n\\\and\nbigand $i in [0..$depth]:\n  bigand $A in $O:\n    bigand $f in ($Add($A) union $Cond($A)):\n      bigand $B in $O when $A!=$B and $f in $Del($B):\n        not $A($i) or not $B($i)\n      end\n    end\n  end\nend\n";
 Unix.close qfformulafile;
end else if encoding == 100 then (* option SAT-EFA *)
begin
 let qfformulafile = Unix.openfile "solvedata/in.qfformula.txt" [Unix.O_TRUNC;Unix.O_CREAT;Unix.O_WRONLY] 0o640 in
 let qfformulawrite s =
   ignore (Unix.write qfformulafile (Bytes.of_string s) 0 (String.length s)) in 
    qfformulawrite "\n;; (SAT-EFA1) Etat initial et but\n\nbigand $f in $I: $f(0) end\nbigand $f in ($F diff $I): not $f(0) end\nbigand $f in $G: $f($length) end\n";
    qfformulawrite "\n;; (SAT-EFA2) Conditions et effets des actions\n\nbigand $i in [1..$length]:\n  bigand $a in $O:\n    ($a($i) =>\n      ((bigand $f in $Cond($a): $f($i-1) end)\n        and\n        (bigand $f in $Add($a): $f($i) end)\n        and\n        (bigand $f in $Del($a): (not $f($i)) end)))\n  end\nend\n";
    qfformulawrite "\n;; (SAT-EFA3.1) Frames-axiomes de retrait\n\nbigand $i in [1..$length]:\n  bigand $f in $F:\n    ($f($i-1) and not $f($i))\n    => (bigor $a in $O when $f in $Del($a): $a($i) end)\n  end\nend\n";
    qfformulawrite "\n;; (SAT-EFA3.2) Frames-axiomes d'ajout\n\nbigand $i in [1..$length]:\n  bigand $f in $F:\n    (not $f($i-1) and $f($i))\n    => (bigor $a in $O when $f in $Add($a): $a($i) end)\n  end\nend\n";
    qfformulawrite "\n;; (SAT-EFA4) Mutex (Forall-step semantics)\nbigand $i in [1..$length]:\n  bigand $a1 in $O:\n    bigand $f in $Cond($a1):\n      bigand $a2 in $O when ($a1 != $a2) and ($f in $Del($a2)):\n        (not $a1($i) or not $a2($i))\n      end\n    end\n  end\nend\n";
 Unix.close qfformulafile;
end else if encoding == 103 then (* option SAT-OPEN *)
begin
 let qfformulafile = Unix.openfile "solvedata/in.qfformula.txt" [Unix.O_TRUNC;Unix.O_CREAT;Unix.O_WRONLY] 0o640 in
 let qfformulawrite s =
   ignore (Unix.write qfformulafile (Bytes.of_string s) 0 (String.length s)) in 
    qfformulawrite "\n;; Ouverture des conditions des actions\n\nbigand $i in [1..$length]:\n  bigand $A in $O:\n    $A($i) => bigand $f in $Cond($A): open($f,$i) end\n  end\nend\n";
    qfformulawrite "\n;; Ouverture des buts\n\nbigand $f in $G:\n  open($f,$length)\n  or bigor $A in $O when $f in $Add($A):\n    $A($length)\n  end\nend\n";
    qfformulawrite "\n;; Fermeture des conditions qui ne sont pas dans I\n\nbigand $f in $F diff $I: not open($f,1) end\n";
    qfformulawrite "\n;; Fermeture des conditions ouvertes\n\nbigand $i in [2..$length]:\n  bigand $f in $F:\n    open($f,$i)\n    => (open($f,$i-1)\n    or bigor $A in $O when $f in $Add($A):\n      $A($i-1)\n    end)\n  end\nend\n";
    qfformulawrite "\n;; Protection des conditions ouvertes\n\nbigand $i in [2..$length]:\n  bigand $f in $F:\n    open($f,$i) => bigand $A in $O when $f in $Del($A):\n      not $A($i-1)\n    end\n  end\nend\n";
    qfformulawrite "\n;; Mutex\n\nbigand $i in [1..$length]:\n  bigand $A in $O:\n    bigand $f in ($Add($A) union $Cond($A)):\n      bigand $B in $O when $A!=$B and $f in $Del($B):\n        not $A($i) or not $B($i)\n      end\n    end\n  end\nend\n";
 Unix.close qfformulafile;
end else if encoding == 150 then (* option SELECT-SAT-EFA *)
begin
 let qfformulafile = Unix.openfile "solvedata/in.qfformula.txt" [Unix.O_TRUNC;Unix.O_CREAT;Unix.O_WRONLY] 0o640 in
 let qfformulawrite s =
   ignore (Unix.write qfformulafile (Bytes.of_string s) 0 (String.length s)) in 
    qfformulawrite "\n;; (SELECT-SAT-EFA1) Etat initial et but\n\nbigand $f in $I: $f(0) end\nbigand $f in ($F diff $I): not $f(0) end\nbigand $f in $G: $f($length) end\n";
    qfformulawrite "\n;; (SELECT-SAT-EFA2) Conditions et effets des actions\n\nbigand $i in [1..$length]:\n  bigand $a in $O:\n    ($a($i) =>\n      ((bigand $f in $Cond($a): $f($i-1) end)\n        and\n        (bigand $f in $Add($a): $f($i) end)\n        and\n        (bigand $f in $Del($a): (not $f($i)) end)))\n  end\nend\n";
    qfformulawrite "\n;; (SELECT-SAT-EFA3.1) Frames-axiomes de retrait\n\nbigand $i in [1..$length]:\n  bigand $f in $F:\n    ($f($i-1) and not $f($i))\n    => (bigor $a in $O when $f in $Del($a): $a($i) end)\n  end\nend\n";
    qfformulawrite "\n;; (SELECT-SAT-EFA3.2) Frames-axiomes d'ajout\n\nbigand $i in [1..$length]:\n  bigand $f in $F:\n    (not $f($i-1) and $f($i))\n    => (bigor $a in $O when $f in $Add($a): $a($i) end)\n  end\nend\n";
    qfformulawrite "\n;; (SELECT-SAT-EFA4) Mutex (Forall-step semantics)\nbigand $i in [1..$length]:\n  bigand $a1 in $O:\n    bigand $f in $Cond($a1):\n      bigand $a2 in $O when ($a1 != $a2) and ($f in $Del($a2)):\n        (not $a1($i) or not $a2($i))\n      end\n    end\n  end\nend\n";
 Unix.close qfformulafile;
end else if encoding == 203 then (* option SMT-OPEN *)
begin
 let qfformulafile = Unix.openfile "solvedata/in.qfformula.txt" [Unix.O_TRUNC;Unix.O_CREAT;Unix.O_WRONLY] 0o640 in
 let qfformulawrite s =
   ignore (Unix.write qfformulafile (Bytes.of_string s) 0 (String.length s)) in
    qfformulawrite "\n;; Ouverture des conditions des actions\n\nbigand $i in [1..$length]:\n  bigand $A in $O:\n    $A($i) => bigand $f in $Cond($A):\n      open($f,$i)\n      \\\and (t($A,$i) - 0.0 >= t_s_open($f,$i) - $t_cond_begin($f,$A))\n      \\\and (t($A,$i) - 0.0 <= t_e_open($f,$i) - $t_cond_begin($f,$A))\n    end\n  end\nend\n";
    qfformulawrite "\n;; Ouverture des buts\n\nbigand $f in $G:\n  open($f,$length)\n  or bigor $A in $O when $f in $Add($A):\n    $A($length)\n    \\\and (t($A,$length) - 0.0 == t_s_open($f,$length) - $t_add_begin($A,$f))\n    \\\and (t_Goal - 0.0 == t_e_open($f,$length) - 0.0)\n  end\nend\n";
    qfformulawrite "\n;; Fermeture des conditions qui ne sont pas dans I\n\nbigand $f in $F diff $I:\n  not open($f,1)\nend\n";
    qfformulawrite "\n;; Fermeture des conditions par I\n\nbigand $f in $I:\n  open($f,1) => (t_Init - 0.0 == t_s_open($f,1) - 0.0)\nend\n";
    qfformulawrite "\n;; Fermeture des conditions ouvertes\n\nbigand $i in [2..$length]:\n  bigand $f in $F:\n    open($f,$i)\n    => ((open($f,$i-1)\n        \\\and (t_s_open($f,$i-1) - 0.0 == t_s_open($f,$i) - 0.0)\n        \\\and (t_e_open($f,$i-1) - 0.0 == t_e_open($f,$i) - 0.0))\n    \\\or bigor $A in $O when $f in $Add($A):\n      $A($i-1) and (t($A,$i-1) - 0.0 == t_s_open($f,$i) - $t_add_begin($A,$f))\n    end)\n  end\nend\n";
    qfformulawrite "\n;; Mutex temporellement étendues 1 (Protection des conditions ouvertes)\n\nbigand $i in [1..$length]:\n  bigand $j in [1..$length]:\n    bigand $f in $F:\n      bigand $A in $O when $f in $Del($A):\n        (open($f,$i) and $A($j)) =>\n          ((t($A,$j) - 0.0 <  t_s_open($f,$i) - $t_del_end($A,$f))\n          \\\or (t_e_open($f,$i) - $t_del_begin($A,$f) < t($A,$j) - 0.0))\n      end\n    end\n  end\nend\n";
    qfformulawrite "\n;; Mutex temporellement étendues 2 (Effets contradictoires)\n\nbigand $i in [1..$length]:\n  bigand $j in [1..$length]:\n    bigand $A in $O:\n      bigand $f in $Add($A):\n        bigand $B in $O when (($i!=$j) or ($A!=$B)) and $f in $Del($B):\n          ($A($i) and $B($j)) =>\n            ((t($A,$i) - $t_del_begin($B,$f) < t($B,$j) - $t_add_end($A,$f))\n            \\\or (t($B,$j) - $t_add_begin($A,$f) < t($A,$i) - $t_del_end($B,$f)))\n        end\n      end\n    end\n  end\nend\n";
    qfformulawrite "\n;; Mutex temporellement étendues 3 (Condition/Effet)\n\nbigand $i in [1..$length]:\n  bigand $j in [1..$length]:\n    bigand $A in $O:\n      bigand $f in $Cond($A):\n        bigand $B in $O when (($i!=$j) or ($A!=$B)) and $f in $Del($B):\n          ($A($i) and $B($j)) =>\n            ((t($A,$i) - $t_del_begin($B,$f) < t($B,$j) - $t_cond_end($f,$A))\n            \\\or (t($B,$j) - $t_cond_begin($f,$A) < t($A,$i) - $t_del_end($B,$f)))\n        end\n      end\n    end\n  end\nend\n";
    qfformulawrite "\n;; Bornes\n(t_Init == 0.0)\nbigand $i in [1..$length]:\n  bigand $A in $O:\n    $A($i) =>\n    (bigand $f in $Cond($A):\n      (t_Init - $t_cond_begin($f,$A) <= t($A,$i) - 0.0)\n      \\\and (t_Goal - $t_cond_end($f,$A) >= t($A,$i) - 0.0)\n    end\n    \\\and bigand $f in $Add($A):\n      (t_Init - $t_add_begin($A,$f) <= t($A,$i) - 0.0)\n      \\\and (t_Goal - $t_add_end($A,$f) >= t($A,$i) - 0.0)\n    end\n    \\\and bigand $f in $Del($A):\n      (t_Init - $t_del_begin($A,$f) <= t($A,$i) - 0.0)\n      \\\and (t_Goal - $t_del_end($A,$f) >= t($A,$i) - 0.0)\n    end)\n    \\\and (not $A($i) => (t($A,$i) < t_Init - 1000.0))\n  end\nend\n";
    qfformulawrite "\n(t(A____Spy___,1) - t_Init == 1.0)\n";
 Unix.close qfformulafile;
end;


(* Génération des ensembles à partir du problème de planification *)

 let setsfile = Unix.openfile "solvedata/in.sets.txt" [Unix.O_TRUNC;Unix.O_CREAT;Unix.O_WRONLY] 0o640 in
 let setswrite s =
   ignore (Unix.write setsfile (Bytes.of_string s) 0 (String.length s)) in  
    let changedash s = (String.map (fun c -> if c=='-' then '_' else c) s) in
    let string_of_fluent_array fluents = Utils.string_of_array "," Utils.to_string fluents in
    setswrite (Printf.sprintf "\n$I = [%s]" (string_of_fluent_array self#init_state));
    setswrite (Printf.sprintf "\n\n$G = [%s]" (string_of_fluent_array self#goal));
    setswrite (Printf.sprintf "\n\n$O = [%s]" (string_of_fluent_array 
      (Array.of_list
        (Array.fold_left (fun l a -> (* if (a#level <= rpg_max_level) && (a#maxlevel >= 0) then *) a::l (* else l*)) [] self#actions)
      )
     ));
    setswrite (Printf.sprintf "\n\n$F = [%s]" (string_of_fluent_array self#fluents));
    setswrite (Printf.sprintf "\n\n$Fp = [%s]\n" (Utils.string_of_list "," Utils.to_string (Array.fold_left (fun l f -> if f#consumers <> [| |] then f::l else l) [] self#fluents)));
    setswrite (Printf.sprintf "\n$Fa = [%s]\n" (Utils.string_of_list "," Utils.to_string (Array.fold_left (fun l f -> if f#producers <> [| |] then f::l else l) [] self#fluents)));
    setswrite (Printf.sprintf "\n$Fd = [%s]\n" (Utils.string_of_list "," Utils.to_string (Array.fold_left (fun l f -> if f#deleters <> [| |] then f::l else l) [] self#fluents)));
    setswrite (Printf.sprintf "\n");
    Array.iter (fun a -> (* if (a#level <= rpg_max_level) && (a#maxlevel >= 0) then *)
     begin
      setswrite (Printf.sprintf "\n%s" a#to_complete_string);
if encoding >= 200 then begin (* SMT Temporal Planning *)
      Array.iter (fun (f,timedata) ->
          setswrite (Printf.sprintf "$t_cond_begin(%s,%s) = %f\n" (changedash f#to_string) (changedash a#to_string) (fst timedata#timeset));
          setswrite (Printf.sprintf "$t_cond_end(%s,%s) = %f\n" (changedash f#to_string) (changedash a#to_string) (snd timedata#timeset))
      ) a#iprec ;
      Array.iter (fun (f,timedata) ->
          setswrite (Printf.sprintf "$t_add_begin(%s,%s) = %f\n" (changedash a#to_string) (changedash f#to_string) (fst timedata#timeset));
          setswrite (Printf.sprintf "$t_add_end(%s,%s) = %f\n" (changedash a#to_string) (changedash f#to_string) (snd timedata#timeset))
      ) a#iadd ;
      Array.iter (fun (f,timedata) ->
          setswrite (Printf.sprintf "$t_del_begin(%s,%s) = %f\n" (changedash a#to_string) (changedash f#to_string) (fst timedata#timeset));
          setswrite (Printf.sprintf "$t_del_end(%s,%s) = %f\n" (changedash a#to_string) (changedash f#to_string) (snd timedata#timeset))
      ) a#idel ;
end (* end SMT Temporal Planning *)

     end
    ) self#actions ;
 Unix.close setsfile;



(** START QBFPLAN-SOLVE **)
if encoding < 100 then
begin


let touistsolveqbf maxdepth branchdepth addatom =
  let touistcode = ref 0 in
  genquantifiers maxdepth branchdepth;
  let atomsfiles = if branchdepth <= maxdepth then (Printf.sprintf " solvedata/in.atoms%d.txt" branchdepth) else "" in
(* Utils.eprint "TouIST solve / depth = %d / branch = %d / atomsfiles = %s / addatom = %s\n" maxdepth branchdepth atomsfiles addatom; *)
Utils.eprint "--- TouIST solve (QBF) / branch atom = %s ---\n" addatom;
flush stdout; flush stderr;
(* INERNAL SOLVER *) (* ignore (Sys.command (Printf.sprintf "(echo '$depth = %d' ; cat solvedata/in.sets.txt solvedata/in.quantifiers.txt ; cat solvedata/in.qfformula.txt%s ; echo '%s') | touist --qbf --solve - > solvedata/out.emodel.txt 2> solvedata/out.touisterr.txt" maxdepth atomsfiles addatom)); *)
(* EXTERNAL SOLVER: *)  
if solvernum == 0 then touistcode := (Sys.command (Printf.sprintf "(echo '$depth = %d' ; cat solvedata/in.sets.txt solvedata/in.quantifiers.txt ; cat solvedata/in.qfformula.txt%s ; echo '%s') | touist --qbf --solver 'depqbf --qdo --no-dynamic-nenofex' - > solvedata/out.emodel.txt 2> solvedata/out.touisterr.txt" maxdepth atomsfiles addatom));
if solvernum == 1 then touistcode := (Sys.command (Printf.sprintf "(echo '$depth = %d' ; cat solvedata/in.sets.txt solvedata/in.quantifiers.txt ; cat solvedata/in.qfformula.txt%s ; echo '%s') | touist --qbf --solver rareqs - > solvedata/out.emodel.txt 2> solvedata/out.touisterr.txt" maxdepth atomsfiles addatom));
if solvernum == 2 then touistcode := (Sys.command (Printf.sprintf "(echo '$depth = %d' ; cat solvedata/in.sets.txt solvedata/in.quantifiers.txt ; cat solvedata/in.qfformula.txt%s ; echo '%s') | touist --qbf --solver './solvers/caqe-mac --partial-assignments' - > solvedata/out.emodel.txt 2> solvedata/out.touisterr.txt" maxdepth atomsfiles addatom));
if solvernum == 3 then touistcode := (Sys.command (Printf.sprintf "(echo '$depth = %d' ; cat solvedata/in.sets.txt solvedata/in.quantifiers.txt ; cat solvedata/in.qfformula.txt%s ; echo '%s') | touist --qbf --solver 'qute --partial-certificate --prefix-mode' - > solvedata/out.emodel.txt 2> solvedata/out.touisterr.txt" maxdepth atomsfiles addatom));
(* FICHIER DEBUG: *)  ignore (Sys.command (Printf.sprintf "(echo '$depth = %d' ; cat solvedata/in.sets.txt solvedata/in.quantifiers.txt ; cat solvedata/in.qfformula.txt%s ; echo '%s') > debug.touistl" maxdepth atomsfiles addatom));
  ignore (Sys.command (Printf.sprintf "((cat solvedata/out.emodel.txt | grep '1 ' | sed -e 's/1 /and /') ; (cat solvedata/out.emodel.txt | grep '0 ' | sed -e 's/0 /and not /')) > solvedata/in.atoms%d.txt" (branchdepth - 1)));
if branchdepth <= maxdepth then ignore (Sys.command (Printf.sprintf "cat solvedata/in.atoms%d.txt | grep 'and A_' | grep '(%d)'" (branchdepth - 1) (branchdepth - 1)));
if (encoding == 1) && (branchdepth <= maxdepth) then ignore (Sys.command (Printf.sprintf "cat solvedata/in.atoms%d.txt | grep 'and A_' | grep '(%d)'" (branchdepth - 1) (branchdepth)));
for i = maxdepth downto branchdepth - 1 do
  Sys.command (Printf.sprintf "cat solvedata/out.emodel.txt | grep '? ' | grep '\\((\\|,\\)%d)'" i) |> ignore;
  if 0 = Sys.command (Printf.sprintf "cat solvedata/out.emodel.txt | grep '? ' | grep '\\((\\|,\\)%d)' | grep '\\(A_\\|F_\\)' >/dev/null" i)
  then exit 1;
  flush stdout; flush stderr;
done;
!touistcode
in

let treedepthbound = Array.length pdata#fluents
in
Utils.eprint "Maximum tree depth: |Fluents|=%d.\n" treedepthbound;
flush stdout; flush stderr;

let treedepth = ref 0 in
let qbftrue = ref false in

let plansat () =
while (!treedepth < treedepthbound) && (not !qbftrue) do
  treedepth := !treedepth + 1;
  Utils.eprint "Searching solution at depth %d...\n" !treedepth;
  flush stdout; flush stderr;
  let touistcode = touistsolveqbf !treedepth (!treedepth + 1) "" in
  if touistcode == 0 then qbftrue:=true
  else if touistcode != 8 then begin Utils.eprint "Solver error %d.\n" touistcode; exit touistcode; end;
(**  let resultfile = (Unix.openfile "solvedata/out.touisterr.txt" [Unix.O_RDONLY] 0o640) in
  let c = Unix.in_channel_of_descr resultfile in
  let result = try input_line c with End_of_file -> "sat" in
(* INTERNAL SOLVER *)  (* if (String.compare result "unsat") != 0 then qbftrue:=true; *)
  if solvernum == 0 then (* DEPQBF EXTERNAL SOLVER *) if (String.compare result "Command 'depqbf --qdo --no-dynamic-nenofex' returned code 20 and no lines beginning with 'v'") != 0 then qbftrue:=true;
  if solvernum == 1 then if (String.compare result "Command 'rareqs' returned code 127 and no lines beginning with 'v'") != 0 then qbftrue:=true;
**)
done in

let (plansat_time,_) = Utils.my_time2bis (fun () -> plansat ()) in
Utils.eprint "Plan existence time (PLANSAT): %.2f\n" plansat_time;

  let rec extract depth = match depth with
    | 0 -> ();
    | i -> begin
             touistsolveqbf !treedepth i (Printf.sprintf "and not b(%d)" i);
             extract (i - 1);
             touistsolveqbf !treedepth i (Printf.sprintf "and b(%d)" i);
             extract (i - 1);
           end;
  in

let extract_time = ref 0.0 in
if !qbftrue then
begin
  Utils.eprint "Solution found at depth %d.\n" !treedepth; flush stdout; flush stderr;
  ignore (Sys.command (Printf.sprintf "cat solvedata/in.atoms%d.txt | grep 'and A_' | grep '(%d)'" !treedepth !treedepth));
  flush stdout; flush stderr;
  let (extr_time,_) = Utils.my_time2bis (fun () -> extract !treedepth) in extract_time := extr_time
end else Utils.eprint "No solution at maximum depth bound.\nThe planning problem does not have any solution.\n";

Utils.eprint "Plan existence time (PLANSAT): %.2f\n" plansat_time;
if !qbftrue then Utils.eprint "Plan extract time: %.2f\n" !extract_time;
flush stdout; flush stderr;

exit 0;
end;
(** END QBFPLAN-SOLVE **)


(** START SATPLAN-SOLVE **)
if encoding < 200 then
begin

let touistsolvesat length =
  let touistcode = ref 0 in
  Utils.eprint "--- TouIST solve (SAT) / length = %d ---\n" length;
  flush stdout; flush stderr;
  if solvernum == 0 then touistcode := (Sys.command (Printf.sprintf "(echo '$length = %d' ; cat solvedata/in.sets.txt ; cat solvedata/in.qfformula.txt) | touist --sat --solve - > solvedata/out.emodel.txt 2> solvedata/out.touisterr.txt" length));
  if solvernum == 101 then touistcode := (Sys.command (Printf.sprintf "(echo '$length = %d' ; cat solvedata/in.sets.txt ; cat solvedata/in.qfformula.txt) | touist --sat --solver 'glucose -model' - > solvedata/out.emodel.txt 2> solvedata/out.touisterr.txt" length));
  if solvernum == 102 then touistcode := (Sys.command (Printf.sprintf "(echo '$length = %d' ; cat solvedata/in.sets.txt ; cat solvedata/in.qfformula.txt) | touist --sat --solver 'glucose-syrup -model' - > solvedata/out.emodel.txt 2> solvedata/out.touisterr.txt" length));
  if solvernum == 103 then touistcode := (Sys.command (Printf.sprintf "(echo '$length = %d' ; cat solvedata/in.sets.txt ; cat solvedata/in.qfformula.txt) | touist --sat --solver 'picosat --partial' - > solvedata/out.emodel.txt 2> solvedata/out.touisterr.txt" length));
  if solvernum == 104 then touistcode := (Sys.command (Printf.sprintf "(echo '$length = %d' ; cat solvedata/in.sets.txt ; cat solvedata/in.qfformula.txt) | touist --sat --solver 'lingeling' - > solvedata/out.emodel.txt 2> solvedata/out.touisterr.txt" length));
  (* FICHIER DEBUG: *) ignore (Sys.command (Printf.sprintf "(echo '$length = %d' ; cat solvedata/in.sets.txt ; cat solvedata/in.qfformula.txt) > debug.touistl" length));
  !touistcode
in

let planlengthbound = 256 (* (Array.length pdata#fluents) * (Array.length pdata#fluents) *)
in
Utils.eprint "Maximum plan length: 2^|Fluents|=%d.\n" planlengthbound;
flush stdout; flush stderr;

let planlength = ref 0 in
let sattrue = ref false in
while (!planlength < planlengthbound) && (not !sattrue) do
  if incrmode <= 1 then planlength := !planlength + 1
  else if !planlength == 0 then planlength := 1
       else planlength := !planlength * incrmode;
  Utils.eprint "Searching solution with length %d...\n" !planlength;
  flush stdout; flush stderr;
  let touistcode = touistsolvesat !planlength in
  if touistcode == 0 then sattrue:=true
  else if touistcode != 8 then begin Utils.eprint "Solver error %d.\n" touistcode; exit touistcode; end;
(*  let resultfile = (Unix.openfile "solvedata/out.touisterr.txt" [Unix.O_RDONLY] 0o640) in
  let c = Unix.in_channel_of_descr resultfile in
  let result = try input_line c with End_of_file -> "sat" in
  if (String.compare result "unsat") != 0 then sattrue:=true; *)
done;

if !sattrue then
begin
  Utils.eprint "Solution found at length %d.\n" !planlength; flush stdout; flush stderr;
  for i = 1 to !planlength do
    ignore (Sys.command (Printf.sprintf "cat solvedata/out.emodel.txt | grep '1 A_' | grep '(%d)'" i));
    flush stdout; flush stderr;
  done;
end else Utils.eprint "No solution at maximum length bound.\nThe planning problem does not have any solution.\n";

exit 0;
end;
(** END SATPLAN-SOLVE **)

(** START SMTPLAN-SOLVE **)
if encoding < 300 then
begin

let touistsolvesat length =
  Utils.eprint "--- TouIST solve (SMT) / length = %d ---\n" length;
  flush stdout; flush stderr;
  ignore (Sys.command (Printf.sprintf "(echo '$length = %d' ; cat solvedata/in.sets.txt ; cat solvedata/in.qfformula.txt) | touist --smt QF_RDL --solve - > solvedata/out.emodel.txt 2> solvedata/out.touisterr.txt" length));
  (* FICHIER DEBUG: *) ignore (Sys.command (Printf.sprintf "(echo '$length = %d' ; cat solvedata/in.sets.txt ; cat solvedata/in.qfformula.txt) > debug.touistl" length));
in

let planlengthbound = 256 (* (Array.length pdata#fluents) * (Array.length pdata#fluents) *)
in
Utils.eprint "Maximum plan length: 2^|Fluents|=%d.\n" planlengthbound;
flush stdout; flush stderr;

let planlength = ref 0 in
let sattrue = ref false in
while (!planlength < planlengthbound) && (not !sattrue) do
  planlength := !planlength + 1;
  Utils.eprint "Searching solution with length %d...\n" !planlength;
  flush stdout; flush stderr;
  touistsolvesat !planlength;
  let resultfile = (Unix.openfile "solvedata/out.touisterr.txt" [Unix.O_RDONLY] 0o640) in
  let c = Unix.in_channel_of_descr resultfile in
  let result = try input_line c with End_of_file -> "sat" in
  if (String.compare result "unsat") != 0 then sattrue:=true;
done;

if !sattrue then
begin
  Utils.eprint "Solution found at length %d.\n" !planlength; flush stdout; flush stderr;
  for i = 1 to !planlength do
    ignore (Sys.command (Printf.sprintf "cat solvedata/out.emodel.txt | grep '1 A_' | grep '(%d)'" i));
    ignore (Sys.command (Printf.sprintf "cat solvedata/out.emodel.txt | sed -e '/^-/ d' | grep 't(A_' | grep ',%d)'" i));
    flush stdout; flush stderr;
  done;
end else Utils.eprint "No solution at maximum length bound.\nThe planning problem does not have any solution.\n";

exit 0;
end;
(** END SMTPLAN-SOLVE **)


(* END QBF-TOUISTPLAN without GP *)
exit 0;









let timedata_null = new Timedata.t (0.0,0.0) 0 in

let action_init =
  self#create_action "Init" [| |] 0.0 0 [| |] [| |] (Array.of_list (Array.fold_left (fun x f -> (f,f#atom#timedata(*timedata_null*)) :: x) [] pdata#init_state)) [| |]
in

let action_goal =
  self#create_action "Goal" [| |] 0.0 0 (Array.of_list (Array.fold_left (fun x f -> (f,timedata_null) :: x) [] pdata#goal)) [| |] [| |] [| |]
in

(*let get_f_timedata_list (f : 'fluent) (a_iset : (('fluent*Timedata.t) array)) = 
  Array.fold_left (fun t ((fluent,timedata) : ('fluent*Timedata.t)) ->
    if (fluent#atom#equal f#atom)
    then timedata :: t
    else t
  ) [] a_iset
in*)

(* Affichage Etat Initial et But *)

(*Array.iter (fun x -> Utils.print "Init_State: %s\n" x#to_string) pdata#init_state;
Utils.print "%s\n" action_init#to_complete_istring;
Array.iter (fun x -> Utils.print "Goal: %s\n" x#to_string) pdata#goal;*)


let print_agenda agenda =
  List.iter (fun (f, ag_pos, ag_neg) ->
    Utils.print "Agenda(%s):\n" f#to_string;
    if ag_pos = [] then Utils.print "  Empty\n" else
    List.iter (fun (t,t_a,la,da,t_b,lb,db) ->
      match t with
       | (true,true) -> Utils.print "  [t_%s_%d + %f ; t_%s_%d + %f]\n" t_a la da t_b lb db;
       | (true,false) -> Utils.print "  [t_%s_%d + %f ; t_%s_%d + %f[\n" t_a la da t_b lb db;
       | (false,true) -> Utils.print "  ]t_%s_%d + %f ; t_%s_%d + %f]\n" t_a la da t_b lb db;
       | (false,false) -> Utils.print "  ]t_%s_%d + %f ; t_%s_%d + %f[\n" t_a la da t_b lb db;
    ) ag_pos;
    Utils.print "Agenda(Not_%s):\n" f#to_string;
    if ag_neg = [] then Utils.print "  Empty\n" else
    List.iter (fun (t,t_a,la,da,t_b,lb,db) ->
      match t with
       | (true,true) -> Utils.print "  [t_%s_%d + %f ; t_%s_%d + %f]\n" t_a la da t_b lb db;
       | (true,false) -> Utils.print "  [t_%s_%d + %f ; t_%s_%d + %f[\n" t_a la da t_b lb db;
       | (false,true) -> Utils.print "  ]t_%s_%d + %f ; t_%s_%d + %f]\n" t_a la da t_b lb db;
       | (false,false) -> Utils.print "  ]t_%s_%d + %f ; t_%s_%d + %f[\n" t_a la da t_b lb db;
    ) ag_neg;
  ) agenda
in

let get_agenda pos f agenda =
  let (fluent,ag_pos,ag_neg) = List.find (fun (fl,ap,an) -> f#atom#equal fl#atom) agenda in
    if pos then ag_pos else ag_neg
in

let agenda_append pos f (t,t_a,la,da,t_b,lb,db) agenda =
 (* if pos then (Utils.print "Ajout Agenda(%s) : " f#to_string) else (Utils.print "Ajout Agenda(Not_%s) : " f#to_string);
  (match t with
   | (true,true) -> Utils.print "[t_%s%d + %f ; t_%s%d + %f]\n" t_a la da t_b lb db
   | (true,false) -> Utils.print "[t_%s%d + %f ; t_%s%d + %f[\n" t_a la da t_b lb db
   | (false,true) -> Utils.print "]t_%s%d + %f ; t_%s%d + %f]\n" t_a la da t_b lb db
   | (false,false) -> Utils.print "]t_%s%d + %f ; t_%s%d + %f[\n" t_a la da t_b lb db);*)
  List.map (fun (fluent,ag_pos,ag_neg) ->
    if (f#atom#equal fluent#atom) then
      if pos then (fluent,((t,t_a,la,da,t_b,lb,db) :: ag_pos),ag_neg)
      else (fluent,ag_pos,((t,t_a,la,da,t_b,lb,db) :: ag_neg))
    else (fluent,ag_pos,ag_neg)
  ) agenda
in

let comp_change comp = if options = "b" then (if (String.get comp 0 = '<') then Printf.sprintf "<=" else Printf.sprintf ">=") else comp in

let comp_apply comp f1 f2 =
  match comp with
   |"<" -> f1 < f2
   |"<=" -> f1 <= f2
   |">" -> f1 > f2
   |">=" -> f1 >= f2
   | _ -> false
in

let actions_distinct t_a la t_b lb =
(* let result= *)
  ((la <> lb) || ((String.compare t_a t_b) <> 0))
(* in
 if result then Printf.printf "actions_distinct: %s_%d <> %s_%d\n" t_a la t_b lb;
 result *)
in

let constraints_append_stc (comp,t_a,la,t_b,lb,ab) constraints =
  (*if debug_mode then Utils.print "Add STC : (t_%s_%d - t_%s_%d %s %f)\n" t_a la t_b lb comp ab;*)
  ((comp,t_a,la,t_b,lb,ab) :: (fst constraints),(snd constraints))
in

let constraints_append_dtc (comp1,t_a,la,t_b,lb,ab) (comp2,t_c,lc,t_d,ld,cd) constraints =
  (*if debug_mode then Utils.print "Add DTC : (t_%s_%d - t_%s_%d %s %f) v (t_%s_%d - t_%s_%d %s %f)\n" t_a la t_b lb comp1 ab t_c lc t_d ld comp2 cd;*)
  let actions_compare = ((actions_distinct t_a la t_b lb),(actions_distinct t_c lc t_d ld)) in
  match actions_compare with
   |(false,false) -> if (comp_apply comp1 0.0 ab)
                      then constraints (* dtc is true *)
                      else if (comp_apply comp2 0.0 cd)
                           then constraints (* dtc is true *)
                           else begin Utils.print "Inconsistency found in constraints set.\n"; ((fst constraints),((comp1,t_a,la,t_b,lb,ab),(comp2,t_c,lc,t_d,ld,cd)) :: (snd constraints)) end (* dtc is false !!! *)
   |(false,true) -> if (comp_apply comp1 0.0 ab)
                      then constraints (* dtc is true *)
                      else (constraints_append_stc (comp2,t_c,lc,t_d,ld,cd) constraints) (* simplify dtc to stc *)
   |(true,false) -> if (comp_apply comp2 0.0 cd)
                      then constraints (* dtc is true *)
                      else (constraints_append_stc (comp1,t_a,la,t_b,lb,ab) constraints) (* simplify dtc to stc *)
   |(true,true) -> ((fst constraints),((comp1,t_a,la,t_b,lb,ab),(comp2,t_c,lc,t_d,ld,cd)) :: (snd constraints))
in

(*let plan_append action level plan =
  (action,level) :: plan
in*)

let found_in_plan (a,i) plan =
  if (List.exists (fun (aa,ii) -> (a#ident_num = aa#ident_num) && (i = ii)) plan)
  then true else false (*begin Utils.print "[%s%d] BINGO !\n" a#to_string i; true end else begin Utils.print "[%s%d] NOT BINGO !!\n" a#to_string i; false end*)
in

let encode_float x = "(/ " ^ string_of_int (int_of_float (x*.1000000.0)) ^ " 1000000)"
in

let solve_constraints constraints plan variables =
begin
  let solver = (new Smtsolver.t) in
  let smtwrite = solver#smtwrite in
    solver#open_smtwrite;
    smtwrite (Printf.sprintf "(benchmark default.smt\n:source {\nTLP-GP automated DTP to SMT-LIB encoding for planning\nby F.Maris and P.Regnier, IRIT - Universite Paul Sabatier, Toulouse\n}\n:logic QF_RDL\n:extrafuns ((St_spy_variable Real))\n:extrafuns ((t_Init_0 Real))\n:extrafuns ((t_Goal_%d Real))\n" (succ rpg_max_level));
    List.iter (fun (a,i) ->
      if (a#ident_num <> 1) then smtwrite (Printf.sprintf ":extrafuns ((t_%s_%i Real))\n" a#to_string i);
    ) plan;
    List.iter (fun (var,i) ->
      smtwrite (Printf.sprintf ":extrafuns ((t_%s_%i Real))\n" var i)
    ) variables;
    smtwrite (Printf.sprintf ":formula ( and\n(= St_spy_variable (+ 1 t_Init_0))\n(>= t_Goal_%d t_Init_0)\n" (succ rpg_max_level));
    (* Temporal boundaries *)
    List.iter (fun (a,i) ->
      let first_prec_time = 0 (* a modifier pour prendre en compte les effets hors intervalle *)
      in
      let last_effect_time = a#duration (* a modifier pour prendre en compte les effets hors intervalle *) in
      smtwrite (Printf.sprintf "(>= (- t_%s_%i t_Init_0) %d)\n(>= (- t_Goal_%d t_%s_%i) %s)\n" a#to_string i first_prec_time (succ rpg_max_level) a#to_string i (encode_float last_effect_time))
    ) plan;
    List.iter (fun (var,i) ->
      smtwrite (Printf.sprintf "(>= t_%s_%i t_Init_0)\n(>= t_Goal_%d t_%s_%i)\n" var i (succ rpg_max_level) var i) (* modifier pour prise en compte de la durée d'intervalle anywhere *)
    ) variables;
    smtwrite "\n";
    (* STP Constraints *)
    List.iter (fun (comp,t_a,la,t_b,lb,ab) ->
      smtwrite (Printf.sprintf "(%s (- t_%s_%d t_%s_%d) %s)\n" comp t_a la t_b lb (encode_float ab))
    ) (fst constraints);
    smtwrite "\n";
    (* DTP Constraints *)
    List.iter (fun ((comp1,t_a,la,t_b,lb,ab),(comp2,t_c,lc,t_d,ld,cd)) ->
       smtwrite (Printf.sprintf "(or (%s (- t_%s_%d t_%s_%d) %s) (%s (- t_%s_%d t_%s_%d) %s))\n" (comp_change comp1) t_a la t_b lb (encode_float ab) (comp_change comp2) t_c lc t_d ld (encode_float cd))
    ) (snd constraints);
    smtwrite "))\n";
    solver#close_smtwrite;
    solver#launch
end
in

(* Search strategy for choosing actions *)
let get_producers (fluent:'fluent) (level : int)=
  let prodlist = ref []
  in
    Array.iter (fun a ->
      for i = (level - 1) downto (a#level) do
        prodlist := (a,i) :: !prodlist;
      done
    ) fluent#producers;
  if (Array.fold_left (fun x f -> (fluent#atom#equal f#atom) || x) false pdata#init_state) then prodlist := (action_init,0) :: !prodlist;
  !prodlist;
in

(*  Array.fold_left (fun x a ->
    let prodlist = ref [] in
    for i=(level - 1) downto 1 do
      prodlist := ((a,i) :: !prodlist)
    done;
    (List.append prodlist x)
  ) [] fluent#producers
in *)

let rec iter_producers funct (agenda,plan,constraints,c) = function
  | [] ->
      (agenda,plan,constraints,false)
  | prod :: prod_tl ->
      let (ag,pl,co,cc) = (funct (agenda,plan,constraints,c) prod) in if cc then (ag,pl,co,true) else (iter_producers funct (agenda,plan,constraints,c) prod_tl)
in

let add_goals ((a,i) : 'action * int) goals_tl =
  Array.fold_left (fun goals_list p -> (p,a,i) :: goals_list) goals_tl a#prec
in

(* FONCTION LINKS *)
let rec links (aux :
(('fluent*'action*int) list) 
* (('fluent *(((bool*bool)*(*'action*)string*int*float*(*'action*)string*int*float) list)*(((bool*bool)*(*'action*)string*int*float*(*'action*)string*int*float) list )) list)
* (('action * int) list) 
* ((( string * (*'action*)string * int * (*'action*)string * int * float)list)* 
((( string * (*'action*)string * int * (*'action*)string * int * float)*
( string * (*'action*)string * int * (*'action*)string * int * float)) list))
* ((string * int) list)
(** * int **)
)

= match aux with
  | ([],agenda,plan,constraints,variables (**,0**) ) ->
    (agenda,plan,constraints,true)
(*  | ([],agenda,plan,constraints,variables (**,depth**) ) ->
    (agenda,plan,constraints,(solve_constraints constraints plan variables)) *)
  | ((p,b,j) :: goals_tl,agenda,plan,constraints,variables (**,depth**) ) ->
    iter_producers (fun (ag,pl,co,c) (a,i) ->
let (aux_agenda,aux_constraints,aux_variables) =
(* Pour chaque effet negatif Not_f de A, ajouter un intervalle d'effet à Agenda(Not_f) *)
(Array.fold_left (fun (c_ag3,c_co3,variables_aux1) neff ->
   ((if not (found_in_plan (a,i) plan) then
       let agenda_aux1=(agenda_append false neff ((get_f_time_bound neff a#idel),a#to_string,i,(fst (get_f_time neff a#idel)),a#to_string,i,(snd (get_f_time neff a#idel))) c_ag3) in
         (***if (get_f_timedata_code neff a#idel)==10 then (* dans le cas d'une transition, ajouter également un intervalle semi-ouvert de protection à Agenda(f) *)
           (agenda_append true neff (((fst (get_f_time_bound neff a#idel)),false),a#to_string,i,(fst (get_f_time neff a#idel)),a#to_string,i,(snd (get_f_time neff a#idel))) agenda_aux1)
         else***) agenda_aux1
     else c_ag3),
(* Pour chaque intervalle de Agenda(f), poser une contrainte de non recouvrement *)
    (let constraints_aux1=(List.fold_left (fun c_co4 (typ,t_aa,ia,ta,t_bb,ib,tb) ->
      let ft_adel = (fst (get_f_time neff a#idel)) (* del begins at start *) in
       let not_auth_tx31 = (actions_distinct a#to_string i t_bb ib) in
       let not_auth_tx32 = (actions_distinct a#to_string i t_aa ia) in
       (constraints_append_dtc
         (if (tb > ft_adel) then ((if (not_auth_tx31) && (snd typ) && (fst (get_f_time_bound neff a#idel)) then ">" else ">="),a#to_string,i,t_bb,ib,(tb -. ft_adel)) else ((if (not_auth_tx31) && (snd typ) && (fst (get_f_time_bound neff a#idel)) then "<" else "<="),t_bb,ib,a#to_string,i,(ft_adel -. tb)))
         (if (ft_adel > ta) then ((if (not_auth_tx32) && (fst typ) && (fst (get_f_time_bound neff a#idel)) then ">" else ">="),t_aa,ia,a#to_string,i,(ft_adel -. ta)) else ((if (not_auth_tx31) && (fst typ) && (fst (get_f_time_bound neff a#idel)) then "<" else "<="),a#to_string,i,t_aa,ia,(ta -. ft_adel)))
         c_co4)
    ) c_co3 (get_agenda true neff c_ag3)) in
      (***if (get_f_timedata_code neff a#idel)==10 then (* dans le cas d'une transition, poser également une contrainte de non recouvrement pour chaque intervalle de Agenda(Not_f) *)
        (List.fold_left (fun c_co4 (typ,t_aa,ia,ta,t_bb,ib,tb) ->
          let ft_adel_start = (fst (get_f_time neff a#idel)) in
          let ft_adel_end = (snd (get_f_time neff a#idel)) (* del at end *) in
            (constraints_append_dtc
              (if (tb > ft_adel_start) then ((if (snd typ) && (fst (get_f_time_bound neff a#idel)) then ">" else ">="),a#to_string,i,t_bb,ib,(tb - ft_adel_start)) else ((if (snd typ) && (fst (get_f_time_bound neff a#idel)) then "<" else "<="),t_bb,ib,a#to_string,i,(ft_adel_start - tb)))
              (if (ft_adel_end > ta) then ((if (fst typ) && (snd (get_f_time_bound neff a#idel)) then ">" else ">="),t_aa,ia,a#to_string,i,(ft_adel_end - ta)) else ((if (fst typ) && (snd (get_f_time_bound neff a#idel)) then "<" else "<="),a#to_string,i,t_aa,ia,(ta - ft_adel_end)))
              c_co4)
         ) constraints_aux1 (get_agenda false neff c_ag3))
      else***)
        constraints_aux1),
     variables_aux1
   )
)
(* Pour chaque effet positif f de A, ajouter un intervalle d'effet à Agenda(f) *)
(Array.fold_left (fun (c_ag1,c_co1,variables_aux2) eff ->
  if not (eff#atom#equal p#atom) then
   ((if not (found_in_plan (a,i) plan) then
       let agenda_aux2=(agenda_append true eff ((get_f_time_bound eff a#iadd),a#to_string,i,(fst (get_f_time eff a#iadd)),a#to_string,i,(snd (get_f_time eff a#iadd))) c_ag1) in
         (***if (get_f_timedata_code eff a#iadd)==10 then (* dans le cas d'une transition, ajouter également un intervalle semi-ouvert de protection à Agenda(Not_f) *)
           (agenda_append false eff (((fst (get_f_time_bound eff a#iadd)),false),a#to_string,i,(fst (get_f_time eff a#iadd)),a#to_string,i,(snd (get_f_time eff a#iadd))) agenda_aux2)
         else***) agenda_aux2
    else c_ag1),
(* Pour chaque intervalle de Agenda(Not_f), poser une contrainte de non recouvrement *)
    (let constraints_aux2=(List.fold_left (fun c_co2 (typ,t_aa,ia,ta,t_bb,ib,tb) ->
      let ft_aadd = (fst (get_f_time eff a#iadd)) (* add begins at start *) in
       let not_auth_tx21 = (actions_distinct a#to_string i t_bb ib) in
       let not_auth_tx22 = (actions_distinct a#to_string i t_aa ia) in
       (*let simplify2 = (if not_auth_tx21 then ,) in*)
       (constraints_append_dtc
         (if (tb > ft_aadd) then ((if (not_auth_tx21) && (snd typ) && (fst (get_f_time_bound eff a#iadd)) then ">" else ">="),a#to_string,i,t_bb,ib,(tb -. ft_aadd)) else ((if (not_auth_tx21) && (snd typ) && (fst (get_f_time_bound eff a#iadd)) then "<" else "<="),t_bb,ib,a#to_string,i,(ft_aadd -. tb)))
         (if (ft_aadd > ta) then ((if (not_auth_tx22) && (fst typ) && (fst (get_f_time_bound eff a#iadd)) then ">" else ">="),t_aa,ia,a#to_string,i,(ft_aadd -. ta)) else ((if (not_auth_tx22) && (fst typ) && (fst (get_f_time_bound eff a#iadd)) then "<" else "<="),a#to_string,i,t_aa,ia,(ta -. ft_aadd)))
         c_co2)
    ) c_co1 (get_agenda false eff c_ag1)) in
      (***if (get_f_timedata_code eff a#iadd)==10 then (* dans le cas d'une transition, poser également une contrainte de non recouvrement pour chaque intervalle de Agenda(f) *)
       (List.fold_left (fun c_co2 (typ,t_aa,ia,ta,t_bb,ib,tb) ->
         let ft_aadd_start = (fst (get_f_time eff a#iadd)) in
         let ft_aadd_end = (snd (get_f_time eff a#iadd)) (* add at end *) in
          (constraints_append_dtc
            (if (tb > ft_aadd_start) then ((if (snd typ) && (fst (get_f_time_bound eff a#iadd)) then ">" else ">="),a#to_string,i,t_bb,ib,(tb - ft_aadd_start)) else ((if (snd typ) && (fst (get_f_time_bound eff a#iadd)) then "<" else "<="),t_bb,ib,a#to_string,i,(ft_aadd_start - tb)))
            (if (ft_aadd_end > ta) then ((if (fst typ) && (snd (get_f_time_bound eff a#iadd)) then ">" else ">="),t_aa,ia,a#to_string,i,(ft_aadd_end - ta)) else ((if (fst typ) && (snd (get_f_time_bound eff a#iadd)) then "<" else "<="),a#to_string,i,t_aa,ia,(ta - ft_aadd_end)))
            c_co2)
       ) constraints_aux2 (get_agenda true eff c_ag1))
      else***) constraints_aux2),
    variables_aux2
  )
  else (c_ag1,c_co1,variables_aux2)
)
(* Poser un intervalle de maintien de précondition à Agenda(p) *)

(let (variables_aux3 : ((string*int) list) ref) = ref [] in
 let (t_a,ft_aadd_ag,ft_aadd_co) =
 let atdc = (get_f_timedata_code p a#iadd) in
   match atdc with
    |0 -> (a#to_string,(fst (get_f_time p a#iadd)),(fst (get_f_time p a#iadd))) (* eff_OVER : p certain au debut de l'intervalle, protection au debut de l'intervalle *)
    |1 -> (a#to_string,(fst (get_f_time p a#iadd)),(snd (get_f_time p a#iadd))) (* eff_SOMEWHERE : p certain a la fin de l'intervalle, protection au debut de l'intervalle *)
    |2 -> variables_aux3 := ("where__" ^ a#to_string ^ "__produces__" ^ p#to_string,i) :: !variables_aux3; ("where__" ^ a#to_string ^ "__produces__" ^ p#to_string,0.0,0.0) (* eff_ANYWHERE : prendre en compte nouvelle variable temporelle !!! a faire !!! *)
    |10 -> (a#to_string,(fst (get_f_time p a#iadd)),(snd (get_f_time p a#iadd))) (* trans_OVER : p certain a la fin de l'intervalle, protection au debut de l'intervalle *)
    |_ -> (a#to_string ^ "__UNKNOWN_INTERVAL_TYPE",0.0,0.0)
   in
  let (t_b,ft_bprec_ag,ft_bprec_co) =
  let btdc = (get_f_timedata_code p b#iprec) in
    match btdc with
     |0 -> (b#to_string,(snd (get_f_time p b#iprec)),(fst (get_f_time p b#iprec)))
     |1 -> (b#to_string,(snd (get_f_time p b#iprec)),(fst (get_f_time p b#iprec)))
     |2 -> variables_aux3 := ("where__" ^ b#to_string ^ "__needs__" ^ p#to_string,j) :: !variables_aux3; ("where__" ^ b#to_string ^ "__needs__" ^ p#to_string,0.0,0.0) (* prec_ANYWHERE :  *)
     |_ -> (b#to_string ^ "__UNKNOWN_INTERVAL_TYPE",0.0,0.0)
  in
let new_agenda1 = (agenda_append true p (((fst (get_f_time_bound p a#iadd)),(snd (get_f_time_bound p b#iprec))),t_a,i,ft_aadd_ag,t_b,j,ft_bprec_ag) ag (* agenda_feed *)) in
(((***if (get_f_timedata_code p a#iadd)==10 then (* dans le cas d'une transition, ajouter également un intervalle de protection à Agenda(Not_p) *)
  (agenda_append false p (((fst (get_f_time_bound p a#iadd)),false),t_a,i,ft_aadd_ag,t_a,i,ft_aadd_co) new_agenda1)
  else***) new_agenda1),
(* Pour chaque intervalle de Agenda(Not_p), poser une contrainte de non recouvrement *)
(let constraints_aux3=(List.fold_left (fun clist (typ,t_aa,ia,ta,t_bb,ib,tb) ->
         let not_auth_tx = (actions_distinct b#to_string j t_aa ia) in
           (constraints_append_dtc
             (if (tb > ft_aadd_ag) then ((if (snd typ) && (fst (get_f_time_bound p a#iadd)) then ">" else ">="),a#to_string,i,t_bb,ib,(tb -. ft_aadd_ag)) else ((if (snd typ) && (fst (get_f_time_bound p a#iadd)) then "<" else "<="),t_bb,ib,a#to_string,i,(ft_aadd_ag -. tb)))
             (if (ft_bprec_ag > ta) then ((if (not_auth_tx) && (fst typ) && (fst (get_f_time_bound p b#iprec)) then ">" else ">="),t_aa,ia,b#to_string,j,(ft_bprec_ag -. ta)) else ((if (not_auth_tx) && (fst typ) && (fst (get_f_time_bound p b#iprec)) then "<" else "<="),b#to_string,j,t_aa,ia,(ta -. ft_bprec_ag)))
             clist)
        )
(* Poser une contrainte de précédence entre A et B *)
  (if (ft_aadd_co >= ft_bprec_co) then (constraints_append_stc (">=",b#to_string,j,a#to_string,i,(ft_aadd_co -. ft_bprec_co)) co (* constraints_feed *))
  else (constraints_append_stc ("<=",a#to_string,i,b#to_string,j,(ft_bprec_co -. ft_aadd_co)) co (* constraints_feed *)))
  (get_agenda false p new_agenda1)
) in
        (***if (get_f_timedata_code p a#iadd)==10 then (* dans le cas d'une transition, poser également une contrainte de non recouvrement de l'intervalle de transition pour chaque intervalle de Agenda(p) *)
(List.fold_left (fun clist (typ,t_aa,ia,ta,t_bb,ib,tb) ->
         let (ft_aadd_start,ft_aadd_end) = (get_f_time p a#iadd) (* add at end *) in
           (constraints_append_dtc
             (if (tb > ft_aadd_start) then ((if (snd typ) && (fst (get_f_time_bound p a#iadd)) then ">" else ">="),a#to_string,i,t_bb,ib,(tb - ft_aadd_start)) else ((if (snd typ) && (fst (get_f_time_bound p a#iadd)) then "<" else "<="),t_bb,ib,a#to_string,i,(ft_aadd_start - tb)))
             (if (ft_aadd_end > ta) then ((if (fst typ) && (snd (get_f_time_bound p a#iadd)) then ">" else ">="),t_aa,ia,a#to_string,i,(ft_aadd_end - ta)) else ((if (fst typ) && (snd (get_f_time_bound p a#iadd)) then "<" else "<="),a#to_string,i,t_aa,ia,(ta - ft_aadd_end)))
             clist)
        ) constraints_aux3 (get_agenda true p new_agenda1))
        else***) constraints_aux3),
(List.append !variables_aux3 variables)
))
a#add) a#del) in
      let (aux_plan,found) = (if not (found_in_plan (a,i) plan) then ((a,i) :: plan,false) else (plan,true)) in

(**)
      if solve_constraints aux_constraints aux_plan aux_variables then
        if not found then
        links (((add_goals (a,i) goals_tl) : (('fluent * 'action * int) list)),
              aux_agenda,
              aux_plan,
              aux_constraints,
              aux_variables)
        else
        links ((goals_tl : (('fluent * 'action * int) list)),
              aux_agenda,
              plan,
              aux_constraints,
              aux_variables)
      else
        (ag,pl,co,false)


 (**     let (csolved,go_west,newdepth) = if (depth == 3) then let csol = (solve_constraints aux_constraints aux_plan) in (csol,csol,0) else (false,true,succ depth) in

     (* if solve_constraints aux_constraints aux_plan then *)
       if go_west then


        if not (found_in_plan (a,i) plan) then
        let (ag_return, pl_return, co_return, sol_return) =
        links (((add_goals (a,i) goals_tl) : (('fluent * 'action * int) list)),
              aux_agenda,
              aux_plan,
              aux_constraints,
              newdepth)
         in if ((depth > 0) && (not sol_return)) then
              if (solve_constraints aux_constraints aux_plan) then
                (aux_agenda,aux_plan,aux_constraints,true)
              else
                (ag,pl,co,false)
            else (ag_return, pl_return, co_return, sol_return)
        else
        let (ag_return, pl_return, co_return, sol_return) =
        links ((goals_tl : (('fluent * 'action * int) list)),
              aux_agenda,
              plan,
              aux_constraints,
              newdepth)
         in if ((depth > 0) && (not sol_return)) then
              if (solve_constraints aux_constraints aux_plan) then
                (aux_agenda,plan,aux_constraints,true)
              else
                (ag,pl,co,false)
            else (ag_return, pl_return, co_return, sol_return)
      else
        (ag,pl,co,false)

(*end*) **)


    ) (agenda,plan,constraints,false) (get_producers p j)
in




goals <- List.map (fun f -> (f,false)) (Array.to_list pdata#goal);

(*links ([(action_init,0);(action_goal,rpg_max_level + 1)],
       (Array.fold_left (fun x f -> ((f,[],[]) :: x) ) [] pdata#fluents),
       [],
       ([],[])
      );*)

(*if solve_constraints [] then Utils.print "TLP-GP_SAT" else Utils.print "TLP-GP_UNSAT";*)



(*
(* Prend en entrée la liste des buts et renvoie un couple (premier but, liste modifiée) *)
let choose_goal goals_list = 
in*)

action_init#set_ident_num 1;
action_goal#set_ident_num 2;
let it = ref 2 in Array.iter (fun a -> it:=(succ !it); a#set_ident_num !it;) pdata#actions;

while not solved do
      Utils.print "\nSearching floating plan using TLP-GP algorithm at level %d ...\n" rpg_max_level;

(*Utils.print "[SPECIAL ACTION_INIT]\n%s\n" action_init#to_complete_istring;
Utils.print "[SPECIAL ACTION_GOAL]\n%s\n" action_goal#to_complete_istring;*)

(*exemple:  List.fold_left (fun goals_list p -> (p,a,i) :: goals_list) goals_tl a#prec*)

let (agenda,plan,constraints,sol) =
links ((Array.fold_left (fun goals_list p -> (p,action_goal,(rpg_max_level + 1)) :: goals_list) [] pdata#goal),
       (Array.fold_left (fun x f -> ((f,[],[]) :: x) ) [] pdata#fluents),
       [],
       ([],[]),
       [] (**,
       0 **)
      )
in
solved <- sol;

  (* print_agenda agenda; *)

if solved then begin
 Utils.print "Floating plan found at level %d.\n" rpg_max_level;
(* List.iter (fun (a,i) ->
   Utils.print "[%s](level %d)\n" a#to_string i;
 ) plan; *)
 Utils.print "Constraints Set: #STC=%d / #DTC=%d\n"
   (List.length (fst constraints))
   (List.length (snd constraints));
 if debug_mode then print_agenda agenda;
 let extractor = (new Smtsolver_extractplan.t) in
  let (plan_times,solver_factor) = extractor#launch in
   if plan_times = [] then
   begin
    Utils.print "SMT Solver did not return a static plan.\n--------------------------------------------\nACTIONS (in the floating plan)\n--------------------------------------------\n";
    List.iter (fun (a,i) ->
      Utils.print "[%s](level %d)\n" a#to_string i;
    ) plan;
   end else begin
    Utils.print "STATIC PLAN:\n--------------------------------------------\npossible start time: (ACTION) [duration]\n--------------------------------------------\n";
    (*List.iter (fun (var_name, level_num, start_time) -> Utils.print "%s (%d) [%d.%06d]\n" var_name level_num ((fst start_time)/(1000000*solver_factor*(snd start_time))) (((fst start_time) mod (1000000*solver_factor*(snd start_time)))/(1000000*solver_factor*(snd start_time)))) plan_times;*)
    (*List.iter (fun (var_name, level_num, start_time) -> Utils.print "%s (%d) [%d.%06d]\n" var_name level_num (start_time/solver_factor) (1000000*(start_time mod solver_factor)/solver_factor)) plan_times;*)
    let current_time = ref (-1.0) in
    List.iter (fun (var_name, level_num, start_time) ->
      let v_action =
       if var_name = "Init" then action_init else if var_name = "Goal" then action_goal else
        (Array.fold_left (fun r a -> if a#to_string = var_name then a else r)) action_init pdata#actions
      in
      if !current_time = start_time then
       Utils.print "%s  %s [%f] {level %d}\n" (String.make (String.length (Printf.sprintf "%f" (!current_time/.(float_of_int solver_factor)))) ' ') v_action#presentation_string v_action#duration level_num
      else begin
       current_time := start_time;
       Utils.print "%f: %s [%f] {level %d}\n" (start_time/.(float_of_int solver_factor)) v_action#presentation_string v_action#duration level_num
      end) plan_times
   end;
(* BEGIN writing plan to output file *)
   if plan_times <> [] then
   begin
    let planfile = open_out "solution-plan.txt" in
    output_string planfile (Printf.sprintf "\n; Makespan: %f\n\n" (List.fold_left (fun t (var_name, level_num, start_time) -> if (String.compare var_name "Goal") = 0 then (start_time/.(float_of_int solver_factor)) else t) 0.0 plan_times));
    let current_time = ref (-1.0) in
    List.iter (fun (var_name, level_num, start_time) ->
      let (no_hide,v_action) =
       if var_name = "Init" then (false,action_init) else if var_name = "Goal" then (false,action_goal) else
        (true,(Array.fold_left (fun r a -> if a#to_string = var_name then a else r)) action_init pdata#actions)
      in
      if no_hide then begin
       (*if !current_time = start_time then
        output_string planfile (Printf.sprintf "%s  %s [%f]\n" (String.make (String.length (Printf.sprintf "%f" (!current_time/.(float_of_int solver_factor)))) ' ') v_action#presentation_string v_action#duration)
       else begin*)
        current_time := start_time;
        output_string planfile (Printf.sprintf "%f: %s [%f]\n" (start_time/.(float_of_int solver_factor)) v_action#presentation_string v_action#duration)
       (*end*)
      end) plan_times;
    close_out_noerr planfile
   end else begin
    let planfile = open_out "solution-plan.txt" in
     output_string planfile (Printf.sprintf "; Floating plan exists ...\n; The solver did not return a static plan.\n" );
     close_out_noerr planfile
   end;
(* END writing plan to output file *)
   Utils.print "--------------------------------------------\nMakespan: ";
   if plan_times = [] then Utils.print "?\n"
    else Utils.print "%f\n" (List.fold_left (fun t (var_name, level_num, start_time) -> if (String.compare var_name "Goal") = 0 then (start_time/.(float_of_int solver_factor)) else t) 0.0 plan_times);
   Utils.print "--------------------------------------------\n\n";
end else begin
 Utils.print "No floating plan at level %d.\n" rpg_max_level;
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

end