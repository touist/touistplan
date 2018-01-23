class node_common =
  object (self)
  end
class fluent atom =
  object (self)
    inherit node_common
    inherit [action] Node.fluent atom
  end
and action name params duration quality prec nprec add del =
  object (self)
    inherit node_common
    inherit [fluent] Node.action name params duration quality prec nprec add del
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


(* [time_allowed] is in seconds. The same amount of time before timeout is
   given to finding the existence (i.e., increase the depth until the QBF is
   true) and to extracting the plan.
   [verbose] 0 -> not verbose, 1 -> more verbose, 2 -> even more verbose *)
class t (problem:string) (domain:string) (depth : int) =
  object (self)
    inherit [fluent, action, plan] PlanningData.t problem domain "" as pdata
    inherit [fluent, action, plan] tsp_common

    method print_statistics = ()
    method run = self#plan_fail
    method virtual create_action : string -> Symb.constant array -> float -> int -> ('fluent*Timedata.t) array -> ('fluent*Timedata.t) array -> ('fluent*Timedata.t) array -> ('fluent*Timedata.t) array -> 'action

    method search =
      let k = depth in

      (** CTE-NOOP **)

      let cte_noop_mutex = (* action mutex constraints *)
        (k+1) *
        Array.fold_left (fun acc a1 ->
            Array.fold_left (fun acc2 a2 ->
                if a1#num >= a2#num then acc2
                else if (Array.exists (fun f -> (Array.mem f a2#prec)||(Array.mem f a2#add)) a1#del)
                     || (Array.exists (fun f -> (Array.mem f a1#prec)||(Array.mem f a1#add)) a2#del)
                then begin (* Utils.print "Mutex{%s,%s}\n" a1#to_string a2#to_string; *) succ acc2 end else acc2
              ) acc pdata#actions
          ) 0 pdata#actions (* (k+1) * |Mutex| (R8) *)
        + (k+1) * Array.fold_left (fun acc f -> acc + Array.length f#deleters) 0 pdata#fluents (* (k+1) * sum_f^F |Deleters(f)| (R9) *)
      in
      let cte_noop_bc = (* branch constraints *)
        Array.fold_left (fun acc a -> if Array.for_all (fun f -> Array.mem f pdata#init_state) a#prec then acc else succ acc) 0 pdata#actions (* |{a / Pre(a) not included in I}| (1) *)
        + Array.fold_left (fun acc fl -> if Array.mem fl pdata#init_state then acc else acc+1) 0 pdata#fluents (* |F-I| (2) *)
        + Array.length pdata#goal (* |G|  (R3) *)
        + 2*k * Array.fold_left (fun acc a -> acc + Array.length a#prec) 0 pdata#actions (* 2k * sum_a^A |Pre(a)| (R4,6) *)
        + 2*k * Array.length pdata#fluents (* 2k * |F| (R5,7) *)
      and cte_noop_nc = (* node constraints without mutex *)
        cte_noop_mutex
      in

      (** CTE-EFA **)

      let cte_efa_mutex = (* action mutex constraints *)
        (k+1) *
        Array.fold_left (fun acc a1 ->
            Array.fold_left (fun acc2 a2 ->
                if a1#num >= a2#num then acc2
                else if (Array.exists (fun f -> (Array.mem f a2#prec)||(Array.mem f a2#add)) a1#del)
                     || (Array.exists (fun f -> (Array.mem f a1#prec)||(Array.mem f a1#add)) a2#del)
                then begin (* Utils.print "Mutex{%s,%s}\n" a1#to_string a2#to_string; *) succ acc2 end else acc2
              ) acc pdata#actions
          ) 0 pdata#actions (* (k+1) * |Mutex| (12) *)
      (* branch constraints (= in which b_i is used) *)
      in
      let cte_efa_bc =
        Array.length pdata#goal (* |F| (1) *)
        + Array.fold_left (fun acc a -> if Array.for_all (fun f -> Array.mem f pdata#init_state) a#prec then acc else succ acc) 0 pdata#actions (* |{a / Pre(a) not included in I}|  (2) *)
        + 2*k * Array.fold_left (fun acc a -> acc + Array.length a#prec) 0 pdata#actions  (* 2k * sum_a^A |Pre(A)|  (4,5) *)
        + Array.fold_left (fun acc fl -> if Array.mem fl pdata#init_state then acc else acc+1) 0 pdata#fluents (* |F-I| (6) *)
        + 2*k * Array.length pdata#fluents (* 2k * |F| (7,8) *)
        + Array.length pdata#init_state (* |I| (9) *)
        + 2*k * Array.length pdata#fluents (* 2k (10,11) *)
      (* node constraints without mutex (= b_i does not appear in these formulas) *)
      and cte_efa_nc =
        (k+1) * Array.fold_left (fun acc a -> acc + Array.length a#add + Array.length a#del) 0 pdata#actions (* (k+1) * sum_a^A(|Add(a)|+|Del(a)|)  (3) *)
        + cte_efa_mutex
      in

      (** CTE-OPEN **)

      let cte_open_mutex = (* action mutex constraints *)
        (k+1) *
          Array.fold_left (fun acc a1 ->
              Array.fold_left (fun acc2 a2 ->
                  if a1#num >= a2#num then acc2
                  else if (Array.exists (fun f -> (Array.mem f a2#prec)||(Array.mem f a2#add)) a1#del)
                       || (Array.exists (fun f -> (Array.mem f a1#prec)||(Array.mem f a1#add)) a2#del)
                  then begin (* Utils.print "Mutex{%s,%s}\n" a1#to_string a2#to_string; *) succ acc2 end else acc2
                ) acc pdata#actions
            ) 0 pdata#actions (* (k+1) * |Mutex| (4) *)
      in     
      let cte_open_bc = (* branch constraints *)
        Array.length pdata#goal (* |G|  (1.2) *)
        + k * Array.fold_left (fun acc fl -> if Array.mem fl pdata#init_state then acc else acc+1) 0 pdata#fluents (* k |F-I| (2.1) *)
        + 2*k * Array.length pdata#fluents (* 2k |F| (2.2) *)
        + 2*k * Array.fold_left (fun acc a -> acc + Array.length a#del) 0 pdata#actions (* 2k * sum_a^A |Del(a)| (3.1) *)
      and cte_open_nc = (* node constraints *)
        (k+1) * Array.fold_left (fun acc a -> acc + Array.length a#prec) 0 pdata#actions (* (k+1) * sum_a^A |Pre(a)| *)
        + cte_open_mutex
      in
      Utils.print "CTE-NOOP (%d); branch constraints: %d; node constraints (no mutex): %d; mutex: %d\n" (cte_noop_bc + cte_noop_nc + cte_noop_mutex) cte_noop_bc (cte_noop_nc - cte_noop_mutex) cte_noop_mutex;
      Utils.print "CTE-EFA (%d); branch constraints: %d; node constraints (no mutex): %d; mutex: %d\n" (cte_efa_bc + cte_efa_nc + cte_efa_mutex) cte_efa_bc (cte_efa_nc - cte_efa_mutex) cte_efa_mutex;
      Utils.print "CTE-OPEN (%d); branch constraints: %d; node constraints (no mutex): %d; mutex: %d\n" (cte_open_bc + cte_open_nc + cte_efa_mutex) cte_open_bc (cte_open_nc - cte_open_mutex) cte_open_mutex;
  end