class node_common =
object (self)

  val mutable n = Array.make 256 0
  method n level = n.(level)
  method set_n level nn = n.(level) <- nn

end

class fluent atom =
object (self)
  inherit node_common
  inherit [action] Node.fluent atom

  val string = 
    atom#pred#to_string ^ 
    if atom#nb_terms = 0 then "" else "_" ^  (Utils.string_of_array "_" Utils.to_string atom#terms)

  method to_string = string
end


and action name params duration quality prec nprec add del =
object (self)
  inherit node_common
  inherit [fluent] Node.action name params duration quality prec nprec add del

  val string = 
    name ^ if Array.length params = 0 then "" else "_" ^ (Utils.string_of_array "_" Utils.to_string params)

  method to_string = string

  method to_complete_string = 
    let string_of_fluent_array fluents =
      Utils.string_of_array "#" Utils.to_string fluents in
      "Prec(" ^ string ^ "#" ^ string_of_fluent_array prec ^ "#\n" ^
      "Add(" ^ string ^ "#" ^ string_of_fluent_array add ^ "#\n" ^
      "Del(" ^ string ^ "#" ^ string_of_fluent_array del ^ "#\n"
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



class t =
object (self)
  inherit [fluent, action, plan] PlanningData.t  as pdata
  inherit [fluent, action, plan] tsp_common

  val mutable nb = 0
  val mutable nbc = 0

  method print_statistics = ()
  method run = self#plan_fail

  method print =
    let level = int_of_string Sys.argv.(4) in
    let pri = Sys.argv.(5) = "i" in
    let pr = if pri then positive_print_int else positive_print_string in
    let prn = if pri then negative_print_int else negative_print_string in
    let endl = if pri then (fun () -> nbc <- nbc + 1 ; endl_int ()) else (fun () -> nbc <- nbc + 1 ; endl_string ()) in

      for i = 0 to level do
	Array.iter (fun f -> nb <- nb + 1 ; f#set_n i nb ; Utils.print "c %s_%i %i\n" f#to_string i nb) pdata#fluents 
      done ;
      for i = 0 to level - 1 do
	Array.iter (fun a -> nb <- nb + 1 ; a#set_n i nb ; Utils.print "c %s_%i %i\n" a#to_string i nb) pdata#actions ;
      done ;

      Utils.print "f %i " nb ;
      (*Array.iter (fun f -> Utils.print "%i " (f#n 0)) pdata#fluents ;*)
      for i = 0 to level - 1 do
	Array.iter (pr i) pdata#actions 
      done ;

      for i = 0 to level - 1 do
	Array.iter (pr i) pdata#actions 
      done ;
      
      Array.iter (pr 0) pdata#fluents ;
      Array.iter (pr level) pdata#fluents ;
      
      endl() ;
      
      Utils.print "p cnf %i %i\n" nb 0 ;

      (*
      Array.iter (fun f -> pr level f) pdata#goal ;
	endl () ;
      *)
      
      Array.iter (fun f -> if not f#is_init then Utils.print "-" ; pr 0 f ; endl()) pdata#fluents ;
      

      for i = 0 to level - 1 do
	Array.iter (pr i) pdata#actions ;
	endl ()
      done ;
      
      for i = 0 to level - 1 do
	Array.iter 
	  (fun a -> 
	     Array.iter (fun f -> prn i a ; Array.iter (prn i) a#prec ; pr (i + 1)  f ; endl()) a#add ;
	     Array.iter (fun f -> prn i a ; Array.iter (prn i) a#prec ; prn (i + 1) f ; endl()) a#del
	  ) pdata#actions ;

	Array.iter 
	  (fun f ->
	     let rec aux f = function
	       | [] -> f() ; endl()
	       | prod :: l -> 
		   aux (fun () -> f() ; pr i prod) l ;
		   Array.iter (fun prec -> aux (fun () -> f() ; pr i prec) l) prod#prec
	     in
	       
	       aux (fun () -> pr i f ; prn (i + 1) f) (Array.to_list f#producers) ;
	       aux (fun () -> prn i f ; pr (i + 1) f) (Array.to_list f#deleters)
	  ) pdata#fluents ;

	Utils.array_prod_iter ( fun a1 a2 -> prn i a1 ; prn i a2 ; endl ()) pdata#actions
	      

(*
	Array.iter 
	  (fun f ->
	     Utils.array_prod_iter2 ( 
	       fun a1 a2 -> 
		 if a1 != a2 then begin
		   prn i a1 ; prn i a2 ; endl ()
		 end
	     ) f#consumers f#deleters
	  ) pdata#fluents
*)
      done ;

      Utils.eprint "Nombre de clauses : %i\n" (nbc - 1)

end
