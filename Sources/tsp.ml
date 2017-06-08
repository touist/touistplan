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
    (String.map (fun c -> if c=='-' then '_' else c) atom#pred#to_string) ^ 
    if atom#nb_terms = 0 then "" else "_" ^  (Utils.string_of_array "_" Utils.to_string atom#terms)

  method to_string = string
end


and action name params duration quality prec nprec add del =
object (self)
  inherit node_common
  inherit [fluent] Node.action name params duration quality prec nprec add del

  val string = 
    name ^ if Array.length params = 0 then "" else "_" ^ (Utils.string_of_array "_" Utils.to_string params)

  method to_string = (String.map (fun c -> if c=='-' then '_' else c) string)

  method to_complete_string = 
  let changedash s = (String.map (fun c -> if c=='-' then '_' else c) s) in
    let string_of_fluent_array fluents =
      (String.map (fun c -> if c=='-' then '_' else c) (Utils.string_of_array "," Utils.to_string fluents)) in
      "$Cond(" ^ (changedash string) ^ ") = [" ^ string_of_fluent_array prec ^ "]\n" ^
      "$Add(" ^ (changedash string) ^ ") = [" ^ string_of_fluent_array add ^ "]\n" ^
      "$Del(" ^ (changedash string) ^ ") = [" ^ string_of_fluent_array del ^ "]\n"
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


class t =
object (self)
  inherit [fluent, action, plan] PlanningData.t  as pdata
  inherit [fluent, action, plan] tsp_common

  method print_statistics = ()
  method run = self#plan_fail

  method print =
  
  
  
  
  
  
    let changedash s = (String.map (fun c -> if c=='-' then '_' else c) s) in
    let string_of_fluent_array fluents = Utils.string_of_array "," Utils.to_string fluents in
    Utils.print "begin sets\n\n$length = %d\n\n$I = [%s]" 2 (string_of_fluent_array self#init_state);
    Utils.print "\n\n$G = [%s]" (string_of_fluent_array self#goal);
    Utils.print "\n\n$O = [%s]" (string_of_fluent_array self#actions);
    Utils.print "\n\n$F = [%s]" (string_of_fluent_array self#fluents);
    Utils.print "\n\n$Fp = [%s]\n" (Utils.string_of_list "," Utils.to_string (Array.fold_left (fun l f -> if f#consumers <> [| |] then f::l else l) [] self#fluents));
    Utils.print "\n$Fa = [%s]\n" (Utils.string_of_list "," Utils.to_string (Array.fold_left (fun l f -> if f#producers <> [| |] then f::l else l) [] self#fluents));
    Utils.print "\n$Fd = [%s]\n" (Utils.string_of_list "," Utils.to_string (Array.fold_left (fun l f -> if f#deleters <> [| |] then f::l else l) [] self#fluents));
    Utils.print "\n" ;
    Array.iter (fun a -> Utils.print "\n%s" a#to_complete_string; (* ) self#actions ;
    Array.iter (fun a -> *)
      Array.iter (fun (f,timedata) ->
          Utils.print "$t_cond_begin(%s,%s) = %f\n" (changedash f#to_string) (changedash a#to_string) (fst timedata#timeset);
          Utils.print "$t_cond_end(%s,%s) = %f\n" (changedash f#to_string) (changedash a#to_string) (snd timedata#timeset)
      ) a#iprec ;
      Array.iter (fun (f,timedata) ->
          Utils.print "$t_add_begin(%s,%s) = %f\n" (changedash a#to_string) (changedash f#to_string) (fst timedata#timeset);
          Utils.print "$t_add_end(%s,%s) = %f\n" (changedash a#to_string) (changedash f#to_string) (snd timedata#timeset)
      ) a#iadd ;
      Array.iter (fun (f,timedata) ->
          Utils.print "$t_del_begin(%s,%s) = %f\n" (changedash a#to_string) (changedash f#to_string) (fst timedata#timeset);
          Utils.print "$t_del_end(%s,%s) = %f\n" (changedash a#to_string) (changedash f#to_string) (snd timedata#timeset)
      ) a#idel ;
    ) self#actions ;
    Utils.print "\nend sets\n\n";
    
    Utils.print "begin formula\n\nbigand $f in $I: $f(0) end\nbigand $f in diff($F,$I): not $f(0) end\nbigand $f in $G: $f($length) end\nbigand $i in [1..$length]:\n  bigand $a in $O:\n    ($a($i) =>\n      ((bigand $f in $Cond($a): $f($i-1) end)\n        and\n        (bigand $f in $Add($a): $f($i) end)\n        and\n        (bigand $f in $Del($a): (not $f($i)) end)))\n  end\nend\nbigand $i in [1..$length]:\n  bigand $f in $F:\n    ($f($i-1) and not $f($i))\n    => (bigor $a in $O when $f in $Del($a): $a($i) end)\n  end\nend\nbigand $i in [1..$length]:\n  bigand $f in $F:\n    (not $f($i-1) and $f($i))\n    => (bigor $a in $O when $f in $Add($a): $a($i) end)\n  end\n
end\nbigand $i in [1..$length]:\n  bigand $a1 in $O:\n    bigand $f in $Cond($a1):\n      bigand $a2 in $O when ($a1 != $a2) and ($f in $Del($a2)):\n        (not $a1($i) or not $a2($i))\n      end\n    end\n  end\nend\n\nend formula\n\n";

end
