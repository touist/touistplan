type t = Top | PosLit of Atom.t | NegLit of Atom.t | Conjunct of t array


let rec to_string = function
  | Top -> "()"
  |  PosLit a -> a#to_string
  | NegLit a -> "(not " ^ a#to_string ^ ")"
  | Conjunct c -> "(and " ^ (Utils.string_of_array " " to_string c) ^ ")"


let rec simplify simplify_func = function
  | PosLit atom when atom#pred#typing ->
      simplify_func atom ;
      Top
  | Conjunct formulas -> begin
      match 
	Array.fold_right (fun f formulas -> 
			  match simplify simplify_func f with
			    | Top -> formulas
			    | formula -> formula :: formulas) formulas []
      with
	| [] -> Top
	| [formula] -> formula
	| formulas -> Conjunct (Array.of_list formulas)
    end
  | formula -> formula
