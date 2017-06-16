  type constraints_t = 
    NecessarlyBefore
  | ImmediatlyLeadsTo
  | Choice
  | Parallel
  | PossiblyBefore
  | EventuallyLeadsTo
  | Fill

let print_list f lst =
let rec print_elements = function
| [] -> ()
| h::t -> f h; print_string ";"; print_elements t
in
print_string "[";
print_elements lst;
print_string "]";;

let print_pair f g (a, b) =
print_string "(";
f a;
print_string ", ";
g b;
print_string ")";;

let print_constraints_type c =
match c with
NecessarlyBefore -> print_string "NecessarlyBefore"
| ImmediatlyLeadsTo -> print_string "ImmediatlyLeadsTo"
| Choice -> print_string "Choice"
| Parallel -> print_string "Parallel"
| PossiblyBefore -> print_string "PossiblyBefore"
| EventuallyLeadsTo -> print_string "EventuallyLeadsTo"
| Fill -> print_string "Fill";;

let print_atom (atom_elemt: Atom.t) : unit = 
print_string atom_elemt#to_string

let print_atom_list (atom_elemts: Atom.t list) : unit = 
print_list print_atom atom_elemts;;

let print_atom_tuple tuples_list =
print_list (print_pair print_constraints_type print_atom_list) tuples_list;;
