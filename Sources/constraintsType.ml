type constraints_t = 
    NecessarlyBefore
  | ImmediatlyLeadsTo
  | Choice
  | Parallel
  | PossiblyBefore
  | EventuallyLeadsTo
  | Fill

(* List Union *) 
let union l1 l2 = List.fold_right (fun x l -> if List.mem x l then l else x::l) l1 l2

(* List Intersection *)
let intersect l1 l2 = List.filter (fun x -> List.mem x l2) l1 ;;

(* Print helpers *)
let print print_el l =
    print_string "{";
    (match l with 
	 [] -> ()
       | h::t -> 
	   print_el h;
	   List.iter (fun a -> print_string ", "; print_el a) t
    ); 
    print_string "}";;

let print_list f lst =
  let rec print_elements = function
  | [] -> ()
  | h::t -> f h; print_string ", "; print_elements t
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

(* ConstraintsType to String *)
let constraints_type_string c =
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
print_list (print_pair constraints_type_string print_atom_list) tuples_list;;

(* Hashtable to save actions by key *)
exception Not_found;;

let add_tbl tbl key data =
  let r =
    try Hashtbl.find tbl key
    with Not_found ->
      let r = ref [] in
      Hashtbl.add tbl key r;
       r in
    r := data :: !r

let add_tbl_action tbl (key:string) (value_lst: Atom.t list) = 
  if (Hashtbl.mem tbl key) then
    (* it exists *)
    let v = Hashtbl.find tbl key in
    let data = v @ value_lst in 
    Hashtbl.add tbl key data
   else
   (* if doesn't exists *)
   Hashtbl.add tbl key value_lst
   ;;