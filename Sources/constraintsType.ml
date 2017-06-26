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

let print_atom_name (atom1: Atom.t) :unit =
  print_string atom1#pred#to_string;;

let print_atom_list_name (at: Atom.t list): unit=
  print_list print_atom_name at;;

let cons_list_atom_name (atom: Atom.t) : string list =
  let l:string list = [] in
 atom#pred#to_string::[] ;;

let rec append_list_atom_name (at: Atom.t list) =
    match at with 
    | [] -> []
    | hd :: l -> List.append (cons_list_atom_name hd) (append_list_atom_name l) ;;


let rec print_list_2 (lst:string list) = 
  match lst with
  | [] ->  print_string "."
  | h::t -> print_string h ; print_string ", ";  print_list_2 t 
  ;;


  let rec find_index_of y l =
      match l with
      | [] -> -1
      | h :: t -> if y = h then 0 else 1 + find_index_of y t
      ;;

  let term_index_pos (atom: Atom.t) (term: Symb.term) = 
      find_index_of term (Array.to_list atom#terms);;

  
  let action_index_pos action action_lst = 
      find_index_of action action_lst;;




let print_atom_list_name2 (at: Atom.t list)=
  print_list_2 (append_list_atom_name at);;

 let params_atom_list (a : Symb.term array) =
  Array.to_list a ;;    

 


let print_atom (atom_elemt: Atom.t) : unit = 
  print_string atom_elemt#to_string

let print_atom_list (atom_elemts: Atom.t list) : unit = 
  print_list print_atom atom_elemts;;


let print_atom_tuple tuples_list =
  print_list (print_pair constraints_type_string print_atom_list) tuples_list;;
(* Hashtable to save actions by key *)

let add_tbl tbl key data =
  let r =
    try Hashtbl.find tbl key
    with Not_found ->
      let r = ref [] in
      Hashtbl.add tbl key r;
       r in
    r := data :: !r

let add_tbl_action tbl key value_lst = 
   Hashtbl.add tbl key value_lst ;;

(* Returns ConstraintsType as a String *)
let constraints_type_to_string c =
  match c with
  NecessarlyBefore -> "NecessarlyBefore"
  | ImmediatlyLeadsTo -> "ImmediatlyLeadsTo"
  | Choice -> "Choice"
  | Parallel -> "Parallel"
  | PossiblyBefore -> "PossiblyBefore"
  | EventuallyLeadsTo -> "EventuallyLeadsTo"
  | Fill -> "Fill";;

let tbl_to_list tbl =
  Hashtbl.fold (fun key r accu -> (fun key l accu -> (key, l) :: accu) key !r accu) tbl []