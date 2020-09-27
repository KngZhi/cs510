(* Section 1 *)
type dTree = Node of char*dTree*dTree | Leaf of int

let tLeft = Node('w', Node('x', Leaf(2), Leaf(5)), Leaf(8))
let tRight = Node('w', Node('x', Leaf(2), Leaf(5)), Node('y', Leaf(7), Leaf(5)))
let tOne = Node('w', tLeft, tRight)

let rec dTree_height: dTree -> int = 
  fun t -> 
  match t with
  | Leaf(_) -> 0
  | Node(_, lt, rt) -> 1 + (max (dTree_height lt) (dTree_height rt))

let rec dTree_size: dTree -> int =
  fun t -> 
  match t with
  | Leaf(_) -> 1
  | Node(_, lt, rt) -> 1 + (dTree_size lt) + (dTree_size rt)

let rec dTree_paths: dTree -> int list list =
  fun t ->
    let rec map flag l =
      match l with
      | [] -> []
      | h::t -> [flag::h] @ map flag t in
    match t with
    | Leaf(_) -> [[]]
    | Node(_, lt, rt) -> 
      let l = map 0 @@ dTree_paths lt in
      let r = map 1 @@ dTree_paths rt in
    l @ r

  
let rec dTree_is_perfect: dTree -> bool =
  fun t ->
  match t with
  | Leaf(_) -> true
  | Node(_, lt, rt) -> (dTree_size lt) = (dTree_size rt)

let rec dTree_map: (char -> char) -> (int -> int) -> dTree -> dTree =
  fun f g t ->
  match t with
  | Leaf(x) -> Leaf (g x)
  | Node(y, lt, rt) -> Node ((f y), (dTree_map f g lt),  (dTree_map f g rt))

(* Section 2 *)
let char_list = ['x'; 'y'; 'z']
let l = [([0;0;0], 0); ([0;0;1], 1); ([0;1;0], 1); 
  ([0;1;1], 0); ([1;0;0], 1); ([1;0;1], 0); ([1;1;0], 0); ([1;1;1], 1)]

let graph = (char_list, l)

let rec list_to_tree: char list -> dTree =
  fun l ->
  match l with
  | [] -> Leaf(0)
  | h::t -> Node(h, list_to_tree t, list_to_tree t)

let rec replace_leaf_at: dTree -> (int list* int) list -> dTree = 
  fun t g ->
    let rec alter: (int list * int) list -> int list -> int -> int = 
      fun g l x ->
        match g with
        | [] -> x
        | (path, target)::t -> if path = l then target else alter t l x in
    let rec aux: dTree -> (int list * int) list -> int list -> dTree = 
      fun t g l ->
        match t with
        | Leaf(x) -> Leaf(alter g l x)
        | Node(x, lt, rt) -> Node(x, aux lt g (0::l), aux rt g (1::l)) in
  aux t g []

let rec bf_to_dTree = 
  fun bf ->
    let (char_l, g) = bf in
    replace_leaf_at (list_to_tree char_l) g