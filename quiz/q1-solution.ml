(* Sample Directed Graph *)
(* 
  Quiz 1 
  17 Sep 2020
*)

(* Example of a directed graph represented as an adjacency list *)

let ex = [(1, 2); (2, 3); (3, 1); (3, 4)]
(*
  1 <------ 3
  |     /|\ |
  |     /   | 
 \/   /    \/
  2         4
*)

(* 
Eg. outgoing ex 3 => [1,4] 
*)

let rec outgoing_nodes (g:(int*int) list) (n:int) : int list =
   List.map snd @@ List.filter (fun (x,y) -> x=n) g

let rec outgoing_nodes' : (int*int) list -> int -> int list =
  fun g n ->
  match g with
  | [] -> []
  | (src,tgt)::t when src=n -> tgt::outgoing_nodes' t n
  | (src,_)::t  -> outgoing_nodes' t n

let rec outgoing_nodes'' g n =
  match g with
  | [] -> []
  | (src,tgt)::t ->
    if  src=n
    then tgt::outgoing_nodes'' t n
    else outgoing_nodes'' t n

(* 
   The list of nodes of the graph without duplicates. The order of the
   nodes in the list is irrelevant.
   eg. nodes ex => [1,2,3,4] 
*)

let rec rem_dups l =
  match l with
  | [] -> []
  | h::t ->
    if List.mem h t
    then rem_dups t
    else h::rem_dups t
             
let rec nodes g =
  rem_dups (List.flatten (List.map (fun (x,y) -> [x;y]) g))

(* solution to nodes using fold *)

let  nodes' g =
  List.fold_left
    (fun l (x,y) ->
       if List.mem x l
       then (if List.mem y l
             then l
             else y::l)
            else x::(if List.mem y l
             then l
             else y::l)
    )
    []
    g

let node'' g =
  let rec nodes_aux g =
  match g with
  | [] -> []
  | (x,y)::t -> x::y::nodes_aux t
  in rem_dups (nodes_aux g)

let rec nodes_aux g =
  match g with
  | [] -> []
  | (x,y)::t -> x::y::nodes_aux t

let nodes''' g = 
  rem_dups (nodes_aux g)
   

(* 
   Remove a node from the graph
   let ex = [(1, 2); (2, 3); (3, 1); (3, 4)]
   Eg. remove ex 2 =>  [(3, 1); (3, 4)] 
*)
let rec remove g n =
  match g with
  | [] -> []
  | (src,tgt)::t ->
    if src=n || tgt =n
    then remove t n
    else (src,tgt)::remove t n
let rec remove' g n =
  match g with
  | [] -> []
  | ((src,tgt) as coord)::t ->
    if src=n || tgt =n
    then remove' t n
    else coord::remove' t n

let remove'' g n =
  List.filter (fun (x,y) -> not (x=n || y=n)) g
 

(* Reachable nodes from a source node.
   Eg. reachale ex 3 => [1,4,2,3] *)

let rec diff k l =
  match k with
  | [] -> []
  | h::t ->
    if List.mem h l
    then diff t l
    else h:: diff t l
           
let rec reachable' visited current g =
  match current with
  | [] -> visited
  | h::t ->
    let next = outgoing_nodes g h
    in reachable' (h::visited) ((diff next visited) @ t) g

let reachable g n =
  reachable' [] [n] g
