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

let rec outgoing_nodes g n =
  match g with
  | [] -> []
  | (cur, next)::t -> if cur = n 
    then next :: outgoing_nodes t n
    else outgoing_nodes t n

let outgoing_nodes (g:(int * int) list) (n:int): int list = 
  List.map snd @@ List.filter (fun (x, y) -> x = n) g

(* 
   The list of nodes of the graph without duplicates. The order of the
   nodes in the list is irrelevant.
   eg. nodes ex => [1,2,3,4]
*)
let rec nodes_help g =
  match g with
  | [] -> []
  | (cur, next)::t -> [cur; next] @ nodes_help t

let rec mem x l =
  match l with
  | [] -> false
  | h::t -> if h = x then true else mem x t
  | _ -> failwith "bad input"

let rec rem_dup g =
  match g with
  | [] -> []
  | x::t -> if mem x t then rem_dup t else x :: rem_dup t 

let rec nodes g = rem_dup (nodes_help g)

(* 
   Remove a node from the graph
   Eg. remove ex 2 =>  [(3, 1); (3, 4)] 
*)
let rec remove (g: (int*int) list) (n: int): ((int*int) list) = 
  match g with   
  | [] -> []   
  | (x,y)::t -> if (x=n) || (y =n) then remove t n else (x,y)::remove t n;;

(* Reachable nodes from a source node.
   Eg. reachale ex 3 => [1,4,2,3] *)
let rec reachable g n =
  match g with
  | [] -> []
  | (cur, next)::l -> 
    if cur = n
    then next :: reachable l n
    else if next = n then cur :: reachable l n 
    else reachable l n 
