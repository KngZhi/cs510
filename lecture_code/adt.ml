(* Algebraic Data Types *)


(* Simple example: enumeration types *)

type pcolor = Red | Green | Blue  

(* Constructors (Ref, Green, Blue) start with a capital letter *)
              
let next (c:pcolor) : pcolor =
  match c with
  | Red -> Green
  | Green -> Blue
  | Blue -> Red


(* Another example where the constructors have arguments *)

          
type student = UGrad of string*int*int | Grad of string*int*int
                                                
let example : student list =
  [UGrad("Tom",25,3);
   UGrad("Jane",22,5);
   Grad("Susan",27,7)]
  
let rec no_of_ugrads (l:student list) : int =
  match l with
  | [] -> 0
  | UGrad(_,_,_)::t -> 1 + no_of_ugrads t                         
  | _::t -> no_of_ugrads t


let name (st:student) : string =
  match st with
  | UGrad(n,_,_) -> n
  | Grad(n,_,_) -> n

(* Example of a polymorphic ADT *)
    
type 'a option = Some of 'a | None



let rec lookup (dict:('b*'c) list) (key:'b) : 'c option =
  match dict with
  | [] -> None
  | (k,v)::t ->
    if k=key
    then Some v
    else lookup t key

type ('a,'b) either = Left of 'a | Right of 'b

(* Binary Trees *)

type 'a btree = Empty | Node of 'a*'a btree*'a btree

let t1 :int btree =
  Node(7,
       Node(4,Empty,Empty),
       Node(12,
            Node(10,Empty,Empty),
            Empty))

let t2 : string btree =
  Node("hello",
       Node("there",Empty,Empty),
       Node("bye",Empty,Empty))

let rec sizet (t:'a btree) : int =
  match t with
  | Empty -> 0
  | Node(i,lt,rt) -> 1 + sizet lt + sizet rt

let rec sumt (t:int btree) : int =
  match t with
  | Empty -> 0
  | Node(i,lt,rt) -> i + sumt lt + sumt rt

let rec bumpt (t:int btree) : int btree =
  match t with
  | Empty -> Empty
  | Node(i,lt,rt) -> Node(i+1,bumpt lt,bumpt rt)

let rec mirror (t:'a btree) : 'a btree =
  match t with
  | Empty -> Empty
  | Node(i,lt,rt) -> Node(i,mirror rt, mirror lt)

let rec mapt : ('a -> 'b) -> 'a btree -> 'b btree =
  fun f t ->
  match t with
  | Empty -> Empty
  | Node(i,lt,rt) -> Node(f i,mapt f lt, mapt f rt)
