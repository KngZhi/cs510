(* 
    Quiz 5 - C510
    Date: 22 Oct 2020
    Parterner: Parisa Fathololumi
*)

(* The fruit type: A for Apple, O for Orange and K for Kiwi *)
type fruit = A | O | K

(* A sample fruit basket *)             
let fb = [A;A;A;O;O;A;O;K;A;K;K]

(* The fruit counter type *) 
type fc = fruit list -> int

(* Three sample fruit counters *)
let rec apples = function
  | [] -> 0
  | A::t -> 1+apples t
  | _::t -> apples t

let rec oranges = function
  | [] -> 0
  | O::t -> 1+oranges t
  | _::t -> oranges t

let rec kiwis = function
  | [] -> 0
  | K::t -> 1+kiwis t
  | _::t -> kiwis t

(* Fruit counter combinators *)

let add_combinator fc1 fc2 =
  fun fb -> (fc1 fb) + (fc2 fb)

(* Exercise 1

What is the type of the following expression: 

add_combinator apples oranges

Answer: fruit list -> int

*)



(* Exercise 2 

Implement a function prod_combinator that combines two fruit counters
 by multiplying theur results 

*)


let mult_combinator fc1 fc2 =
  fun fb -> (fc1 fb) * (fc2 fb)


(* Exercise 3:

   Consider the following generic fruit counter combinator *)
let generic_combinator fc1 f =
  fun fb ->
    match fc1 fb with
    | n -> f n fb

(*
  Use it to give new implementations of:  
  add_combinator, and
  mult_combinator 
*)

let add_combinator' fc1 fc2 = 
  generic_combinator fc1 (fun n fb -> n + fc2 fb)

let mult_combinator' fc1 fc2 =
  generic_combinator fc1 (fun n fb -> n * fc2 fb)

(* 
   Exercise 4: implement a function that takes a list of fruit
 combinators and returns a fruit combinator that applies each of
 them returning a list of their individual results.

  For example,

# combine_fc fcl fb;;
- : int list = [5; 3; 5; 3]
*)

let fcl = [apples; oranges; apples; kiwis]

let rec combine_list : (fruit list -> int) list -> (fruit list -> int list) = 
  fun fcl fb -> 
    match fcl with
    | [] -> fun fb -> fb
    | h::t -> (h fb) :: (combine_list t fb)
