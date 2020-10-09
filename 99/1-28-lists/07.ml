(* There is no nested list type in OCaml, so we need to define one
first. A node of a nested list is either an element, or a list of
  nodes. *) 
  type 'a node = One of 'a | Many of 'a node list;;

  let rec flatten: 'a node list -> 'a list = fun list ->
    match list with
    | [] -> []
    | One(h)::t -> h :: flatten t
    | Many(h)::t -> flatten h @ flatten h 