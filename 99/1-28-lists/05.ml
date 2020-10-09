let rec rev: 'a list -> 'a list = fun l ->
  match l with
  | [] -> []
  | h::t -> rev t @ [h]

let rev': 'a list -> 'a list =
  let rec aux acc = function
    | [] -> acc
    | h::t -> aux (h::acc) t in
  aux [] list
