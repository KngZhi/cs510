let rec last: 'a list -> 'a option = fun l ->
  match l with
  | [] -> None
  | [x] -> Some x
  | _::t -> last t
