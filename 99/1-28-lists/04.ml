let rec length: 'a list -> int = fun l ->
  match l with
  | [] -> 0
  | _::t -> 1 + (length t)
