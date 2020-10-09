let rec last_two: string list -> (string * string) option = fun l ->
  match l with
  | [] -> None
  | [x; y] -> Some (x, y)
  | _::t -> last_two t
