let rec at: int -> string list -> string option = fun k l ->
  match l with
  | [] -> None
  | h::t -> if k = 1 then Some h else at (k - 1) t
    
