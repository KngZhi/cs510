let rec compress: 'a list -> 'a list = fun l ->
  match l with
  | [] -> []
  | [x] -> [x]
  | x::y::t -> if x = y then compress (y::t) else  x ::  compress (y :: t)