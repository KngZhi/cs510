let is_palindrome_v0: 'a list -> bool = fun l ->
  let rec rev = fun l ->
    match l with
    | [] -> []
    | h::t -> rev t @ [h] in
  l = rev l
    
    
let is_palindrome_v1: 'a list -> bool = fun l ->
  l = List.rev l
