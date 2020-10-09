type 'a rle = One of 'a | Many of int * 'a

let rec encode: string list -> 'a rle list = fun l ->
  let rec convert: string list -> (int * string) list = fun l ->
    match l with
    | [] -> []
    | h::t -> (1, h) :: (convert t)
  in
  let rec run_len: (int * string) list -> 'a rle list =
    fun l ->
      match l with
      | [] -> []
      | [(n, x)] -> if n > 2 then [(Many (n, x))] else [(One x)]
      | (n, x)::(n_1, y)::t -> if x = y
          then run_len ((n + 1, x) :: t)
          else 
          if n >= 2
          then Many (n, x) :: run_len ((n_1, y)::t)
          else (One x) :: run_len ((n_1, y)::t)
  in
  run_len @@ convert l;;


let encode' list =
  let rec aux count acc = function
    | [] -> [] (* Can only be reached if original lit is empty *)
    | [x] -> (count+1, x) :: acc
    | a :: (b :: _ as t) -> 
        if a = b 
        then aux (count + 1) acc t
        else aux 0 ((count+1,a) :: acc) t in
  let trans = fun h ->
    let (n, x) = h in if n > 1 then Many (n, x) else One x in
    
  List.map trans @@ List.rev (aux 0 [] list);;



encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];; 

encode' ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];; 
