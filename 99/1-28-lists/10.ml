let rec encode: string list -> (int * string) list = fun l ->
  let rec convert: string list -> (int * string) list = fun l ->
    match l with
    | [] -> []
    | h::t -> (1, h) :: (convert t)
  in
  let rec run_len: (int * string) list -> (int * string) list =
    fun l ->
      match l with
      | [] -> []
      | [h] -> [h]
      | (n, x)::(n_1, y)::t -> if x = y
          then run_len ((n + 1, x) :: t)
          else (n, x) :: run_len ((n_1, y)::t)
  in
  run_len @@ convert l

let rec encode_improve: string list -> (int * string) list = 
  fun list -> 
  let rec aux = fun l count acc ->
    match l with
    | [] -> []
    | [h] -> (count + 1, h)::acc
    | a::(b:: _ as t) -> if a = b then aux (count + 1) acc t
                                  else aux 0 ((count + 1, a):: acc) t in
  List.rev (aux 0 [] list)
    
