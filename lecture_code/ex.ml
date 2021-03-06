let rec mem x l =
        match x with
        | [] -> false
        | h::t -> if h = x then true else mem t
        | _ -> failwith "bad input"

let rec has_dupl l =
        match l with
        | [] -> false
        | h::t -> mem h t || has_dupl t

let rec is_singleton l =
        match l with
        | [x] -> true
        | _ -> false

let rec rev l =
        match l with
        | [] -> []
        | h::t -> rev t @ [h]

let rec rem_adj_dupl l =
        match l with
        | [] -> []
        | [x] -> [x]
        | h::t -> if mem h t then rem_adj_dupl t else [h] @ (rem_adj_dupl t)
