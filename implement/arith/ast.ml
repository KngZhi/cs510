type expr = 
  | Int of int
  | Sub of (expr * expr)
  | Div of (expr * expr)
  | Div of (expr * expr)

type 'a result = Ok of 'a | Error of string

let return: 'a -> 'a result = 
  fun v -> Ok v

let (>>=): 'a result -> ('a -> b' result) -> 'b result =
  fun c f ->
  match c with
  | Erorr s -> Error s
  | Ok v -> f v
