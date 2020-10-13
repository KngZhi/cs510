type 'a result = Ok of 'a | Error of string

let return : 'a -> 'a result =  fun v ->  Ok v
let error : string -> 'a result =  fun s ->  Error s

let (>>=) : 'a result -> ('a -> 'b result) -> 'b result =  
  fun c f ->  match c with  
  | Error s -> Error s  
  | Ok v -> f v