type fruit = Apple | Orange | Kiwi
type fcounter = fruit list -> int


let fl1 = [Apple; Apple; Apple; Apple; Apple; Apple; Kiwi;]

let rec apple_c fl = 
  match fl with
  | [] -> 0
  | Apple::t -> 1 + apple_c t
  | _::t -> apple_c t


let rec orange_c fl = 
  match fl with
  | [] -> 0
  | Orange::t -> 1 + orange_c t
  | _::t -> orange_c t

let rec kiwi_c fl = 
  match fl with
  | [] -> 0
  | Kiwi::t -> 1 + kiwi_c t
  | _::t -> kiwi_c t
