type program = int list

let square : program = [0; 2; 2; 3; 3; 4; 4; 5; 5; 1]
let letter_e : program = [0; 2; 2; 3; 3; 5; 5; 4; 3; 5; 4; 3; 3; 5; 5; 1]

let draw_with_end (x:program): program = [0] @ x @ [1]

(* return the list of coordinates that 
 * the program has colored using its pen.
 * TODO: remove dulp
 * XXX:  implement at `ex.ml` *)
let rec colored (x, y) (l:program) =
    match l with
    | [] -> []
    | h::t -> 
              match h with
              | 2 -> [(x + h, y)] @ colored (x, y) t
              | 3 -> [(x, y + h)] @ colored (x, y) t
              | 4 -> [(x + h, y)] @ colored (x, y) t
              | 5 -> [(x, y + h)] @ colored (x, y) t
              | _ -> [(x + h, y + h)] @colored(x, y) t


let equivalent (x: program) (y: program): bool =
        true

let rec mirror_image (l: program): program =
  match l with
  | [] -> []
  | h::t ->
    match h with
    | 2 -> [4] @ mirror_image t
    | 4 -> [2] @ mirror_image t
    | 3 -> [5] @ mirror_image t
    | 5 -> [3] @ mirror_image t
    | _ -> [h] @ mirror_image t

(* 
let mirror_image' x =
    match h with
    | 2 -> [4]
    | 4 -> [2]
    | 3 -> [5]
    | 5 -> [3]
    | _ -> [h] *)
