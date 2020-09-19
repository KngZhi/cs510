type program = int list

let square : program = [0; 2; 2; 3; 3; 4; 4; 5; 5; 1]
let rev_square = [0; 3; 3; 2; 2; 5; 5; 4; 4; 1]
let counter_square = [0;2;2;3;3;5;5;4;4;1]
let letter_e : program = [0; 2; 2; 3; 3; 5; 5; 4; 3; 5; 4; 3; 3; 5; 5; 1]

let rec colored_help: (int*int) -> program -> (int * int) list = 
  fun (x,y) l ->
  match l with
  | [] -> []
  | h::t -> 
    match h with
    | 2 -> [(x, y + 1)] @ colored_help (x, y + 1) t
    | 3 -> [(x + 1, y)] @ colored_help (x + 1, y) t
    | 4 -> [(x, y - 1)] @ colored_help (x, y - 1) t
    | 5 -> [(x - 1, y)] @ colored_help (x - 1, y) t
    | _ -> [(x, y)] @ colored_help(x, y) t

let rec rem_dup: (int*int) list -> (int*int) list = 
  fun l ->
  match l with
  | [] -> []
  | x::t -> if List.mem x t then rem_dup t else x:: rem_dup t

let colored: (int*int) -> program -> (int*int) list =
  fun coor image -> rem_dup @@ colored_help coor image


(* 2. equ when two programs have same set of coordinates. *)

let equivalent a b: bool =
  let equ_helper: ('a * 'a -> 'a) -> program -> int list = 
    fun f x -> List.sort compare @@ List.map f (colored (0, 0) x)
  in
  let a_x = equ_helper fst a in
  let a_y = equ_helper snd a in
  let b_x = equ_helper fst b in
  let b_y = equ_helper snd b in
  a_x = b_x && a_y = b_y

(* 3. returns a program that draws the mirror image *)
let mirror_helper: int -> int = fun x ->
  match x with
  | 2 -> 4
  | 4 -> 2
  | 3 -> 5
  | 5 -> 3
  | _ -> x

let mirror_image: program -> program = fun image -> List.map mirror_helper image
(* 4. rotate image 90 degrees*)

let rotate_image_90_helper:int -> int = fun x ->
  match x with
  | 2 -> 3
  | 3 -> 4
  | 4 -> 5
  | 5 -> 2
  | _ -> x

let rotate_image_90: program -> program = fun images ->
  List.map rotate_image_90_helper images

(* 5. repeat function *)
let rec repeat (r_times:int) (x:'a): 'a list =
  match r_times with
  | 0 -> []
  | _ -> x :: repeat (r_times - 1) x

(* 6. enlarge program x times *)
let rec pantograph (x:int) (images:program): program =
  match images with
  | [] -> []
  | h::t -> if h = 0 || h = 1 then [h] @ pantograph x t else repeat x h @ pantograph x t

let pantograph_m_helper (x_times:int) (item:int): int list =
  match item with
  | 0 -> [item]
  | 1 -> [item]
  | _ -> repeat x_times item

let pantograph_m (enlarge_times:int) (images:program): program = List.flatten @@ List.map (pantograph_m_helper enlarge_times) images

(* 6.2 pantograph_fold *)

let pantograph_f_helper: int -> int -> int list -> int list = fun x_times item acc ->
  match item with
  | 0 -> item :: acc
  | 1 -> item :: acc
  | _ -> repeat x_times item @ acc

let pantograph_f (enlarge_times:int) (images:program): program = 
  List.fold_right (pantograph_f_helper enlarge_times) images []

let rec compress_helper image = 
  match image with
  | [] -> []
  | [x] -> [x]
  | (x, y)::(x_1, y_1)::l -> 
    if x = x_1
    then compress_helper ((x, y+y_1)::l)
    else (x,y) :: compress_helper((x_1, y_1)::l)

let compress: program -> (int * int) list =
  fun image -> compress_helper (List.map (fun x -> (x, 1)) image)
(* 8. uncpmress *)
let rec uncompress: (int * int) list -> program =
  fun image ->
    match image with
    | [] -> []
    | (x, y)::t -> repeat y x @ uncompress t 

let uncompress_m_helper (x, y) = repeat y x

let uncompress_m image = List.flatten @@ List.map uncompress_m_helper image

let uncompress_f_helper (x, y) acc = repeat y x @ acc

let uncompress_f image = List.fold_right uncompress_f_helper image []
