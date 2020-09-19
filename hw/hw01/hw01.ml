type program = int list

let square : program = [0; 2; 2; 3; 3; 4; 4; 5; 5; 1]
let rev_square = [0; 3; 3; 2; 2; 5; 5; 4; 4; 1]
let counter_square = [0;2;2;3;3;5;5;4;4;1]
let letter_e : program = [0; 2; 2; 3; 3; 5; 5; 4; 3; 5; 4; 3; 3; 5; 5; 1]

let rec map f l =
  match l with
  | [] -> []
  | x::xs -> f x :: map f xs

let rec fold_right f l a =
  match l with
  | [] -> a
  | (x::xs) -> f x (fold_right f xs a)

let rec flatten = function [] -> [] 
  | l::r -> l @ flatten r

let rec colored_help (x,y) l = 
  match l with
  | [] -> []
  | h::t -> 
    match h with
    | 2 -> [(x, y + 1)] @ colored_help (x, y + 1) t
    | 3 -> [(x + 1, y)] @ colored_help (x + 1, y) t
    | 4 -> [(x, y - 1)] @ colored_help (x, y - 1) t
    | 5 -> [(x - 1, y)] @ colored_help (x - 1, y) t
    | _ -> [(x, y)] @ colored_help(x, y) t

let rec rem_dup l =
  match l with
  | [] -> []
  | x::t -> if List.mem x t then rem_dup t else x:: rem_dup t

let colored coor image = rem_dup @@ colored_help coor image


(* 2. equ when two programs have same set of coordinates. *)
let helper f x= List.sort compare @@ List.map f (colored (0, 0) x)

let equivalent a b: bool =
  let a_x = helper fst a in
  let a_y = helper snd a in
  let b_x = helper fst b in
  let b_y = helper snd b in
  a_x = b_x && a_y = b_y

(* 3. returns a program that draws the mirror image *)
let mirror_image' x =
  match x with
  | 2 -> 4
  | 4 -> 2
  | 3 -> 5
  | 5 -> 3
  | _ -> x

let mirror_image image = map mirror_image' image

(* 4. rotate image 90 degrees*)

let rotate_image_90_helper x =
  match x with
  | 2 -> 3
  | 3 -> 4
  | 4 -> 5
  | 5 -> 2
  | _ -> x

let rotate_image_90 images = map rotate_image_90_helper images

(* 5. repeat function *)
let rec repeat (r_times:int) x =
  match r_times with
  | 0 -> []
  | _ -> x :: repeat (r_times - 1) x

(* 6. enlarge program x times *)
let rec pantograph (x:int) (images:program) =
  match images with
  | [] -> []
  | h::t -> if h = 0 || h = 1 then [h] @ pantograph x t else repeat x h @ pantograph x t

let pantograph_m_helper x_times item = 
  match item with
  | 0 -> [item]
  | 1 -> [item]
  | _ -> repeat x_times item

let pantograph_m (enlarge_times:int) (images:program) = flatten (map (pantograph_m_helper enlarge_times) images)

(* 6.2 pantograph_fold *)

let pantograph_f_helper x_times item acc =
  match item with
  | 0 -> item :: acc
  | 1 -> item :: acc
  | _ -> repeat x_times item @ acc

let pantograph_f (enlarge_times:int) (images:program) = 
  fold_right (pantograph_f_helper enlarge_times) images []

let rec compress_helper image = 
  match image with
  | [] -> []
  | [x] -> [x]
  | (x, y)::(x_1, y_1)::l -> 
    if x = x_1
    then compress_helper ((x, y+y_1)::l)
    else (x,y) :: compress_helper((x_1, y_1)::l)

let compress image = 
  compress_helper (map (fun x -> (x, 1)) image)
(* 8. uncpmress *)
let rec uncompress image =
  match image with
  | [] -> []
  | (x, y)::t -> repeat y x @ uncompress t 

let uncompress_m_helper (x, y) = repeat y x

let uncompress_m image = flatten(map uncompress_m_helper image)

let uncompress_f_helper (x, y) acc = repeat y x @ acc

let uncompress_f image = fold_right uncompress_f_helper image []
