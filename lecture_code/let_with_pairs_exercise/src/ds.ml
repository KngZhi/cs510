


type 'a result = Ok of 'a | Error of string

let return : 'a -> 'a result =
  fun v -> Ok v

let error : string -> 'a result =
  fun s -> Error s

let (>>=) : 'a result -> ('a -> 'b result) -> 'b result =
  fun c f ->
  match c with
  | Error s -> Error s
  | Ok v -> f v
              
type exp_val =
  | NumVal of int
  | BoolVal of bool
  | PairVal of exp_val*exp_val

let val_of_PairVal: exp_val -> (exp_val*exp_val) result =
  fun ev ->
  match ev with
  | PairVal(v1, v2) -> return (v1, v2)
  | _ -> error "Expect a pair"

let int_of_numVal :  exp_val -> int result =
  fun ev ->
  match ev with
  | NumVal n -> return n
  | _ -> error "Expected a number"

let bool_of_boolVal :  exp_val -> bool result =
  fun ev ->
  match ev with
  | BoolVal b -> return b
  | _ -> error "Expected a boolean"


type env =
  | EmptyEnv
  | ExtendEnv of string*exp_val*env

(*
                   [x:=4, y:=true]

ExtendEnv("x",NumVal 4,ExtendEnv("y",BoolVal true,EmptyEnv))
*)

let empty_env () =
  EmptyEnv
    
let rec apply_env : string -> env -> exp_val result =
  fun id env ->
  match env with
  | EmptyEnv -> error "Id not found"
  | ExtendEnv(id',v,tail) ->
    if id=id'
    then return v
    else apply_env id tail

let extend_env =
  fun en id ev ->
  ExtendEnv(id,ev,en)
