(* This file defines expressed values and environments *)


(* expressed values and environments are defined mutually recursively *)

type exp_val =
  | NumVal of int
  | BoolVal of bool
  | PairVal of exp_val * exp_val
      
type env =
  | EmptyEnv
  | ExtendEnv of string*exp_val*env

(* result of evaluation *)
                 
type 'a result = Ok of 'a | Error of string

let return : 'a -> 'a result = fun v ->
   Ok v

let error : string -> 'a result = fun s ->
  Error s

let (>>=) : 'a result -> ('a -> 'b result) -> 'b result = fun c f ->
  match c with
  | Error s -> Error s
  | Ok v -> f v

(* Operations on environments *)

let empty_env : unit -> env = fun () ->
  EmptyEnv

let rec lookup (dict:('b*'c) list) (key:'b) : 'c option =
  match dict with
  | [] -> None
  | (k,v)::t ->
    if k=key
    then Some v
    else lookup t key

let extend_env : env -> string -> exp_val -> env = fun env id v ->
  ExtendEnv(id,v,env)

let rec apply_env : string -> env -> exp_val result = fun id env ->
  match env with
  | EmptyEnv -> error @@ id^" not found!"
  | ExtendEnv(v,ev,tail) ->
    if id=v
    then return ev
    else apply_env id tail

(* operations on expressed values *)

let int_of_numVal : exp_val -> int result =  function
  |  NumVal n -> return n
  | _ -> error "Expected a number!"

let bool_of_boolVal : exp_val -> bool result =  function
  |  BoolVal b -> return b
  | _ -> error "Expected a boolean!"
           
let string_of_expval = function
  | NumVal n -> "NumVal " ^ string_of_int n
  | BoolVal b -> "BoolVal " ^ string_of_bool b

let rec string_of_env  = function
  | EmptyEnv -> ""
  | ExtendEnv(id,v,env) -> string_of_env env^"\n("^id^","^string_of_expval v^")"

let pair_of_pairVal : exp_val -> (exp_val*exp_val) result = function
  | PairVal(v1,v2) -> return (v1,v2)
  | _ -> error "Expected a pair!"