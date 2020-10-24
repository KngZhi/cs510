open Ast
open Ds

let rec eval_expr : expr -> env -> exp_val result = fun e en ->
  match e with
  | Int(n) ->
    return @@ NumVal n
  | Var(id) ->
    apply_env id en
  | Add(e1,e2) ->
    eval_expr e1 en >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 en >>=
    int_of_numVal >>= fun n2 ->
    return @@ NumVal (n1+n2)
  | Sub(e1,e2) ->
    eval_expr e1 en >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 en >>=
    int_of_numVal >>= fun n2 ->
    return @@ NumVal (n1-n2)
  | Mul(e1,e2) ->
    eval_expr e1 en >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 en >>=
    int_of_numVal >>= fun n2 ->
    return @@ NumVal (n1*n2)
  | Div(e1,e2) ->
    eval_expr e1 en >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 en >>=
    int_of_numVal >>= fun n2 ->
    return @@ NumVal (n1/n2)
  | Abs(e) ->
    eval_expr e en >>=
    int_of_numVal >>= fun i ->
    return (NumVal (abs i))
  | Let(v,def,body) ->
    eval_expr def en >>= fun ev -> 
    eval_expr body (extend_env en v ev) 
  | ITE(e1,e2,e3) ->
    eval_expr e1 en >>=
    bool_of_boolVal >>= fun b ->
    if b 
    then eval_expr e2 en
    else eval_expr e3 en
  | IsZero(e) ->
    eval_expr e en >>=
    int_of_numVal >>= fun n ->
    return @@ BoolVal (n = 0)
  | Not(e) ->
    eval_expr e en >>=
    bool_of_boolVal >>= fun b ->
    return (BoolVal (not(b)))
  | Pair(e1,e2) ->
    eval_expr e1 en >>= fun v1 ->
    eval_expr e2 en >>= fun v2 ->
    return (PairVal(v1, v2))
  | Unpair(id1, id2,e1, e2) ->
    eval_expr e1 en >>= pair_of_pairVal >>= fun (v1, v2) ->
    eval_expr e2 (extend_env (extend_env en id1 v1) id2 v2)
| Debug(_e) ->
    print_string "Environment:";
    print_endline @@ string_of_env en;
    return @@ NumVal 28
  | _ -> error "Not implemented yet!"


let eval_prog (AProg e) = eval_expr e



(***********************************************************************)
(* Everything above this is essentially the same as we saw in lecture. *)
(***********************************************************************)

(* Parse a string into an ast *)


let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(* Interpret an expression *)
let interp (s:string) : exp_val result =
 let c = s |> parse |> eval_prog
 in c EmptyEnv
