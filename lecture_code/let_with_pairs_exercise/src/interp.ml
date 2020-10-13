open Ast
open Ds

         
let rec eval_expr : expr -> env -> exp_val result =
  fun e en ->
  match e with
  | Int n -> return (NumVal n)
  | Var id -> apply_env id en
  | Add(e1,e2) ->
    eval_expr e1 en >>= 
    int_of_numVal >>= fun v1 ->
    eval_expr e2 en >>= 
    int_of_numVal >>= fun v2 ->
    return (NumVal (v1+v2))
  | Div(e1,e2) ->
    eval_expr e1 en >>=
    int_of_numVal >>= fun n ->
    eval_expr e2 en >>=
    int_of_numVal >>= fun m ->
    if m=0
    then error "Division by zero"
    else return (NumVal (n/m))
  | IsZero(e) ->
    eval_expr e en >>=
    int_of_numVal >>= fun n ->
    return (BoolVal (n=0))
  | ITE(e1,e2,e3) ->
    eval_expr e1 en >>=
    bool_of_boolVal >>= fun b ->
    if b
    then eval_expr e2 en
    else eval_expr e3 en
  | Let(id,e1,e2) ->
    eval_expr e1 en >>= fun v ->
    eval_expr e2 (extend_env en id v)
  | Pair(e1,e2) ->
    eval_expr e1 en >>= fun v1 ->
    eval_expr e2 en >>= fun v2 ->
    return (PairVal(v1, v2))
  | Fst(e) ->
    eval_expr e en >>= fun p ->
    val_of_PairVal p >>= fun n ->
    return (fst n)
  | Snd(e) ->
    eval_expr e en >>= fun p ->
    val_of_PairVal p >>= fun n ->
    return (snd n)
  | _ -> failwith "not implemented"
              


(* Parse a string into an ast *)
let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(* Interpret an expression *)
let interp (s:string) : exp_val result =
  let c = s |> parse 
  in eval_expr c (empty_env ())
