open Ds
open Ast


let rec eval_expr : expr -> int result =
  fun e ->
  match e with
  | Int n -> return n
  | Sub(e1,e2) ->
    eval_expr e1 >>= fun n ->
    eval_expr e2 >>= fun m ->
    return (n -m )
  | Abs(e) -> 
    eval_expr e >>= fun n -> 
    if n >= 0 then return n else return (- n)
  | Div(e1,e2) ->
      eval_expr e1 >>= fun n ->
      eval_expr e2 >>= fun m ->
      return (n / m )
  | _ -> failwith "not implemented"
              

(* parse a string into an ast *)
let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(* Interpret an expression *)
let interp (e:string) : int result   =
  let c = e |> parse |> eval_expr
  in c
