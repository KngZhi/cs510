Open Ds

let rec eval_expr: expr -> int result =
  fun e ->
    match e with
    | Int(n) -> Ok n
    | Div(e1, e2)

