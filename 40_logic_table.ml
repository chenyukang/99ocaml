

type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr;;

let rec eval env = function
  | Var x -> List.assoc x env
  | Not e -> not(eval env e)
  | And(e1, e2) -> (eval env e1) && (eval env e2)
  | Or(e1, e2) -> (eval env e1) || (eval env e2);;

let rec make_table env vars exp =
  match vars with
  | [] -> [(List.rev env, eval env exp)]
  | v::tl ->
     make_table ((v, true)::env) tl exp @
       make_table ((v, false)::env) tl exp;;

let table vars exp = make_table [] vars exp;;


let a = Var "a" and b = Var "b" and c = Var "c" in
    table ["a"; "b"; "c"] (Or(And(a, Or(b,c)), Or(And(a,b), And(a,c))));;
