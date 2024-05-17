open Node
open Transform

let rec appears bind type' =
  match type' with
  | Var var when var.bind == bind ->
    true
  | type' ->
    fold (appears bind) (||) false type'
