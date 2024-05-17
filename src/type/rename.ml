open Node
open Transform

let rec rename bind other type' =
  match type' with
  | Var var when var.bind == bind ->
    Var { bind = other }
  | type' ->
    map (rename bind other) type'
