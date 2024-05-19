open Node
open Syn_transform

let rec rename bind other type' =
  match type' with
  | Var var when var.bind == bind ->
    Var { bind = other }
  | type' ->
    syn_map (rename bind other) type'
