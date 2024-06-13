open Abt.Type
open Trans_syn

let rec rename bind other type' =
  match type' with
  | Var var when var.bind == bind ->
    Var { var with bind = other }
  | type' ->
    map (rename bind other) type'
