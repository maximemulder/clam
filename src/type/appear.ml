open Node
open Syn

let rec appears bind type' =
  match type' with
  | Var var when var.bind == bind ->
    true
  | type' ->
    syn_fold (appears bind) (||) false type'
