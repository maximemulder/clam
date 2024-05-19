open Node
open Syn_transform

let rec appears bind type' =
  match type' with
  | Var var when var.bind == bind ->
    true
  | type' ->
    syn_fold (appears bind) (||) false type'
