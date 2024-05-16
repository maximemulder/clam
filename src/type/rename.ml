open Map.Map(Util.Monad.Identity)
open Node
open Util.Monad.Identity

let rec rename bind other type' =
  match type' with
  | Var var when var.bind == bind ->
    return (Var { bind = other })
  | type' ->
    map (rename bind other) type'
