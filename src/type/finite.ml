open Node

let rec check type' recs =
  match type' with
  | Rec rec' ->
    check rec'.body (rec'.bind :: recs)
  | Var var ->
    not (List.memq var.bind recs)
  | Union union ->
    let left  = check union.left  recs in
    let right = check union.right recs in
    left || right
  | Inter inter ->
    let left  = check inter.left  recs in
    let right = check inter.right recs in
    left || right
  | _ ->
    true
