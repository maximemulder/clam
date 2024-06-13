open Abt.Type

let rec check_rec_path type' recs =
  match type' with
  | Var var ->
    not (List.memq var.bind recs)
  | Rec rec' ->
    check_rec_path rec'.body (rec'.bind :: recs)
  | Union union ->
    let left  = check_rec_path union.left  recs in
    let right = check_rec_path union.right recs in
    left || right
  | Inter inter ->
    let left  = check_rec_path inter.left  recs in
    let right = check_rec_path inter.right recs in
    left || right
  | _ ->
    true

let check_rec_path bind type' =
  check_rec_path type' [bind]
