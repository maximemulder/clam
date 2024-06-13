open Abt.Type
open Context
open Context.Monad

(* TYPE LEVELING *)

(* TODO: Factorize *)
let rec levelize bind type' =
  match type' with
  | Top _ | Bot _ | Unit _ | Bool _ | Int _ | String _ ->
    return ()
  | Var var ->
    if var.bind == bind then
      return ()
    else
    let* var = get_var var.bind in
    (match var with
    | Fresh fresh ->
      let* () = reorder bind fresh.bind in
      let* () = levelize fresh.bind fresh.lower in
      let* () = levelize fresh.bind fresh.upper in
      return ()
    | Rigid _ ->
      return ())
  | Tuple tuple ->
    list_iter (levelize bind) tuple.elems
  | Record record ->
    map_iter (levelize_attr bind) record.attrs
  | Lam lam ->
    let* () = levelize bind lam.param in
    let* () = levelize bind lam.ret   in
    return ()
  | Univ univ ->
    let* () = levelize_param bind univ.param in
    let* () = with_param_rigid univ.param (levelize bind univ.ret) in
    return ()
  | Abs abs ->
    let* () = levelize_param bind abs.param in
    let* () = with_param_rigid abs.param (levelize bind abs.body) in
    return ()
  | App app ->
    let* () = levelize bind app.abs in
    let* () = levelize bind app.arg in
    return ()
  | Rec rec' ->
    with_rec_rigid rec' (levelize bind rec'.body)
  | Union union ->
    let* () = levelize bind union.left  in
    let* () = levelize bind union.right in
    return ()
  | Inter inter ->
    let* () = levelize bind inter.left  in
    let* () = levelize bind inter.right in
    return ()

and levelize_attr bind attr =
  levelize bind attr.type'

and levelize_param bind param =
  let* () = levelize bind param.lower in
  let* () = levelize bind param.upper in
  return ()

let levelize (fresh: fresh) type' =
  let* () = levelize fresh.bind type' in
  let* () = levelize fresh.bind fresh.lower in
  let* () = levelize fresh.bind fresh.upper in
  return ()
