open Context
open Context.Monad
open Node

(* TYPE LEVELING *)

let rec levelize bind type' =
  list_iter (list_iter (levelize_base bind)) type'.dnf

and levelize_base bind type' =
  match type' with
  | Top | Bot | Unit | Bool | Int | String ->
    return ()
  | Var var ->
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
    let* () = levelize bind lam.ret in
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

(* TYPE APPEARS FRESH *)

let rec appears_fresh type' =
  list_any (list_any appears_fresh_base) type'.dnf

and appears_fresh_base type' =
  match type' with
  | Top | Bot | Unit | Bool | Int | String ->
    return false
  | Var var ->
    let* var = get_var var.bind in (
      match var with
      | Fresh _ ->
        return true
      | Rigid _ ->
        return false
    )
  | Tuple tuple ->
    list_any appears_fresh tuple.elems
  | Record record ->
    map_any appears_fresh_attr record.attrs
  | Lam lam ->
    let* param = appears_fresh lam.param in
    let* ret   = appears_fresh lam.ret in
    return (param || ret)
  | Univ univ ->
    let* param = appears_fresh_param univ.param in
    let* ret   = with_param_rigid univ.param (appears_fresh univ.ret) in
    return (param || ret)
  | Abs abs ->
    let* param = appears_fresh_param abs.param in
    let* body  = with_param_rigid abs.param (appears_fresh abs.body) in
    return (param || body)
  | App app ->
    let* abs = appears_fresh app.abs in
    let* arg = appears_fresh app.arg in
    return (abs || arg)

and appears_fresh_attr attr =
  appears_fresh attr.type'

and appears_fresh_param param =
  let* lower = appears_fresh param.lower in
  let* upper = appears_fresh param.upper in
  return (lower || upper)

(* TYPE APPEARS *)

let rec appears bind type' =
  list_any (list_any (appears_base bind)) type'.dnf

and appears_base bind type' =
  match type' with
  | Top | Bot | Unit | Bool | Int | String ->
    return false
  | Var var ->
    return (var.bind == bind)
  | Tuple tuple ->
    list_any (appears bind) tuple.elems
  | Record record ->
    map_any (appears_attr bind) record.attrs
  | Lam lam ->
    let* param = appears bind lam.param in
    let* ret   = appears bind lam.ret   in
    return (param || ret)
  | Univ univ ->
    let* param = appears_param bind univ.param in
    let* ret   = with_param_rigid univ.param (appears bind univ.ret) in
    return (param || ret)
  | Abs abs ->
    let* param = appears_param bind abs.param in
    let* body  = with_param_rigid abs.param (appears bind abs.body) in
    return (param || body)
  | App app ->
    let* abs = appears bind app.abs in
    let* arg = appears bind app.arg in
    return (abs || arg)

and appears_attr bind attr =
  appears bind attr.type'

and appears_param bind param =
  let* lower = appears bind param.lower in
  let* upper = appears bind param.upper in
  return (lower || upper)
