open Context
open Context.Monad
open Node

(* TYPE LEVELING *)

(* Update the context to ensure all the type variables present in a given type
  appear before the given other type variable. *)

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

(* TYPE FRESH *)

(* Check if a type contains any fresh variable. *)

let rec has_fresh type' =
  list_any (list_any has_fresh_base) type'.dnf

and has_fresh_base type' =
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
    list_any has_fresh tuple.elems
  | Record record ->
    map_any has_fresh_attr record.attrs
  | Lam lam ->
    let* param = has_fresh lam.param in
    let* ret   = has_fresh lam.ret in
    return (param || ret)
  | Univ univ ->
    let* param = has_fresh_param univ.param in
    let* ret   = with_param_rigid univ.param (has_fresh univ.ret) in
    return (param || ret)
  | Abs abs ->
    let* param = has_fresh_param abs.param in
    let* body  = with_param_rigid abs.param (has_fresh abs.body) in
    return (param || body)
  | App app ->
    let* abs = has_fresh app.abs in
    let* arg = has_fresh app.arg in
    return (abs || arg)

and has_fresh_attr attr =
  has_fresh attr.type'

and has_fresh_param param =
  let* lower = has_fresh param.lower in
  let* upper = has_fresh param.upper in
  return (lower || upper)

(* TYPE CONTAIN *)

(* Check if a type variable appears in a given type. *)

let rec occurs bind type' =
  list_any (list_any (occurs_base bind)) type'.dnf

and occurs_base bind type' =
  match type' with
  | Top | Bot | Unit | Bool | Int | String ->
    return false
  | Var var ->
    return (var.bind == bind)
  | Tuple tuple ->
    list_any (occurs bind) tuple.elems
  | Record record ->
    map_any (occurs_attr bind) record.attrs
  | Lam lam ->
    let* param = occurs bind lam.param in
    let* ret   = occurs bind lam.ret   in
    return (param || ret)
  | Univ univ ->
    let* param = occurs_param bind univ.param in
    let* ret   = with_param_rigid univ.param (occurs bind univ.ret) in
    return (param || ret)
  | Abs abs ->
    let* param = occurs_param bind abs.param in
    let* body  = with_param_rigid abs.param (occurs bind abs.body) in
    return (param || body)
  | App app ->
    let* abs = occurs bind app.abs in
    let* arg = occurs bind app.arg in
    return (abs || arg)

and occurs_attr bind attr =
  occurs bind attr.type'

and occurs_param bind param =
  let* lower = occurs bind param.lower in
  let* upper = occurs bind param.upper in
  return (lower || upper)
