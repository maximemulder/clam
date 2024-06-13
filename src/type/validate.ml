open Abt.Type
open Check_rec_path
open Context
open Context.Monad
open System

(*
  There is currently a bug when recursive types are incorrectly kinded, but I
  won't fix it for now since I will probably change the kind system later.

  Example of bugged term: List. [T] => None | List
*)

let rec validate_proper type' =
  let* () = validate type' in
  let* cond = is_proper type' in
  if not cond then
    Error.validate_proper type'
  else
  return ()

and validate (type': type') =
  match type' with
  | Top _ | Bot _ | Unit _ | Bool _ | Int _ | String _ | Var _ -> return ()
  | Tuple tuple   -> validate_tuple tuple
  | Record record -> validate_record record
  | Lam lam       -> validate_lam lam
  | Univ univ     -> validate_univ univ
  | Abs abs       -> validate_abs abs
  | App app       -> validate_app app
  | Rec rec'      -> validate_rec rec'
  | Union union   -> validate_union union
  | Inter inter   -> validate_inter inter

and validate_tuple tuple =
  let* () = list_iter validate_proper tuple.elems in
  return ()

and validate_record record =
  let* () = map_iter validate_record_attr record.attrs in
  return ()

and validate_record_attr attr =
  let* () = validate_proper attr.type' in
  return ()

and validate_lam lam =
  let* () = validate_proper lam.param in
  let* () = validate_proper lam.ret   in
  return ()

and validate_univ univ =
  validate_param_with univ.param (validate_proper univ.ret)

and validate_abs abs =
  validate_param_with abs.param (validate abs.body)

and validate_app app =
  let* () = validate app.abs in
  let* () = validate app.arg in
  let* lower, upper = validate_app_param app.abs in
  let* lower_cond = System.isa lower app.arg in
  let* upper_cond = System.isa app.arg upper in
  if not lower_cond || not upper_cond then
    Error.validate_app_arg app lower upper
  else
  return ()

and validate_rec rec' =
  if check_rec_path rec'.bind rec'.body then failwith ("TODO: Infinite recursive type.") else
  with_def rec'.bind ((Var { span = rec'.span; bind = rec'.bind })) (validate rec'.body)

and validate_union union =
  let* () = validate union.left  in
  let* () = validate union.right in
  let* kind_left  = Kind.get_kind union.left  in
  let* kind_right = Kind.get_kind union.right in
  let* cond = System.is_kind kind_left kind_right in
  if not cond then
    Error.validate_union_kind union
  else
  return ()

and validate_inter inter =
  let* () = validate inter.left  in
  let* () = validate inter.right in
  let* kind_left  = Kind.get_kind inter.left  in
  let* kind_right = Kind.get_kind inter.right in
  let* cond = System.is_kind kind_left kind_right in
  if not cond then
    Error.validate_inter_kind inter
  else
  return ()

and validate_app_param abs =
  match abs with
  | Var var ->
    let* var = get_var var.bind in
    (match var with
    | Fresh fresh ->
      (* TODO: Check this case. *)
      validate_app_param fresh.lower
    | Rigid rigid ->
      (* Both bounds have the same kind in a well-formed type variable. *)
      validate_app_param rigid.lower)
  | Abs abs ->
    return (abs.param.lower, abs.param.upper)
  | Union union ->
    (* Both operands have the same kind in a well-formed union. *)
    validate_app_param union.left
  | Inter inter ->
    (* Both operands have the same kind in a well-formed intersection. *)
    validate_app_param inter.left
  | _ ->
    invalid_arg "validate_app_param"

and validate_param (param: param) =
  let* () = validate param.lower in
  let* () = validate param.upper in
  let* cond = System.isa param.lower param.upper in
  if not cond then
    Error.validate_param param
  else
  return ()

and validate_param_with param f =
  let* () = validate_param param in
  with_param_rigid param f
