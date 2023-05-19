open Collection
open Model
open TypingSub

let check_subtype type' constraint' =
  if Bool.not (is_subtype_of type' constraint')
    then TypingErrors.raise_type type' constraint'
    else ()

let rec check type' =
  match type' with
  | TypeAny ->
    ()
  | TypeVoid ->
    ()
  | TypeBool ->
    ()
  | TypeInt ->
    ()
  | TypeChar ->
    ()
  | TypeString ->
    ()
  | TypeVar _  ->
    ()
  | TypeAbsExpr (params, type') ->
    let _ = List.iter check params in
    check type'
  | TypeAbsExprType (params, type') ->
    let _ = List.iter check_param params in
    check type'
  | TypeTuple types ->
    List.iter check types
  | TypeRecord attrs ->
    NameMap.iter (fun _ attr -> check_attr attr) attrs
  | TypeInter (left, right) ->
    let _ = check left in
    check right
  | TypeUnion (left, right) ->
    let _ = check left in
    check right
  | TypeAbs (params, type') ->
    let _ = List.iter check_param params in
    check type'
  | TypeApp (type', args) -> check_app type' args

and check_attr attr =
  check attr.attr_type

and check_param param =
  check param.param_type

and check_app type' args =
  match type' with
  | TypeAbs (params, type') ->
    check_app_abs type' params args
  | _ -> TypingErrors.raise_type_app_kind type'

and check_app_abs type' params args =
  let length_params = List.length params in
  let length_args = List.length args in
  if length_params != length_args then
    TypingErrors.raise_type_app_arity length_params length_args
  else
  let binds = List.combine params args in
  let _ = List.iter (fun (param, arg) -> check_subtype arg param.param_type) binds in
  check type'
