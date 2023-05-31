open Utils
open Model
open TypingSub

let check_subtype type' constraint' =
  if Bool.not (is_subtype type' constraint') then
    TypingErrors.raise_type_constraint type' constraint'
  else
    ()

let rec check type' =
  match snd type' with
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
  | TypeAbsExpr abs ->
    List.iter check abs.type_abs_expr_params;
    check abs.type_abs_expr_ret;
  | TypeAbsExprType (params, type') ->
    List.iter check_param params;
    check type';
  | TypeTuple types ->
    List.iter check types;
  | TypeRecord attrs ->
    NameMap.iter (fun _ attr -> check_attr attr) attrs;
  | TypeInter (left, right) ->
    check left;
    check right;
  | TypeUnion (left, right) ->
    check left;
    check right;
  | TypeAbs abs ->
    List.iter check_param abs.type_abs_params;
    check abs.type_abs_body;
  | TypeApp app ->
    check_app app;

and check_attr attr =
  check attr.attr_type

and check_param param =
  check param.param_type

and check_app app =
  match snd app.type_app_type with
  | TypeAbs abs ->
    check_app_abs abs app.type_app_args
  | _ -> TypingErrors.raise_type_app_kind app.type_app_type

and check_app_abs abs args =
  let params = abs.type_abs_params in
  let body = abs.type_abs_body in
  if List.compare_lengths params args != 0 then
    TypingErrors.raise_type_app_arity body params args
  else
  let binds = List.combine params args in
  List.iter (fun (param, arg) -> check_subtype arg param.param_type) binds;
  check body;
