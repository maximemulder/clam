open Utils
open Model
open TypingSub

let check_subtype type' constraint' =
  if Bool.not (is_subtype_of type' constraint') then
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
  | TypeAbsExpr (params, type') ->
    List.iter check params;
    check type';
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
  | TypeAbs (params, type') ->
    List.iter check_param params;
    check type';
  | TypeApp (type', args) ->
    check_app type' args;

and check_attr attr =
  check attr.attr_type

and check_param param =
  check param.param_type

and check_app type' args =
  match snd type' with
  | TypeAbs (params, type') ->
    check_app_abs type' params args
  | _ -> TypingErrors.raise_type_app_kind type'

and check_app_abs type' params args =
  if List.compare_lengths params args != 0 then
    TypingErrors.raise_type_app_arity type' params args
  else
  let binds = List.combine params args in
  List.iter (fun (param, arg) -> check_subtype arg param.param_type) binds;
  check type'
