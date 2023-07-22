open Utils
open Model

module Reader = struct
  type r = TypingContext.context
end

open Monad.Monad(Monad.ReaderMonad(Reader))

let rec validate_subtype type' constr =
  let* () = validate type' in
  let* sub = TypingSub.is_subtype type' constr in
  if Bool.not sub then
    TypingErrors.raise_type_constraint type' constr
  else
    return ()

and validate_proper type' =
  let* () = validate type' in
  match type' with
  | TypeAbs _ -> TypingErrors.raise_type_proper type'
  | TypeApp app ->
    let type' = TypingApply.apply_app app in
    validate_proper type'
  | _ ->
    return ()

and validate type' =
  match type' with
  | TypeTop _ ->
    return ()
  | TypeUnit _ ->
    return ()
  | TypeBool _ ->
    return ()
  | TypeInt _ ->
    return ()
  | TypeChar _ ->
    return ()
  | TypeString _ ->
    return ()
  | TypeVar _ ->
    return ()
  | TypeAbsExpr abs ->
    let* () = iter_list validate abs.type_abs_expr_params in
    validate abs.type_abs_expr_body;
  | TypeAbsExprType abs ->
    let* () = iter_list validate_param abs.type_abs_expr_type_params in
    validate abs.type_abs_expr_type_body;
  | TypeTuple tuple ->
    iter_list validate tuple.type_tuple_types
  | TypeRecord record ->
    validate_record record
  | TypeInter inter ->
    let* () = validate inter.type_inter_left in
    validate inter.type_inter_right;
  | TypeUnion union ->
    let* () = validate union.type_union_left in
    validate union.type_union_right;
  | TypeAbs abs ->
    let* () = iter_list validate_param abs.type_abs_params in
    validate abs.type_abs_body;
  | TypeApp app ->
    validate_app app;

and validate_record record context =
  NameMap.iter (fun _ attr -> validate_attr attr context) record.type_record_attrs

and validate_app app =
  match app.type_app_type with
  | TypeAbs abs ->
    valiate_app_abs app abs
  | _ -> TypingErrors.raise_type_app_kind app.type_app_type

and valiate_app_abs app abs =
  let params = abs.type_abs_params in
  let args = app.type_app_args in
  let body = abs.type_abs_body in
  if List.compare_lengths params args != 0 then
    TypingErrors.raise_type_app_arity app abs
  else
  let entries = List.combine params args in
  let* () = iter_list (fun (param, arg) -> validate_subtype arg param.param_type) entries in
  validate body;

and validate_param param =
  validate param.param_type

and validate_attr attr =
  validate attr.attr_type
