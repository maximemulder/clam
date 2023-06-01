open Model
open TypingContext

module Reader = struct
  type r = context
end

open Monad.Monad(Monad.ReaderMonad(Reader))

let rec apply_type type' =
  match type' with
  | TypeAny _ ->
    return type'
  | TypeVoid _ ->
    return type'
  | TypeBool _ ->
    return type'
  | TypeInt _ ->
    return type'
  | TypeChar _ ->
    return type'
  | TypeString _ ->
    return type'
  | TypeVar var ->
    apply_var var
  | TypeAbsExpr abs ->
    let* params = map_list apply_type abs.type_abs_expr_params in
    let* ret = apply_type abs.type_abs_expr_ret in
    return (TypeAbsExpr {
      abs with
      type_abs_expr_params = params;
      type_abs_expr_ret = ret;
    })
  | TypeAbsExprType abs ->
    let* params = map_list apply_param abs.type_abs_expr_type_params in
    let* body = apply_type abs.type_abs_expr_type_body in
    return (TypeAbsExprType {
      abs with
      type_abs_expr_type_params = params;
      type_abs_expr_type_body = body;
    })
  | TypeTuple tuple ->
    let* types = map_list apply_type tuple.type_tuple_types in
    return (TypeTuple {
      tuple with
      type_tuple_types = types;
    })
  | TypeRecord record ->
    let* attrs = map_map apply_attr record.type_record_attrs in
    return (TypeRecord {
      record with
      type_record_attrs = attrs;
    })
  | TypeInter inter ->
    let* left = apply_type inter.type_inter_left in
    let* right = apply_type inter.type_inter_right in
    return (TypeInter {
      inter with
      type_inter_left = left;
      type_inter_right = right;
    })
  | TypeUnion union ->
    let* left = apply_type union.type_union_left in
    let* right = apply_type union.type_union_right in
    return (TypeUnion {
      union with
      type_union_left = left;
      type_union_right = right;
    })
  | TypeAbs abs ->
    let* params = map_list apply_param abs.type_abs_params in
    let* body = apply_type abs.type_abs_body in
    return (TypeAbs {
      abs with
      type_abs_params = params;
      type_abs_body = body;
    })
  | TypeApp app -> apply_app2 app

and apply_attr attr =
  let* type' = apply_type attr.attr_type in
  return { attr with attr_type = type' }

and apply_param param =
  let* type' = apply_type param.param_type in
  return { param with param_type = type' }

and apply_app2 app =
  let args = app.type_app_args in
  match app.type_app_type with
  | TypeAbs abs -> apply_app_abs abs.type_abs_body abs.type_abs_params args
  | TypeAbsExprType abs -> apply_app_abs abs.type_abs_expr_type_body abs.type_abs_expr_type_params args
  | _ -> TypingErrors.raise_unexpected ()

and apply_app_abs body params args context =
  let params = List.combine params args in
  let context = { parent = Some context; params } in
  apply_type body context

and apply_var var context =
  match List.find_opt (fun other -> (fst other) = var.type_var_param) context.params with
  | Some pair -> apply_type (snd pair) context
  | None ->
  match context.parent with
  | Some parent -> apply_var var parent
  | None -> TypeVar var

let apply type' context =
  apply_type type' context

let apply_app app context =
  apply_app2 app context
