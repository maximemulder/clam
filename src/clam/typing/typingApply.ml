open Model

type entries = (param_type * type') list

let find_arg param entries =
  let entry = List.find_opt (fun entry -> fst entry = param) entries in
  Option.map snd entry

module Reader = struct
  type r = entries
end

open Monad.Monad(Monad.ReaderMonad(Reader))

let rec apply type' =
  match type' with
  | TypeAny _ ->
    return type_any
  | TypeVoid _ ->
    return type_void
  | TypeBool _ ->
    return type_bool
  | TypeInt _ ->
    return type_int
  | TypeChar _ ->
    return type_char
  | TypeString _ ->
    return type_string
  | TypeVar var ->
    apply_var var
  | TypeTuple tuple ->
    let* types = map_list apply tuple.type_tuple_types in
    return (TypeTuple { tuple with
      type_tuple_types = types
    })
  | TypeRecord record ->
    let* attrs = map_map apply_attr record.type_record_attrs in
    return (TypeRecord { record with
      type_record_attrs = attrs
    })
  | TypeInter inter ->
    let* left = apply inter.type_inter_left in
    let* right = apply inter.type_inter_right in
    return (TypeInter { inter with
      type_inter_left = left;
      type_inter_right = right;
    })
  | TypeUnion union ->
    let* left = apply union.type_union_left in
    let* right = apply union.type_union_right in
    return (TypeUnion { union with
      type_union_left = left;
      type_union_right = right;
    })
  | TypeAbsExpr abs ->
    let* params = map_list apply abs.type_abs_expr_params in
    let* ret = apply abs.type_abs_expr_ret in
    return (TypeAbsExpr { abs with
      type_abs_expr_params = params;
      type_abs_expr_ret = ret;
    })
  | TypeAbsExprType abs ->
    let* params = map_list apply_param abs.type_abs_expr_type_params in
    let* body = apply abs.type_abs_expr_type_body in
    return (TypeAbsExprType { abs with
      type_abs_expr_type_params = params;
      type_abs_expr_type_body = body;
    })
  | TypeAbs abs ->
    let* params = map_list apply_param abs.type_abs_params in
    let* body = apply abs.type_abs_body in
    return (TypeAbs { abs with
      type_abs_params = params;
      type_abs_body = body;
    })
  | TypeApp app ->
    let* type' = apply app.type_app_type in
    let* args = map_list apply app.type_app_args in
    return (TypeApp { app with
      type_app_type = type';
      type_app_args = args;
    })

and apply_var var =
  let* arg = find_arg var.type_var_param in
  match arg with
  | Some arg ->
    return arg
  | None ->
    return (TypeVar var)

and apply_param param =
  let* type' = apply param.param_type in
  return { param with param_type = type' }

and apply_attr attr =
  let* type' = apply attr.attr_type in
  return { attr with attr_type = type' }

let apply_app app =
  match app.type_app_type with
  | TypeAbs abs ->
    if List.compare_lengths abs.type_abs_params app.type_app_args != 0 then
      TypingErrors.raise_unexpected ()
    else
    let pairs = List.combine abs.type_abs_params app.type_app_args in
    apply abs.type_abs_body pairs
  | _ -> TypingErrors.raise_unexpected ()
