open Model

type entries = (param_type * type') list

let find_arg param entries =
  let entry = List.find_opt (fun entry -> fst entry == param) entries in
  Option.map snd entry

let param_entries (left: param_type) (right: param_type) pos =
  [(right, TypeVar { pos; param = left })]

module Reader = struct
  type r = entries
end

open Monad.Monad(Monad.ReaderMonad(Reader))

let rec apply (type': type') =
  match type' with
  | TypeTop    _ -> return type'
  | TypeBot    _ -> return type'
  | TypeUnit   _ -> return type'
  | TypeBool   _ -> return type'
  | TypeInt    _ -> return type'
  | TypeChar   _ -> return type'
  | TypeString _ -> return type'
  | TypeVar var ->
    apply_var var
  | TypeTuple tuple ->
    let* elems = map_list apply tuple.elems in
    return (TypeTuple { tuple with elems })
  | TypeRecord record ->
    let* attrs = map_map apply_attr record.attrs in
    return (TypeRecord { record with attrs })
  | TypeInter inter ->
    let* left = apply inter.left in
    let* right = apply inter.right in
    return (TypeInter { inter with left; right })
  | TypeUnion union ->
    let* left = apply union.left in
    let* right = apply union.right in
    return (TypeUnion { union with left; right })
  | TypeAbsExpr abs ->
    let* param = apply abs.param in
    let* body = apply abs.body in
    return (TypeAbsExpr { abs with param; body })
  | TypeAbsExprType abs ->
    let* param = apply_param abs.param in
    let body = apply_abs_expr_param abs param in
    let* body = apply body in
    return (TypeAbsExprType { abs with param; body })
  | TypeAbs abs ->
    let* param = apply_param abs.param in
    let body = apply_abs_param abs param in
    let* body = apply body in
    return (TypeAbs { abs with param; body })
  | TypeApp app ->
    let* type' = apply app.type' in
    let* arg = apply app.arg in
    return (TypeApp { app with type'; arg })

and apply_var var =
  let* arg = find_arg var.param in
  match arg with
  | Some arg ->
    return arg
  | None ->
    return (TypeVar var)

and apply_param param =
  let* type' = apply param.type' in
  return { param with type' }

and apply_attr attr =
  let* type' = apply attr.type' in
  return { attr with type' }

and apply_abs_expr_param abs param =
  let entries = param_entries param abs.param abs.pos in
  apply abs.body entries

and apply_abs_param abs param =
  let entries = param_entries param abs.param abs.pos in
  apply abs.body entries

let rec apply_app (app: type_app) =
  match apply_app_type app.type' with
  | Some abs ->
    let entries = [(abs.param, app.arg)] in
    apply abs.body entries
  | _ -> TypingErrors.raise_unexpected ()

and apply_app_type (type': type'): type_abs option =
  match type' with
  | TypeVar var ->
    apply_app_type var.param.type'
  | TypeAbs abs ->
    Some abs
  | TypeApp app ->
    apply_app_type (apply_app app)
  | _ ->
    None
