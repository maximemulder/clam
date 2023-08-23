open Model

type entries = (param_type * type') list

let find_arg param entries =
  let entry = List.find_opt (fun entry -> fst entry = param) entries in
  Option.map snd entry

module Reader = struct
  type r = entries
end

open Monad.Monad(Monad.ReaderMonad(Reader))

let rec apply (type': type') =
  match type' with
  | TypeTop _ ->
    return type'
  | TypeBot _ ->
    return type'
  | TypeUnit _ ->
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
    let* params = map_list apply abs.params in
    let* body = apply abs.body in
    return (TypeAbsExpr { abs with params; body })
  | TypeAbsExprType abs ->
    let* params = map_list apply_param abs.params in
    let* body = apply abs.body in
    return (TypeAbsExprType { abs with params; body })
  | TypeAbs abs ->
    let* params = map_list apply_param abs.params in
    let* body = apply abs.body in
    return (TypeAbs { abs with params; body })
  | TypeApp app ->
    let* type' = apply app.type' in
    let* args = map_list apply app.args in
    return (TypeApp { app with type'; args })

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

let apply_app (app: type_app) =
  match app.type' with
  | TypeAbs abs ->
    if List.compare_lengths abs.params app.args != 0 then
      TypingErrors.raise_unexpected ()
    else
    let pairs = List.combine abs.params app.args in
    apply abs.body pairs
  | _ -> TypingErrors.raise_unexpected ()
