open Model

module Reader = struct
  type r = TypingContext.context
end

open Monad.Monad(Monad.ReaderMonad(Reader))

let rec validate_subtype type' constr =
  let* () = validate type' in
  let* sub = Typing.isa type' constr in
  if Bool.not sub then
    TypingErrors.raise_subtype_constraint type' constr
  else
    return ()

and validate_suptype type' constr =
  let* () = validate type' in
  let* sub = Typing.isa constr type' in
  if Bool.not sub then
    TypingErrors.raise_suptype_constraint type' constr
  else
    return ()

and validate_proper (type': type') =
  let* () = validate type' in
  match type' with
  | TypeAbs _ -> TypingErrors.raise_type_proper type'
  | TypeApp app ->
    let type' = TypingApp.apply_app app in
    validate_proper type'
  | _ ->
    return ()

and validate (type': type') =
  match type' with
  | TypeTop _ ->
    return ()
  | TypeBot _ ->
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
    let* () = validate abs.param in
    validate abs.body;
  | TypeAbsExprType abs ->
    let* () = validate_param abs.param in
    validate abs.body;
  | TypeTuple tuple ->
    iter_list validate tuple.elems
  | TypeRecord record ->
    validate_record record
  | TypeInter inter ->
    let* () = validate inter.left in
    validate inter.right;
  | TypeUnion union ->
    let* () = validate union.left in
    validate union.right;
  | TypeAbs abs ->
    let*() = validate_param abs.param in
    validate abs.body;
  | TypeApp app ->
    validate_app app;

and validate_record record context =
  Utils.NameMap.iter (fun _ attr -> validate_attr attr context) record.attrs

and validate_app app =
  match validate_app_type app.type' with
  | Some abs ->
    valiate_app_abs app abs
  | None ->
    TypingErrors.raise_type_app_kind app.type'

and validate_app_type type' =
  match type' with
  | TypeVar var ->
    validate_app_type var.param.type'
  | TypeApp app ->
    validate_app_type (TypingApp.apply_app app)
  | TypeAbs abs ->
    Some abs
  | _ ->
    None

and valiate_app_abs app abs =
  let* () = validate_subtype app.arg abs.param.type' in
  validate abs.body

and validate_attr attr =
  validate attr.type'

and validate_param param =
  validate param.type'
