open Model


let rec validate_subtype type' constr =
  let () = validate type' in
  let sub = Typing.isa type' constr in
  if Bool.not sub then
    TypingErrors.raise_subtype_constraint type' constr
  else
    ()

and validate_suptype type' constr =
  let () = validate type' in
  let sub = Typing.isa constr type' in
  if Bool.not sub then
    TypingErrors.raise_suptype_constraint type' constr
  else
    ()

and validate_proper (type': type') =
  let () = validate type' in
  match type' with
  | TypeAbs _ -> TypingErrors.raise_type_proper type'
  | TypeApp app ->
    let type' = TypingApp.apply_app app in
    validate_proper type'
  | _ ->
    ()

and validate (type': type') =
  match type' with
  | TypeTop _    -> ()
  | TypeBot _    -> ()
  | TypeUnit _   -> ()
  | TypeBool _   -> ()
  | TypeInt _    -> ()
  | TypeChar _   -> ()
  | TypeString _ -> ()
  | TypeVar _    -> ()
  | TypeAbsExpr abs ->
    validate abs.param;
    validate abs.body;
  | TypeAbsExprType abs ->
    validate_param abs.param;
    validate abs.body;
  | TypeTuple tuple ->
    List.iter validate tuple.elems
  | TypeRecord record ->
    validate_record record;
  | TypeInter inter ->
    validate inter.left;
    validate inter.right;
  | TypeUnion union ->
    validate union.left;
    validate union.right;
  | TypeAbs abs ->
    validate_param abs.param;
    validate abs.body;
  | TypeApp app ->
    validate_app app;

and validate_record record =
  Utils.NameMap.iter (fun _ attr -> validate_attr attr) record.attrs

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
  validate_subtype app.arg abs.param.type';
  validate abs.body

and validate_attr attr =
  validate attr.type'

and validate_param param =
  validate param.type'
