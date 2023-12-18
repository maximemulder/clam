type kind =
  | Type
  | Abs of kind * kind

let rec get_kind ctx (type': Type.type') =
  get_kind_union ctx type'

and get_kind_union ctx (union: Type.union) =
  get_kind_inter ctx (List.nth union.union 0)

and get_kind_inter ctx (inter: Type.inter) =
  get_kind_base ctx (List.nth inter.inter 0)

and get_kind_base ctx (type': Type.base) =
  match type' with
  | Var var ->
    let bound = TypeContext.get_bind_type ctx var.bind in
    get_kind ctx bound
  | Abs abs ->
    let param = get_kind ctx abs.param.bound in
    let body  = get_kind ctx abs.body in
    Abs (param, body)
  | App app ->
    get_kind_app ctx app
  | _ ->
    Type

and get_kind_app ctx app =
  match get_kind ctx app.abs with
  | Abs (_, body) ->
    body
  | Type ->
    invalid_arg "TypingKind.get_kind"
