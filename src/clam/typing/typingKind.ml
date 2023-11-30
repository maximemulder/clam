open Model

type kind =
  | Type
  | Abs of kind * kind

let rec get_kind type' =
  match type' with
  | TypeVar var -> get_kind var.param.bound
  | TypeInter inter ->
    let left = get_kind inter.left in
    let right = get_kind inter.right in
    if left = right then
      left
    else
    invalid_arg "Typing.get_kind"
  | TypeUnion union ->
    let left = get_kind union.left in
    let right = get_kind union.right in
    if left = right then
      left
    else
    invalid_arg "Typing.get_kind"
  | TypeAbs abs ->
    let param = get_kind abs.param.bound in
    let body = get_kind abs.body in
    Abs (param, body)
  | TypeApp app ->
    (match get_kind app.type' with
      | Abs (_, body) -> body
      | Type -> invalid_arg "TypingKind.get_kind"
    )
  | _ -> Type
