open Model

let rec promote (type': type') =
  match type' with
  | TypeVar var -> promote_var var
  | _ -> type'

and promote_var var =
  promote var.param.type'
