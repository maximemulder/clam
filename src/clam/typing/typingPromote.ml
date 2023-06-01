open Model

let rec promote type' =
  match type' with
  | TypeVar var -> promote_var var
  | _ -> type'

and promote_var var =
  promote var.type_var_param.param_type
