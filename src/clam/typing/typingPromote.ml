open Model

let rec promote type' =
  match snd type' with
  | TypeVar param -> promote param.param_type
  | _ -> type'
