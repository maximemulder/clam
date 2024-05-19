type pol = Pos | Neg

let inv pol =
  match pol with
  | Neg -> Pos
  | Pos -> Neg
