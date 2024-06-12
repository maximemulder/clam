let type_span type' =
  let open Type in
  match type' with
  | Top    { span; _ } -> span
  | Bot    { span; _ } -> span
  | Unit   { span; _ } -> span
  | Bool   { span; _ } -> span
  | Int    { span; _ } -> span
  | String { span; _ } -> span
  | Var    { span; _ } -> span
  | Tuple  { span; _ } -> span
  | Record { span; _ } -> span
  | Inter  { span; _ } -> span
  | Union  { span; _ } -> span
  | Lam    { span; _ } -> span
  | Univ   { span; _ } -> span
  | Rec    { span; _ } -> span
  | Abs    { span; _ } -> span
  | App    { span; _ } -> span

let expr_span expr =
  let open Expr in
  match expr with
  | Unit    { span; _ } -> span
  | Bool    { span; _ } -> span
  | Int     { span; _ } -> span
  | String  { span; _ } -> span
  | Bind    { span; _ } -> span
  | Tuple   { span; _ } -> span
  | Record  { span; _ } -> span
  | Elem    { span; _ } -> span
  | Attr    { span; _ } -> span
  | Ascr    { span; _ } -> span
  | If      { span; _ } -> span
  | LamAbs  { span; _ } -> span
  | LamApp  { span; _ } -> span
  | UnivAbs { span; _ } -> span
  | UnivApp { span; _ } -> span
