open Node

let type_span type' =
  match type' with
  | TypeTop    { span; _ } -> span
  | TypeBot    { span; _ } -> span
  | TypeUnit   { span; _ } -> span
  | TypeBool   { span; _ } -> span
  | TypeInt    { span; _ } -> span
  | TypeString { span; _ } -> span
  | TypeVar    { span; _ } -> span
  | TypeTuple  { span; _ } -> span
  | TypeRecord { span; _ } -> span
  | TypeInter  { span; _ } -> span
  | TypeUnion  { span; _ } -> span
  | TypeLam    { span; _ } -> span
  | TypeUniv   { span; _ } -> span
  | TypeAbs    { span; _ } -> span
  | TypeApp    { span; _ } -> span

let expr_span expr =
  match expr with
  | ExprUnit    { span; _ } -> span
  | ExprBool    { span; _ } -> span
  | ExprInt     { span; _ } -> span
  | ExprString  { span; _ } -> span
  | ExprBind    { span; _ } -> span
  | ExprTuple   { span; _ } -> span
  | ExprRecord  { span; _ } -> span
  | ExprElem    { span; _ } -> span
  | ExprAttr    { span; _ } -> span
  | ExprAscr    { span; _ } -> span
  | ExprIf      { span; _ } -> span
  | ExprLamAbs  { span; _ } -> span
  | ExprLamApp  { span; _ } -> span
  | ExprUnivAbs { span; _ } -> span
  | ExprUnivApp { span; _ } -> span
