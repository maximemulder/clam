open Node

let type_span type' =
  match type' with
  | TypeName    { span; _ } -> span
  | TypeProduct { span; _ } -> span
  | TypeLam     { span; _ } -> span
  | TypeUniv    { span; _ } -> span
  | TypeAbs     { span; _ } -> span
  | TypeApp     { span; _ } -> span
  | TypeInter   { span; _ } -> span
  | TypeUnion   { span; _ } -> span

let expr_span expr =
  match expr with
  | ExprUnit    { span; _ } -> span
  | ExprTrue    { span; _ } -> span
  | ExprFalse   { span; _ } -> span
  | ExprInt     { span; _ } -> span
  | ExprString  { span; _ } -> span
  | ExprName    { span; _ } -> span
  | ExprProduct { span; _ } -> span
  | ExprElem    { span; _ } -> span
  | ExprAttr    { span; _ } -> span
  | ExprPreop   { span; _ } -> span
  | ExprBinop   { span; _ } -> span
  | ExprAscr    { span; _ } -> span
  | ExprStmt    { span; _ } -> span
  | ExprIf      { span; _ } -> span
  | ExprLamAbs  { span; _ } -> span
  | ExprLamApp  { span; _ } -> span
  | ExprUnivAbs { span; _ } -> span
  | ExprUnivApp { span; _ } -> span
