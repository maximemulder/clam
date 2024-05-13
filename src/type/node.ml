type type' =
  | Top
  | Bot
  | Unit
  | Bool
  | Int
  | String
  | Var    of var
  | Tuple  of tuple
  | Record of record
  | Lam    of lam
  | Univ   of univ
  | Abs    of abs
  | App    of app
  | Union  of union
  | Inter  of inter

and var = {
  bind: Abt.bind_type;
}

and tuple = {
  elems: type' list;
}

and record = {
  attrs: attr Util.NameMap.t;
}

and lam = {
  param: type';
  ret: type';
}

and univ = {
  param: param;
  ret: type';
}

and abs = {
  param: param;
  body: type';
}

and app = {
  abs: type';
  arg: type';
}

and attr = {
  label: String.t;
  type': type';
}

and param = {
  bind: Abt.bind_type;
  lower: type';
  upper: type';
}

and union = {
  left:  type';
  right: type';
}

and inter = {
  left:  type';
  right: type';
}
