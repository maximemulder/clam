type span = Code.span

type bind_type = {
  name: string;
}

type type' =
  | Top    of top
  | Bot    of bot
  | Unit   of unit'
  | Bool   of bool'
  | Int    of int'
  | String of string'
  | Var    of var
  | Tuple  of tuple
  | Record of record
  | Lam    of lam
  | Univ   of univ
  | Abs    of abs
  | App    of app
  | Rec    of rec'
  | Union  of union
  | Inter  of inter

and top = {
  span: span;
}

and bot = {
  span: span;
}

and unit' = {
  span: span;
}

and bool' = {
  span: span;
}

and int' = {
  span: span;
}

and string' = {
  span: span;
}

and var = {
  span: span;
  bind: bind_type;
}

and tuple = {
  span: span;
  elems: type' list;
}

and record = {
  span: span;
  attrs: attr Util.NameMap.t;
}

and lam = {
  span: span;
  param: type';
  ret: type';
}

and univ = {
  span: span;
  param: param;
  ret: type';
}

and abs = {
  span: span;
  param: param;
  body: type';
}

and app = {
  span: span;
  abs: type';
  arg: type';
}

and rec' = {
  span: span;
  bind: bind_type;
  body: type';
}

and union = {
  span: span;
  left: type';
  right: type';
}

and inter = {
  span: span;
  left: type';
  right: type';
}

(* AUXILIARIES *)

and attr = {
  span: span;
  label: string;
  type': type';
}

and param = {
  span: span;
  bind: bind_type;
  lower: type';
  upper: type';
}
