open Code
open Ident

type term =
  | Bot
  | Top
  | Var    of var
  | Record of record
  | Attr   of attr
  | Group  of group
  | If     of if'
  | Ascr   of ascr
  | Abs    of abs
  | App    of app
  | Univ   of univ
  | Rec    of rec'
  (* | Not of not *)
  | Union  of union
  | Inter  of inter
  | Range  of range

and var = {
  span: span;
  ident: ident;
}

and record = {
  span: span;
  attrs: record_attr list;
}

and record_attr =  {
  tag: string;
  term: term;
}

and attr = {
  span: span;
  record: term;
  tag: string;
}

and group = {
  span: span;
  term: term;
}

and if' = {
  span: span;
  cond: term;
  then': term;
  else': term;
}

and ascr = {
  span: span;
  term: term;
  type': term;
}

and abs = {
  span: span;
  param: param;
  body: term;
}

and app = {
  span: span;
  abs: term;
  arg: term;
}

and univ = {
  span: span;
  param: param;
  body: term;
}

and rec' = {
  span: span;
  ident: ident;
  body: term;
}

and union = {
  span: span;
  left: term;
  right: term;
}

and inter = {
  span: span;
  left: term;
  right: term;
}

and range = {
  span: span;
  lower: term;
  upper: term;
}

and param = {
  ident: ident option;
  type': term option;
}
