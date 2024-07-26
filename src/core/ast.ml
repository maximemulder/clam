open Code
open Ident

type term =
  | Type
  | Bot
  | Top
  | Var    of var
  | Row    of row
  | Record of record
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

and group = {
  span: span;
  body: term;
}

and if' = {
  span: span;
  cond: term;
  then': term;
  else': term;
}

and ascr = {
  span: span;
  body: term;
  type': term;
}

and var = {
  span: span;
  ident: ident;
}

and row = {
  span: span;
  tag: string;
  type': term;
}

and record = {
  span: span;
  attrs: attr list;
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

and attr = {
  tag: string;
  value: term;
}

and param = {
  span: span;
  ident: ident option;
  type': term option;
}
