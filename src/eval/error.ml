open Abt.Expr
open Abt.Span

type error = {
  span: Code.span;
  message: string;
}

exception Error of error

let raise span message =
  raise (Error { span; message })

let raise_bool (expr: expr) =
  raise (expr_span expr) "Expected boolean."

let raise_int (expr: expr) =
  raise (expr_span expr) "Expected integer."

let raise_string (expr: expr) =
  raise (expr_span expr) "Expected string."

let raise_tuple (expr: expr) =
  raise (expr_span expr) "Expected tuple."

let raise_record (expr: expr) =
  raise (expr_span expr) "Expected record."

let raise_lam (expr: expr) =
  raise (expr_span expr) "Expected lambda."
