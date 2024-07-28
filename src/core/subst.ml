open Ast
open Map

let rec subst ident other term =
  match term with
  | Var var when var.ident = ident ->
    other
  | term ->
    map (subst ident other) term
