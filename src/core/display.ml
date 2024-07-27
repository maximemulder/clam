open Ast

let rec display term =
  match term with
  | Type ->
    "Type"
  | Bot ->
    "Bot"
  | Top ->
    "Top"
  | Var var ->
    var.ident.name
  | Group group ->
    "(" ^ display group.body ^ ")"
  | If if' ->
    "if " ^ display if'.cond ^ " then " ^ display if'.then' ^ " else " ^ display if'.else'
  | Union union ->
    display union.left ^ " | " ^ display union.right
  | Inter inter ->
    display inter.left ^ " & " ^ display inter.right
  | _ ->
    "?"
