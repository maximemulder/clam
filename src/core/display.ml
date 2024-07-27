open Ast

let rec display term =
  match term with
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
  | Interval interval ->
    display_interval interval
  | _ ->
    "?"

and display_interval interval =
  if interval.lower = interval.upper then
    (* [interval] is a singleton interval *)
    ":" ^ display interval.lower
  else
    (* [type'] is a non-singleton interval *)
    display interval.lower ^ " .. " ^ display interval.upper
