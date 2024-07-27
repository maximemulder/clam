open Ast

let rec display term =
  match term with
  | Bot ->
    "Bot"
  | Top ->
    "Top"
  | Var var ->
    var.ident.name
  | Row row ->
    "{" ^ row.tag ^ ": " ^ display row.type' ^ "}"
  | Record record ->
    "{" ^ String.concat ", " (List.map (fun attr -> attr.tag ^ " = " ^ display attr.term) record.attrs) ^ "}"
  | Group group ->
    "(" ^ display group.body ^ ")"
  | If if' ->
    "if " ^ display if'.cond ^ " then " ^ display if'.then' ^ " else " ^ display if'.else'
  | Ascr ascr ->
    display ascr.body ^ ": " ^ display ascr.type'
  | Abs abs ->
    "("
      ^ (match abs.param.ident with Some ident -> ident.name | None -> "") ^ " : " ^
      (match abs.param.type' with Some type' -> display type' | None -> "") ^  ") " ^ display abs.body
  | App app ->
    display app.abs ^ " " ^ display app.arg
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
  else if interval.lower = Bot && interval.upper = Top then
    (* [interval] is the first universal interval *)
    "Type"
  else
    display interval.lower ^ " .. " ^ display interval.upper
