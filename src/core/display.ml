open Ast

let rec display term =
  match term with
  | Bot               -> display_bot
  | Top               -> display_top
  | Var var           -> display_var var
  | Row row           -> display_row row
  | Record record     -> display_record record
  | Group group       -> display_group group
  | If if'            -> display_if if'
  | Ascr ascr         -> display_ascr ascr
  | Abs abs           -> display_abs abs
  | App app           -> display_app app
  | Univ univ         -> display_univ univ
  | Rec rec'          -> display_rec rec'
  | Union union       -> display_union union
  | Inter inter       -> display_inter inter
  | Interval interval -> display_interval interval

and display_bot =
  "Bot"

and display_top =
  "Top"

and display_var var =
  var.ident.name

and display_row row =
  "{" ^ row.tag ^ ": " ^ display row.type' ^ "}"

and display_record record =
  "{" ^ String.concat ", " (List.map display_record_attr record.attrs) ^ "}"

and display_record_attr attr =
  attr.tag ^ " = " ^ display attr.term

and display_group group =
  "(" ^ display group.body ^ ")"

and display_if if' =
  "if " ^ display if'.cond
    ^ " then " ^ display if'.then'
    ^ " else " ^ display if'.else'

and display_ascr ascr =
  display ascr.body ^ ": " ^ display ascr.type'

and display_abs abs =
  "(" ^ display_param abs.param ^ ") " ^ display abs.body

and display_app app =
  display app.abs ^ " " ^ display app.arg

and display_univ univ =
  "[" ^ display_param univ.param ^ "] " ^ display univ.body

and display_rec rec' =
  rec'.ident.name ^ ". " ^ display rec'.body

and display_union union =
  display union.left ^ " | " ^ display union.right

and display_inter inter =
  display inter.left ^ " & " ^ display inter.right

and display_interval interval =
  (* Is [interval] a singleton interval ? *)
  if interval.lower = interval.upper then
    ":" ^ display interval.lower
  else
    display interval.lower ^ " .. " ^ display interval.upper

and display_param param =
  display_param_ident param ^ " : " ^ display_param_type' param

and display_param_ident param =
  match param.ident with
  | Some ident -> ident.name
  | None -> ""

and display_param_type' param =
  match param.type' with
  | Some type' -> display type'
  | None -> ""
