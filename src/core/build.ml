open Ast
open Ctx
open Ident
open Prim

let span = Code.span_primitive

let single term =
  Range { span; lower = term; upper = term }

let range lower upper =
  Range { span; lower; upper }

let bot =
  Bot

let top =
  Top

let var ident =
  Var { span; ident }

let record attrs =
  let attrs = List.map (fun (tag, term) -> { tag; term }) attrs in
  Record { span; attrs }

let attr record tag =
  Attr { span; record; tag }

let group term =
  Group { span; term }

let if' cond then' else' =
  If { span; cond; then'; else' }

let ascr term type' =
  Ascr { span; term; type' }

let abs name type' body =
  let ident = new_ident name in
  let body = body (var ident) in
  let ident = Some ident in
  Abs { span; param = { ident; type' }; body }

let app abs arg =
  App { span; abs; arg }

let univ name type' body =
  let ident = new_ident name in
  let body = body (var ident) in
  let ident = Some ident in
  Univ { span; param = { ident; type' }; body }

let rec' name body =
  let ident = new_ident name in
  let body = body (var ident) in
  Rec { span; ident; body }

let union left right =
  Union { span; left; right }

let inter left right =
  Inter { span; left; right }

let unit   = var_unit  span
let true'  = var_true  span
let false' = var_false span
let bool   = var_bool  span
let type'  = var_type  span

let ctx = {
  vals = [
    {ident = ident_bool; value = union true' false'};
    {ident = ident_type; value = range Bot Top};
  ];
  vars = [
    {ident = ident_unit;  type' = Top};
    {ident = ident_true;  type' = Top};
    {ident = ident_false; type' = Top};
  ];
  univs = [];
  exiss = [];
}
