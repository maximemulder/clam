open Ast
open Ctx
open Ident
open Prim

let span = Code.span_primitive

let singleton term =
  Interval { span; lower = term; upper = term }

let interval lower upper =
  Interval { span; lower; upper }

let type' =
  Interval { span; lower = Bot; upper = Bot }

let bot =
  Bot

let top =
  Top

let var ident =
  Var { span; ident }

let row tag type' =
  Row { span; tag; type' }

let record attrs =
  let attrs = List.map (fun (tag, value) -> { tag; value }) attrs in
  Record { span; attrs }

let group body =
  Group { span; body }

let if' cond then' else' =
  If { span; cond; then'; else' }

let ascr body type' =
  Ascr { span; body; type' }

let abs name type' body =
  let ident = new_ident name in
  let body = body (var ident) in
  let ident = Some ident in
  Abs { span; param = { span; ident; type' }; body }

let app abs arg =
  App { span; abs; arg }

let univ name type' body =
  let ident = new_ident name in
  let body = body (var ident) in
  let ident = Some ident in
  Univ { span; param = { span; ident; type' }; body }

let rec' name body =
  let ident = new_ident name in
  let body = body ident in
  Rec { span; ident; body }

let union left right =
  Union { span; left; right }

let inter left right =
  Inter { span; left; right }

let unit   = var_unit  span
let true'  = var_true  span
let false' = var_false span
let bool   = var_bool  span

let ctx = {
  vals = [
    {ident = ident_bool; value = (union (singleton true') (singleton false'))}
  ];
  vars = [
    {ident = ident_unit;  type' = Top};
    {ident = ident_true;  type' = Top};
    {ident = ident_false; type' = Top};
  ];
  univs = [];
  exiss = [];
}
