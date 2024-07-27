open Ast
open Ident

let span = Code.span_primitive

let unit =
  Var { span; ident = { name = "Unit"; index = 0 } }

let true' =
  Var { span; ident = { name = "True"; index = 0 } }

let false' =
  Var { span; ident = { name = "False"; index = 0 } }

let type' =
  Type

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
