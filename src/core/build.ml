open Ast

let span = Code.span_primitive

let type' =
  Type

let bot =
  Bot

let top =
  Top

let var name =
  Var { span; name }

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
  let body = body (var name) in
  let name = Some name in
  Abs { span; param = { span; name; type' }; body }

let app abs arg =
  App { span; abs; arg }

let univ name type' body =
  let body = body (var name) in
  let name = Some name in
  Univ { span; param = { span; name; type' }; body }

let rec' name body =
  Rec { span; name; body }

let union left right =
  Union { span; left; right }

let inter left right =
  Inter { span; left; right }
