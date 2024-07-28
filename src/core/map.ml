open Ast

let rec map f term =
  match term with
  | Bot | Top | Var _ ->
    term
  | Row row ->
    let type' = f row.type' in
    Row { row with type' }
  | Record record ->
    let attrs = List.map (map_attr f) record.attrs in
    Record { record with attrs }
  | Group group ->
    let body = f group.body in
    Group { group with body }
  | If if' ->
    let cond = f if'.cond in
    let then' = f if'.then' in
    let else' = f if'.else' in
    If { if' with cond; then'; else' }
  | Ascr ascr ->
    let body = f ascr.body in
    let type' = f ascr.type' in
    Ascr { ascr with body; type' }
  | Abs abs ->
    let type' = Option.map f abs.param.type' in
    let body = f abs.body in
    Abs { abs with param = { abs.param with type' }; body }
  | App app ->
    let abs = f app.abs in
    let arg = f app.arg in
    App { app with abs; arg }
  | Univ univ ->
    let type' = Option.map f univ.param.type' in
    let body = f univ.body in
    Univ { univ with param = { univ.param with type' }; body }
  | Rec rec' ->
    let body = f rec'.body in
    Rec { rec' with body }
  | Union union ->
    let left = f union.left in
    let right = f union.right in
    Union { union with left; right }
  | Inter inter ->
    let left = f inter.left in
    let right = f inter.right in
    Inter { inter with left; right }
  | Interval interval ->
    let lower = f interval.lower in
    let upper = f interval.upper in
    Interval { interval with lower; upper }

and map_attr f attr =
  { attr with term = f attr.term }
