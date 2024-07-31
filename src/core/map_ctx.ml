open Ast
open Ctx
open Ctx.ResultState

let rec map_term f term =
  match term with
  | Bot | Top | Var _ ->
    return term
  | Record record ->
    let* attrs = MUtil.list_map (map_record_attr f) record.attrs in
    return (Record { record with attrs })
  | Attr attr ->
    let* record = f attr.record in
    return (Attr { attr with record })
  | Group group ->
    let* term = f group.term in
    return (Group { group with term })
  | If if' ->
    let* cond =  f if'.cond  in
    let* then' = f if'.then' in
    let* else' = f if'.else' in
      return (If { if' with cond; then'; else' })
  | Ascr ascr ->
    let* term  = f ascr.term  in
    let* type' = f ascr.type' in
    return (Ascr { ascr with term; type' })
  | Abs abs ->
    let* type' = MUtil.option_map f abs.param.type' in
    let* body = f abs.body in
    return (Abs { abs with param = { abs.param with type' }; body })
  | App app ->
    let* abs = f app.abs in
    let* arg = f app.arg in
    return (App { app with abs; arg })
  | Univ univ ->
    let* type' = MUtil.option_map f univ.param.type' in
    let* body = f univ.body in
    return (Univ { univ with param = { univ.param with type' }; body })
  | Rec rec' ->
    let* body = f rec'.body in
    return (Rec { rec' with body })
  | Union union ->
    let* left  = f union.left  in
    let* right = f union.right in
    return (Union { union with left; right })
  | Inter inter ->
    let* left  = f inter.left  in
    let* right = f inter.right in
    return (Inter { inter with left; right })
  | Range range ->
    let* lower = f range.lower in
    let* upper = f range.upper in
    return (Range { range with lower; upper })

and map_record_attr f attr =
  let* term = f attr.term in
  return { attr with term }

let map_term_ctx = map_term
