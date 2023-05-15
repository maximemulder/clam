open Collection
open Display_type
open Typing_check

open Monad.Monad(Monad.ReaderMonad(Typing_check.Reader))

let require_constraint constraint' type' =
  match constraint' with
  | Some constraint' ->
    let* constraint' = check constraint' in
    let* type' = check type' in
    return (check_subtype type' constraint')
  | None -> return ()

let rec get_expr_type expr =
  match expr with
  | Model.ExprVoid         -> Model.TypeVoid
  | Model.ExprBool   _     -> Model.TypeBool
  | Model.ExprInt    _     -> Model.TypeInt
  | Model.ExprChar   _     -> Model.TypeChar
  | Model.ExprString _     -> Model.TypeString
  | Model.ExprTuple exprs  -> Model.TypeTuple (List.map get_expr_type exprs)
  | Model.ExprRecord attrs -> Model.TypeRecord (get_attrs_types attrs)
  | _ -> Model.TypeAny

and get_attrs_types attrs =
  List.fold_left (fun map attr ->
    NameMap.add attr.Model.attr_expr_name (Model.make_attr_type attr.Model.attr_expr_name (get_expr_type attr.Model.attr_expr)) map
  ) NameMap.empty attrs

let check_expr expr constraint' =
  require_constraint constraint' (get_expr_type expr)

let check_exprs exprs =
  List.iter (fun done' -> check_expr done'.Modelize_exprs.done_expr done'.Modelize_exprs.done_type { parent = None; binds = [] }) exprs

let check_type type' =
  let context =  { parent = None; binds = [] } in
  let _ = check type' context in
  ()

let check_types types =
  List.iter (fun type' -> check_type type') types
