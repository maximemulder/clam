open Utils
open Model
open TypingApp
open TypingContext
open TypingMerge
open TypingSub

module DefKey = struct
  type t = def_expr
  let compare x y = Stdlib.compare x.def_expr_id y.def_expr_id
end

module DefSet = Set.Make(DefKey)

module BindKey = struct
  type t = bind_expr

  let compare x y  = Stdlib.compare (bind_expr_id x) (bind_expr_id y)
end

module BindMap = Map.Make(BindKey)

type progress = {
  remains: DefSet.t;
  currents: DefSet.t;
  dones: type' BindMap.t;
}

type state = {
  progress: progress;
  context: context;
}

module State = struct
  type s = state
end

open Monad.Monad(Monad.StateMonad(State))

let (let+) (m: 'a t) (f: 'a -> 'b): 'b t =
fun s ->
  let (a, s1) = m s in
  (f a, s1)

let make_progress defs =
  let remains = List.fold_left (fun set def -> DefSet.add def set) DefSet.empty defs in
  { remains; currents = DefSet.empty; dones = BindMap.empty }

let start_progress progress def =
  let remains = DefSet.remove def progress.remains in
  let currents = DefSet.add def progress.currents in
  { progress with remains; currents }

let end_progress progress def type' =
  let currents = DefSet.remove def progress.currents in
  let dones = BindMap.add (BindExprDef def) type' progress.dones in
  { progress with currents; dones }

let add_bind bind type' state =
  let dones = BindMap.add bind type' state.progress.dones in
  { state with progress = { state.progress with dones } }

let make_state progress =
  { progress; context = empty_context }

let return_def def =
  fun type' state -> (type', { state with progress = end_progress state.progress def type' })

let make_type expr type_data =
  (fst expr, type_data)

let make_type_attr attr type' =
  make_attr_type (fst attr.attr_expr) attr.attr_expr_name type'

let preop_types =
  [
    ("+", (TypeInt, TypeInt));
    ("-", (TypeInt, TypeInt));
    ("!", (TypeBool, TypeBool));
  ]
  |> List.to_seq
  |> NameMap.of_seq

let binop_types =
  [
    ("+",  ((TypeInt, TypeInt), TypeInt));
    ("-",  ((TypeInt, TypeInt), TypeInt));
    ("*",  ((TypeInt, TypeInt), TypeInt));
    ("/",  ((TypeInt, TypeInt), TypeInt));
    ("%",  ((TypeInt, TypeInt), TypeInt));
    ("++", ((TypeString, TypeString), TypeString));
    ("==", ((TypeAny, TypeAny), TypeBool));
    ("!=", ((TypeAny, TypeAny), TypeBool));
    ("<",  ((TypeInt, TypeInt), TypeBool));
    (">",  ((TypeInt, TypeInt), TypeBool));
    ("<=", ((TypeInt, TypeInt), TypeBool));
    (">=", ((TypeInt, TypeInt), TypeBool));
    ("|",  ((TypeBool, TypeBool), TypeBool));
    ("&",  ((TypeBool, TypeBool), TypeBool));
  ]
  |> List.to_seq
  |> NameMap.of_seq

let check_type type' =
  TypingCheck.check type'

let check_type_proper type' state =
  TypingCheck.check type';
  let type' = apply type' state.context in
  match snd type' with
  | TypeAbs _ -> TypingErrors.raise_type_proper type'
  | _ -> (type', state)

let rec check_expr expr constraint' =
  let* type' = infer_none expr in
  if Bool.not (is_subtype type' constraint') then
    TypingErrors.raise_expr_constraint expr type' constraint'
   else
    return ()

and infer_expr expr returner =
  match snd expr with
  | ExprVoid ->
    returner (make_type expr TypeVoid)
  | ExprBool _ ->
    returner (make_type expr TypeBool)
  | ExprInt _ ->
    returner (make_type expr TypeInt)
  | ExprChar _ ->
    returner (make_type expr TypeChar)
  | ExprString _ ->
    returner (make_type expr TypeString)
  | ExprBind bind ->
    infer_bind expr (Option.get bind.bind_expr) returner
  | ExprTuple types ->
    let* types = infer_tuple_types types in
    returner (make_type expr (TypeTuple types))
  | ExprRecord attrs ->
    let* attrs = infer_record_attrs attrs in
    returner (make_type expr (TypeRecord attrs))
  | ExprVariant (expr, index) ->
    infer_elem expr index returner
  | ExprAttr (expr, attr) ->
    infer_attr expr attr returner
  | ExprPreop (op, operand) ->
    infer_preop expr op operand returner
  | ExprBinop (left, op, right) ->
    infer_binop expr left op right returner
  | ExprAscr (expr, type') ->
    infer_ascr expr type' returner
  | ExprIf (if', then', else') ->
    let* _ = check_expr if' (fst expr, TypeBool) in
    let* then' = infer_none then' in
    let* else' = infer_none else' in
    returner (make_type expr (merge_inter then' else'))
  | ExprBlock block ->
    infer_block expr block returner
  | ExprAbs (params, type', body) ->
    infer_abs expr params type' body returner
  | ExprApp (expr, args) ->
    infer_app expr args returner
  | ExprTypeAbs (params, expr) ->
    let _ = List.iter (fun param -> check_type param.param_type) params in
    let* type' = infer_none expr in
    returner (fst expr, TypeAbsExprType (params, type'))
  | ExprTypeApp (expr, args) ->
    infer_type_app expr args returner

and infer_none expr =
  infer_expr expr return

and infer_bind expr bind returner state =
  let pos = fst expr in
  match bind with
  | BindExprPrint ->
    returner (pos, TypeAbsExpr ([(pos, TypeAny)], (pos, TypeVoid))) state
  | BindExprDef def when DefSet.mem def state.progress.remains ->
    let (type', progress) = check_def def state.progress in
    (type', { state with progress })
  | BindExprDef def when DefSet.mem def state.progress.currents ->
    TypingErrors.raise_expr_recursive def
  | _ -> returner (BindMap.find bind state.progress.dones) state

and infer_tuple_types exprs =
  list_map infer_none exprs

and infer_record_attrs attrs =
  let mapper = (fun attr ->
    let* type' = infer_none attr.attr_expr in
    return (make_type_attr attr type')
  ) in
  let attrs = List.fold_left (fun map attr -> NameMap.add attr.attr_expr_name attr map) NameMap.empty attrs in
  map_map mapper attrs

and infer_elem expr index returner =
  let* type' = infer_none expr in
  match snd type' with
  | TypeTuple types ->
    infer_elem_tuple expr index type' types returner
  | _ ->
    TypingErrors.raise_expr_tuple_kind expr type'

and infer_elem_tuple expr index type' types returner =
  match List.nth_opt types index with
  | Some type' -> returner type'
  | None -> TypingErrors.raise_expr_tuple_index expr type' index

and infer_attr expr attr returner =
  let* type' = infer_none expr in
  match snd type' with
  | TypeRecord attrs ->
  infer_attr_record expr attr type' attrs returner
  | _ -> TypingErrors.raise_expr_record_kind expr type'

and infer_attr_record expr attr type' attrs returner =
  match NameMap.find_opt attr attrs with
  | Some attr -> returner attr.attr_type
  | None -> TypingErrors.raise_expr_record_attr expr type' attr

and infer_preop expr op operand returner =
  let entry = NameMap.find_opt op preop_types in
  match entry with
  | Some (type_operand, type_result) ->
    let operand_type = (fst operand, type_operand) in
    let result = (fst expr, type_result) in
    let* _ = returner result in
    let* _ = check_expr operand operand_type in
    return result
  | None ->
    TypingErrors.raise_unexpected

and infer_binop expr left op right returner =
  let entry = NameMap.find_opt op binop_types in
  match entry with
  | Some ((type_left, type_right), result) ->
    let left_type = (fst left, type_left) in
    let right_type = (fst right, type_right) in
    let result = (fst expr, result) in
    let* _ = returner result in
    let* _ = check_expr left left_type in
    let* _ = check_expr right right_type in
    return result
  | None ->
    TypingErrors.raise_unexpected

and infer_ascr expr type' returner state =
  let (type', state) = check_type_proper type' state in
  let (_, state) = returner type' state in
  let (_, state) = check_expr expr type' state in
  (type', state)

and infer_block expr block returner =
  let* _ = list_map infer_block_stmt block.block_stmts in
  match block.block_expr with
  | Some expr -> infer_expr expr returner
  | None -> returner (fst expr, TypeVoid)

and infer_block_stmt stmt state =
  match stmt with
  | StmtVar (var, type', expr) ->
    let (type', state) = match type' with
    | Some type' ->
      let (type', state) = check_type_proper type' state in
      let (_, state) = check_expr expr type' state in
      (type', state)
    | None ->
      infer_none expr state
    in
    let state = add_bind (BindExprVar var) type' state in
    ((), state)
  | StmtExpr expr ->
    let _ = infer_none expr state in
    ((), state)

and infer_abs expr params type' body returner =
  let* params = infer_abs_params params in
  match type' with
  | Some type' ->
    let* type' = check_type_proper type' in
    let signature = (fst expr, TypeAbsExpr (params, type')) in
    let* _ = returner signature in
    let* _ = check_expr body type' in
    return signature
  | None ->
    let* type' = infer_none body in
    returner (fst expr, TypeAbsExpr (params, type'))

and infer_abs_params params state =
  let types = List.map (fun param -> match param.param_expr_type with
  | Some type' -> type'
  | None -> TypingErrors.raise_param param
  ) params in
  let (types, state) = list_map check_type_proper types state in
  let params = List.map (fun param -> BindExprParam param) params in
  let pairs = List.combine params types in
  let state = List.fold_left (fun state pair -> add_bind (fst pair) (snd pair) state) state pairs in
  (types, state)

and infer_app expr args returner =
  let* type' = infer_none expr in
  match snd type' with
  | TypeAbsExpr (params, type') ->
    infer_app_abs expr type' params args returner
  | _ -> TypingErrors.raise_expr_app_kind expr type'

and infer_app_abs expr type' params args returner =
  if List.compare_lengths params args != 0 then
    TypingErrors.raise_expr_app_arity expr params args
  else
  let pairs = List.combine params args in
  let* _ = list_map (fun (param, arg) -> check_expr arg param) pairs in
  returner type'

and infer_type_app expr args returner =
  let* type' = infer_none expr in
  match snd type' with
  | TypeAbsExprType (params, type') ->
    infer_type_app_abs expr params args type' returner
  | _ -> TypingErrors.raise_expr_type_app_kind expr type'

and infer_type_app_abs expr params args type' returner state =
  if List.compare_lengths params args != 0 then
    TypingErrors.raise_expr_type_app_arity expr params args
  else
  let pairs = List.combine params args in
  List.iter (fun (param, arg) -> check_type arg; TypingCheck.check_subtype arg param.param_type; ) pairs;
  let context = { parent = Some state.context; params = pairs } in
  let body = apply type' context in
  returner body state

and check_def def progress =
  let progress = start_progress progress def in
  match def.def_expr_type with
  | Some type' ->
    let (type', _) = check_type_proper type' (make_state progress) in
    let progress = end_progress progress def type' in
    let state = make_state progress in
    let (_, state) = check_expr def.def_expr type' state in
    (type', state.progress)
  | None ->
    let state = make_state progress in
    let (type', state) = infer_expr def.def_expr (return_def def) state in
    (type', state.progress)

let rec progress_defs progress =
  match DefSet.choose_opt progress.remains with
  | None -> progress
  | Some def ->
    let (_, progress) = check_def def progress in
    progress_defs progress

let check_exprs defs =
  progress_defs (make_progress defs)

let check_types types =
  List.iter (fun type' -> TypingCheck.check type') types
