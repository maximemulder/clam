open Utils
open Model

module DefKey = struct
  type t = def_expr
  let compare x y = Stdlib.compare x.id y.id
end

module DefSet = Set.Make(DefKey)

module BindKey = struct
  type t = bind_expr

  let compare x y  = Stdlib.compare (bind_expr_id x) (bind_expr_id y)
end

module BindMap = Map.Make(BindKey)

type state = {
  remains: DefSet.t;
  currents: DefSet.t;
  dones: type' BindMap.t;
}

module State = struct
  type s = state
end

open Monad.Monad(Monad.StateMonad(State))

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
  let dones = BindMap.add bind type' state.dones in
  ((), { state with dones })

let return_def (def: def_expr) =
  fun type' state -> (type', end_progress state def type')

let return_abs (abs: expr_abs) param returner =
  fun body -> returner (TypeAbsExpr { pos = abs.pos; param; body })

let preop_types =
  [
    ("+", (prim_int, prim_int));
    ("-", (prim_int, prim_int));
    ("!", (prim_bool, prim_bool));
  ]
  |> List.to_seq
  |> NameMap.of_seq

let binop_types =
  [
    ("+",  ((prim_int, prim_int), prim_int));
    ("-",  ((prim_int, prim_int), prim_int));
    ("*",  ((prim_int, prim_int), prim_int));
    ("/",  ((prim_int, prim_int), prim_int));
    ("%",  ((prim_int, prim_int), prim_int));
    ("++", ((prim_string, prim_string), prim_string));
    ("==", ((prim_top, prim_top), prim_bool));
    ("!=", ((prim_top, prim_top), prim_bool));
    ("<",  ((prim_int, prim_int), prim_bool));
    (">",  ((prim_int, prim_int), prim_bool));
    ("<=", ((prim_int, prim_int), prim_bool));
    (">=", ((prim_int, prim_int), prim_bool));
    ("|",  ((prim_bool, prim_bool), prim_bool));
    ("&",  ((prim_bool, prim_bool), prim_bool));
  ]
  |> List.to_seq
  |> NameMap.of_seq

let print_type =
  TypeAbsExpr { pos = prim_pos; param = prim_top; body = prim_unit }

let validate type' =
  let () = TypingValidate.validate type' in
  return ()

let validate_proper type' =
  let () = TypingValidate.validate_proper type' in
  return ()

let validate_subtype type' constr =
  let () = TypingValidate.validate_subtype type' constr in
  return ()

let validate_suptype type' constr =
  let () = TypingValidate.validate_suptype type' constr in
  return ()

module type INFERER = sig
  type t
  val meet: t -> t -> t
  val join: t -> t -> t
  val bot: t
end

let rec infer_type f type' =
  let type' = Typing.normalize type' in
  match type' with
  | TypeBot _ ->
    return (Some Model.prim_bot)
  | TypeVar var ->
    infer_type f var.param.type'
  | TypeInter inter ->
    let* left = infer_type f inter.left in
    let* right = infer_type f inter.right in
    return (Utils.join_option2 left right Typing.meet)
  | TypeUnion union ->
    let* left = infer_type f union.left in
    let* right = infer_type f union.right in
    return (Utils.map_option2 left right Typing.join)
  | TypeApp app ->
    let type' = TypingApp.apply_app app in
    infer_type f type'
  | _ ->
    f type'

module Inferer(I: INFERER) = struct
  let rec infer f type' =
    let type' = Typing.normalize type' in
    match type' with
    | TypeBot _ ->
      return (Some I.bot)
    | TypeVar var ->
      infer f var.param.type'
    | TypeInter inter ->
      let* left = infer f inter.left in
      let* right = infer f inter.right in
      return (Utils.join_option2 left right I.meet)
    | TypeUnion union ->
      let* left = infer f union.left in
      let* right = infer f union.right in
      return (Utils.map_option2 left right I.join)
    | TypeApp app ->
      let type' = TypingApp.apply_app app in
      infer f type'
    | _ ->
      f type'
end

module InfererProj = Inferer(
  struct
    type t = type'
    let join = Typing.join
    let meet = Typing.meet
    let bot = Model.prim_bot
  end
)

module InfererApp = Inferer(
  struct
    type t = type' * type'
    let join l r = Typing.meet (fst l) (fst r), Typing.join (snd l) (snd r)
    let meet l r = Typing.join (fst l) (fst r), Typing.meet (snd l) (snd r)
    let bot = prim_top, prim_bot
  end
)

let rec check expr constr =
  let constr = Typing.normalize constr in
  match constr with
  | TypeUnion _ | TypeInter _ ->
    check_infer expr constr
  | TypeApp app ->
    let constr = TypingApp.apply_app app in
    check expr constr
  | _ ->
  match expr with
  | ExprTuple tuple ->
    check_tuple tuple constr
  | ExprRecord record ->
    check_record record constr
  | ExprIf if' ->
    check_if if' constr
  | ExprAbs abs ->
    check_abs abs constr
  | ExprTypeAbs abs ->
    check_type_abs abs constr
  | _ ->
    check_infer expr constr

and check_infer expr constr =
  let* type' = infer_none expr in
  if Bool.not (Typing.isa type' constr) then
    TypingErrors.raise_expr_constraint expr type' constr
  else
    return ()

and check_tuple tuple constr =
  match constr with
  | TypeTuple constr_tuple ->
    if List.compare_lengths tuple.elems constr_tuple.elems != 0 then
      TypingErrors.raise_check_tuple_arity tuple constr_tuple
    else
    iter_list2 check tuple.elems constr_tuple.elems
  | _ ->
    TypingErrors.raise_check_tuple tuple constr

and check_record record constr =
  match constr with
  | TypeRecord constr_record ->
    iter_map (check_record_attr record) constr_record.attrs
  | _ ->
    TypingErrors.raise_check_record record constr

and check_record_attr record constr_attr =
  match List.find_opt (fun (attr: attr_expr) -> attr.name = constr_attr.name) record.attrs with
  | Some attr -> check attr.expr constr_attr.type'
  | None -> TypingErrors.raise_check_record_attr record constr_attr

and check_if if' constr =
  let* _ = check if'.cond prim_bool in
  let* _ = check if'.then' constr in
  let* _ = check if'.else' constr in
  return ()

and check_abs abs constr =
  match constr with
  | TypeAbsExpr constr_abs ->
    let* _ = check_abs_param abs.param constr_abs.param in
    let* _ = check abs.body constr_abs.body in
    return ()
  | _ ->
    TypingErrors.raise_check_abs abs constr

and check_abs_param param constr =
  let* type' = match param.type' with
  | Some type' ->
    let* () = validate_proper type' in
    let* () = validate_suptype type' constr in
    return type'
  | None ->
    return constr
  in
  add_bind (BindExprParam param) type'

and check_type_abs abs constr =
  match constr with
  | TypeAbsExprType constr_abs ->
    let* _ = check_type_abs_param abs constr_abs.param in
    let constr_body = TypingApp.apply_abs_expr_param constr_abs abs.param in
    let* _ = check abs.body constr_body in
    return ()
  | _ ->
    TypingErrors.raise_check_type_abs abs constr

and check_type_abs_param abs constr_param =
  let* () = validate abs.param.type' in
  if Typing.is abs.param.type' constr_param.type' then
    return ()
  else
    TypingErrors.raise_check_type_abs_param abs constr_param

and infer expr returner =
  match expr with
  | ExprUnit unit ->
    infer_unit unit returner
  | ExprBool bool ->
    infer_bool bool returner
  | ExprInt int ->
    infer_int int returner
  | ExprChar char ->
    infer_char char returner
  | ExprString string ->
    infer_string string returner
  | ExprBind bind ->
    infer_bind bind returner
  | ExprTuple tuple ->
    infer_tuple tuple returner
  | ExprRecord record ->
    infer_record record returner
  | ExprElem elem ->
    infer_elem elem returner
  | ExprAttr attr ->
    infer_attr attr returner
  | ExprPreop preop ->
    infer_preop preop returner
  | ExprBinop binop ->
    infer_binop binop returner
  | ExprAscr ascr ->
    infer_ascr ascr returner
  | ExprIf if' ->
    infer_if if' returner
  | ExprAbs abs ->
    infer_abs abs returner
  | ExprApp app ->
    infer_app app returner
  | ExprTypeAbs abs ->
    infer_type_abs abs returner
  | ExprTypeApp app ->
    infer_type_app app returner
  | ExprStmt stmt ->
    infer_stmt stmt returner

and infer_none expr =
  infer expr return

and infer_unit unit returner =
  returner (TypeUnit { pos = unit.pos })

and infer_bool bool returner =
  returner (TypeBool { pos = bool.pos })

and infer_int int returner =
  returner (TypeInt { pos = int.pos })

and infer_char char returner =
  returner (TypeChar { pos = char.pos })

and infer_string string returner =
  returner (TypeString { pos = string.pos })

and infer_bind bind returner state =
  let bind = Option.get !(bind.bind) in
  match bind with
  | BindExprPrint ->
    returner print_type state
  | BindExprDef def when DefSet.mem def state.remains ->
    let (type', state) = check_def def state in
    returner type' state
  | BindExprDef def when DefSet.mem def state.currents ->
    TypingErrors.raise_expr_recursive def
  | _ ->
    returner (BindMap.find bind state.dones) state

and infer_tuple tuple returner =
  let* elems = map_list infer_none tuple.elems in
  returner (TypeTuple { pos = tuple.pos; elems })

and infer_record record returner =
  let attrs = record.attrs in
  let attrs = List.fold_left (fun map (attr: attr_expr) -> NameMap.add attr.name attr map) NameMap.empty attrs in
  let* attrs = map_map infer_record_attr attrs in
  returner (TypeRecord { pos = record.pos; attrs })

and infer_record_attr attr =
  let* type' = infer_none attr.expr in
  return ({ pos = attr.pos; name = attr.name; type' })

and infer_elem elem returner =
  let* tuple = infer_none elem.expr in
  let* type' = InfererProj.infer (infer_elem_final elem.index) tuple in
  match type' with
  | Some type' -> returner type'
  | None -> TypingErrors.raise_expr_elem elem tuple

and infer_elem_final index type' =
  match type' with
  | TypeTuple tuple ->
    return (List.nth_opt tuple.elems index)
  | _ ->
    return None

and infer_attr attr returner =
  let* record = infer_none attr.expr in
  let* type' = infer_type (infer_attr_final attr.name) record in
  match type' with
  | Some type' -> returner type'
  | None -> TypingErrors.raise_expr_attr attr record

and infer_attr_final name type' =
  match type' with
  | TypeRecord record ->
    return (Option.map (fun (attr: attr_type) -> attr.type') (NameMap.find_opt name record.attrs))
  | _ ->
    return None

and infer_app app returner =
  let* abs = infer_none app.expr in
  let* type' = InfererApp.infer infer_app_final abs in
  match type' with
  | Some (param, body) ->
    let* () = check app.arg param in
    returner body
  | _ ->
    TypingErrors.raise_expr_app_kind app abs

and infer_app_final type' =
  match type' with
  | TypeAbsExpr abs ->
    return (Some (abs.param, abs.body))
  | _ ->
    return None

(* TODO: Check (and very probably fix) this function (and remove infer_type) *)
and infer_type_app app returner =
  let* abs = infer_none app.expr in
  let* type' = infer_type (infer_type_app_final app.arg) abs in
  match type' with
  | Some type' ->
    returner type'
  | None ->
    TypingErrors.raise_expr_type_app_kind app abs

and infer_type_app_final arg type' =
  match type' with
  | TypeAbsExprType abs ->
    let* _ = validate_subtype arg abs.param.type' in
    let entry = TypingApp.entry abs.param arg in
    let body = TypingApp.apply abs.body entry in
    return (Some body)
  | _ ->
    return None

and infer_preop preop returner =
  let entry = NameMap.find_opt preop.op preop_types in
  match entry with
  | Some (type_operand, type_result) ->
    let operand = preop.expr in
    let* returned = returner type_result in
    let* _ = check operand type_operand in
    return returned
  | None ->
    TypingErrors.raise_unexpected

and infer_binop binop returner =
  let entry = NameMap.find_opt binop.op binop_types in
  match entry with
  | Some ((type_left, type_right), type_result) ->
    let left = binop.left in
    let right = binop.right in
    let* returned = returner type_result in
    let* _ = check left type_left in
    let* _ = check right type_right in
    return returned
  | None ->
    TypingErrors.raise_unexpected

and infer_ascr ascr returner =
  let type' = ascr.type' in
  let* () = validate type' in
  let* returned = returner type' in
  let* _ = check ascr.expr type' in
  return returned

and infer_if if' returner =
  let* _ = check if'.cond prim_bool in
  let* then' = infer_none if'.then' in
  let* else' = infer_none if'.else' in
  returner (Typing.join then' else')

and infer_abs abs returner =
  let* param = infer_abs_param abs.param in
  let returner = return_abs abs param returner in
  infer abs.body returner

and infer_abs_param param =
  let type' = match param.type' with
  | Some type' -> type'
  | None -> TypingErrors.raise_param param in
  let* () = validate_proper type' in
  let bind = BindExprParam param in
  let* _ = add_bind bind type' in
  return type'

and infer_type_abs abs returner =
  TypingValidate.validate abs.param.type';
  let* body = infer_none abs.body in
  returner (TypeAbsExprType { pos = abs.pos; param = abs.param; body })

and infer_stmt stmt returner =
  let* _ = infer_stmt_body stmt.stmt in
  infer stmt.expr returner

and infer_stmt_body body =
  match body with
  | StmtVar (var, type', expr) ->
    let* type' = match type' with
    | Some type' ->
      let* () = validate type' in
      let* _ = check expr type' in
      return type'
    | None ->
      infer_none expr
    in
    let* _ = add_bind (BindExprVar var) type' in
    return ()
  | StmtExpr expr ->
    let* _ = infer_none expr in
    return ()

and check_def def state =
  let state = start_progress state def in
  match def.type' with
  | Some type' ->
    TypingValidate.validate_proper type';
    let state = end_progress state def type' in
    let (_, state) = check def.expr type' state in
    (type', state)
  | None ->
    let (type', state) = infer def.expr (return_def def) state in
    (type', state)

let rec progress_defs progress =
  match DefSet.choose_opt progress.remains with
  | None -> progress
  | Some def ->
    let (_, progress) = check_def def progress in
    progress_defs progress

let check_exprs defs =
  progress_defs (make_progress defs)

let check_types types =
  List.iter TypingValidate.validate types
