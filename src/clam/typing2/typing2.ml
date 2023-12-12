exception Todo of string

let todo = raise (Todo "TODO")

let simplify_app =
  todo

(* BOTTOM TYPE EQUIVALENCE *)

(**
  These functions check if a type is equivalent to the bottom type. This requires
  to recursively check the bounds of type variables as variables whose bounds are
  equivalent to `Bot` are themselves equivalent to `Bot`.
*)

let rec type_is_bot (type': Type.type') =
  union_is_bot type'

and union_is_bot (type': Type.union) =
  List.for_all inter_is_bot type'.union

and inter_is_bot (type': Type.inter) =
  List.exists base_is_bot type'.inter

and base_is_bot (type': Type.base) =
  match type' with
  | Bot _ ->
    true
  | Var var ->
    var_is_bot var
  | _ ->
    false

and var_is_bot (var: Type.var) =
  type_is_bot var.param.bound

(* TYPE EQUIVALENCE *)

(**
  This function uses mutual subtyping to check for type equivalence so that it
  can handle cases such as the following:
  `A <: B |- A | B = A`
  This introduces a dependency of type equivalence to subtyping. It should however
  be noted that the above type should always appear simplified to `B`, therefore
  the handling of this case is not strictly necessary.
*)
let rec is (left: Type.type') (right: Type.type') =
  isa left right && isa right left

and is_param (left: Type.param) (right: Type.param) =
  is left.bound right.bound

(* SUBTYPING *)

and isa (sub: Type.type') (sup: Type.type') =
  isa_union_1 sub sup

and isa_union_1 (sub: Type.union) (sup: Type.union) =
  List.for_all (Utils.flip isa_union_2 sup) sub.union

and isa_union_2 (sub: Type.inter) (sup: Type.union) =
  List.exists (isa_inter_1 sub) sup.union

and isa_inter_1 (sub: Type.inter) (sup: Type.inter) =
  List.for_all (isa_inter_2 sub) sup.inter

and isa_inter_2 (sub: Type.inter) (sup: Type.base) =
  List.exists (Utils.flip isa_base sup) sub.inter

and isa_base (sub: Type.base) (sup: Type.base) =
  match sub, sup with
  |        _, Top    _ -> isa_top sub
  | Bot    _,        _ -> true
  | Unit   _, Unit   _ -> true
  | Bool   _, Bool   _ -> true
  | Int    _, Int    _ -> true
  | Char   _, Char   _ -> true
  | String _, String _ -> true
  | Var sub_var, _ ->
    isa_var sub_var sup
  | Tuple sub_tuple, Tuple sup_tuple ->
    isa_tuple sub_tuple sup_tuple
  | Record sub_record, Record sup_record ->
    isa_record sub_record sup_record
  | AbsExpr sub_abs, AbsExpr sup_abs ->
    isa_abs_expr sub_abs sup_abs
  | AbsTypeExpr sub_abs, AbsTypeExpr sup_abs ->
    isa_abs_type_expr sub_abs sup_abs
  | Abs sub_abs, Abs sup_abs ->
    isa_abs sub_abs sup_abs
  | App left_app, App right_app ->
    isa_app left_app right_app
  | _ ->
    false

and isa_top sub =
  (* match TypingKind.get_kind sub with
  | TypingKind.Type -> true
  | TypingKind.Abs _ -> false *)
  todo

and isa_var sub_var sup =
  var_is_bot sub_var ||
  match sup with
  | Var sup_var when sub_var.param == sup_var.param ->
    true
  | _ ->
    isa sub_var.param.bound (Type.base sup)

and isa_tuple sub_tuple sup_tuple =
  List.equal isa sub_tuple.elems sup_tuple.elems

and isa_record sub_record sup_record =
  Utils.NameMap.for_all (fun _ sup_attr -> isa_record_attr sub_record sup_attr) sup_record.attrs

and isa_record_attr sub_record sup_attr =
  match Utils.NameMap.find_opt sup_attr.name sub_record.attrs with
  | Some sub_attr ->
    isa sub_attr.type' sup_attr.type'
  | None ->
    false

and isa_abs_expr sub_abs sup_abs =
  isa sup_abs.param sub_abs.param &&
  isa sub_abs.ret sup_abs.ret

and isa_abs_type_expr sub_abs sup_abs =
  is_param sub_abs.param sup_abs.param
  (* && let sup_body = TypingApp.apply_abs_expr_param sup_abs sub_abs.param in
  isa sub_abs.body sup_body *)

and isa_abs sub_abs sup_abs =
  is_param sub_abs.param sup_abs.param
  (* && let sup_body = TypingApp.apply_abs_param sup_abs sub_abs.param in
  isa sub_abs.body sup_body *)

and isa_app sub_app sup_app =
  isa sub_app.abs sup_app.abs &&
  is sub_app.arg sup_app.arg

(* TYPE JOIN *)

let rec join (left: Type.type') (right: Type.type') =
    let types = Utils.try_reduce_rec (left.union @ right.union) join_inter in
    { Type.union = types }

and join_inter left right =
  if isa_inter_1 left right then
    Some right
  else
  if isa_inter_1 right left then
    Some left
  else
    None

(* TYPE MEET *)

let rec meet (left: Type.type') (right: Type.type') =
  let types = Utils.product_lists meet_inter left.union right.union in
  let types = Utils.try_reduce_rec types join_inter in
  { Type.union = types }

and meet_inter (left: Type.inter) (right: Type.inter) =
  let types = Utils.try_reduce_rec (left.inter @ right.inter) meet_base in
  { Type.inter = types }

and meet_base (left: Type.base) (right: Type.base) =
  let pos = Type.pos left in
  match left, right with
  | Top    _, right    -> Some right
  | left    , Top    _ -> Some left
  | Bot    _,        _ -> Some (Bot    { pos })
  |        _, Bot    _ -> Some (Bot    { pos })
  | Unit   _, Unit   _ -> Some (Unit   { pos })
  | Bool   _, Bool   _ -> Some (Bool   { pos })
  | Int    _, Int    _ -> Some (Int    { pos })
  | Char   _, Char   _ -> Some (Char   { pos })
  | String _, String _ -> Some (String { pos })
  | (Var left_var, _) when isa_var left_var right ->
    Some (Var left_var)
  | _, Var right_var when isa_var right_var left ->
    Some (Var right_var)
  | Var _, _ ->
    None
  | _, Var _ ->
    None
  | Tuple left_tuple, Tuple right_tuple ->
    meet_tuple left_tuple right_tuple
  | Record left_record, Record right_record ->
    meet_record left_record right_record
  | AbsExpr left_abs, AbsExpr right_abs ->
    meet_abs_expr left_abs right_abs
  | AbsTypeExpr left_abs, AbsTypeExpr right_abs ->
    meet_abs_type_expr left_abs right_abs
  | Abs left_abs, Abs right_abs ->
    meet_abs left_abs right_abs
  (* TODO: The two next cases are probably wrong but I am too done to fix it now *)
  | App left_app, _ ->
    let left = simplify_app left_app in
    meet_base left right
  | _, App right_app ->
    let right = simplify_app right_app in
    meet_base left right
  | _, _ ->
    Some (Bot { pos })

and meet_tuple left right =
  if List.compare_lengths left.elems right.elems != 0 then
    Some (Bot { pos = left.pos })
  else
  let elems = List.map2 meet left.elems right.elems in
  Some (Tuple { pos = left.pos; elems })

and meet_record left right =
  let attrs = Utils.NameMap.merge meet_record_attr left.attrs right.attrs in
  Some (Record { pos = left.pos; attrs })

and meet_record_attr name left right =
  match left, right with
  | Some left, Some right ->
    let type' = meet left.type' right.type' in
    Some { pos = left.pos; name; type' }
  | Some left, None ->
    Some left
  | None, Some right ->
    Some right
  | None, None ->
    None

and meet_abs_expr left right =
  match is left.param right.param, is left.ret right.ret with
  | true, true ->
    None
  | _, _ ->
    let param = join left.param right.param in
    let ret = meet left.ret right.ret in
    Some (AbsExpr { pos = left.pos; param; ret })

and meet_abs_type_expr left right =
  if not (is_param left.param right.param) then
    Some (Bot { pos = left.pos })
  else
  (*let right_ret = TypingApp.apply_abs_expr_param right left.param in
  let ret = meet left.ret right_ret in
  Some (AbsTypeExpr { pos = left.pos; param = left_abs.param; ret })*)
  todo

and meet_abs left right =
  if not (is_param left.param right.param) then
    Some (Bot { pos = left.pos })
  else
  (* let right_ret = TypingApp.apply_abs_param right left.param in
  let ret = meet left.ret right_ret in
  Some (Abs { pos = left_abs.pos; param = left_abs.param; ret }) *)
  todo
