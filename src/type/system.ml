(*
  This file contains various algorithms used for type checking. These algorithms predate type
  constraining and do not implement some new rules such as higher-rank polymorphism subtyping.
  I should work on unifying them with constraining when I have the time.
*)

open Node

(* TYPE EQUIVALENCE *)

(*
  This function uses mutual subtyping to check for type equivalence so that it
  can handle cases such as the following:
  `A <: B |- A | B = A`
  This introduces a dependency of type equivalence to subtyping. It should however
  be noted that the above type should always appear simplified to `B`, therefore
  the handling of this case is not strictly necessary.
*)
let rec is ctx left right =
  isa ctx left right && isa ctx right left

and is_param ctx (left: Node.param) (right: Node.param) =
  is ctx left.lower right.lower
  && is ctx left.upper right.upper

(* SUBTYPING *)

and isa ctx (sub: Node.type') (sup: Node.type') =
  match sub, sup with
  | Dnf sub, Dnf sup ->
    isa_union_1 ctx sub sup
  | _, _ ->
    raise (invalid_arg "TODO")

and isa_union_1 ctx subs sups =
  List.for_all (Util.flip (isa_union_2 ctx) sups) subs

and isa_union_2 ctx subs sups =
  List.exists (isa_inter_1 ctx subs) sups

and isa_inter_1 ctx subs sups =
  List.for_all (isa_inter_2 ctx subs) sups

and isa_inter_2 ctx subs sups =
  List.exists (Util.flip (isa_base ctx) sups) subs

and isa_base ctx (sub: Node.base) (sup: Node.base) =
  match sub, sup with
  | _, Top ->
    isa_top ctx sub
  | Bot, _ ->
    isa_bot ctx sup
  | Unit, Unit | Bool, Bool | Int, Int | String, String ->
    true
  | Var sub_var, _ ->
    isa_var_sub ctx sub_var sup
  | _, Var sup_var ->
    isa_var_sup ctx sub sup_var
  | Tuple sub_tuple, Tuple sup_tuple ->
    isa_tuple ctx sub_tuple sup_tuple
  | Record sub_record, Record sup_record ->
    isa_record ctx sub_record sup_record
  | Lam sub_lam, Lam sup_lam ->
    isa_lam ctx sub_lam sup_lam
  | Univ sub_univ, Univ sup_univ ->
    isa_univ ctx sub_univ sup_univ
  | Abs sub_abs, Abs sup_abs ->
    isa_abs ctx sub_abs sup_abs
  | App sub_app, App sup_app ->
    isa_app ctx sub_app sup_app
  | App sub_app, _ ->
    let sub_abs = promote ctx sub_app.abs in
    let sub_type = compute ctx sub_abs sub_app.arg in
    isa ctx sub_type (Node.base sup)
  | _ ->
    false

and isa_top ctx sub =
  Kind.get_kind_base ctx sub = Type

and isa_bot ctx sup =
  Kind.get_kind_base ctx sup = Type

and isa_var_sub ctx sub_var sup =
  match sup with
  | Var sup_var when sub_var.bind == sup_var.bind ->
    true
  | _ ->
    let _, sub_upper = Context.get_bounds ctx sub_var.bind in
    isa ctx sub_upper (base sup)

and isa_var_sup ctx sub sup_var =
  match sub with
  | Var sub_var when sub_var.bind == sup_var.bind ->
    true
  | _ ->
    let sup_lower, _ = Context.get_bounds ctx sup_var.bind in
    isa ctx (base sub) sup_lower

and isa_tuple ctx sub_tuple sup_tuple =
  List.equal (isa ctx) sub_tuple.elems sup_tuple.elems

and isa_record ctx sub_record sup_record =
  Util.NameMap.for_all (fun _ sup_attr -> isa_record_attr ctx sub_record sup_attr) sup_record.attrs

and isa_record_attr ctx sub_record sup_attr =
  match Util.NameMap.find_opt sup_attr.label sub_record.attrs with
  | Some sub_attr ->
    isa ctx sub_attr.type' sup_attr.type'
  | None ->
    false

and isa_lam ctx sub_lam sup_lam =
  isa ctx sup_lam.param sub_lam.param &&
  isa ctx sub_lam.ret sup_lam.ret

and isa_univ ctx sub_univ sup_univ =
  is_param ctx sub_univ.param sup_univ.param &&
  let sup_ret = Rename.rename sup_univ.param.bind sub_univ.param.bind sup_univ.ret in
  let ctx = Context.add_param ctx sub_univ.param in
  isa ctx sub_univ.ret sup_ret

and isa_abs ctx sub_abs sup_abs =
  is_param ctx sub_abs.param sup_abs.param &&
  let sup_body = Rename.rename sup_abs.param.bind sub_abs.param.bind sup_abs.body in
  let ctx = Context.add_param ctx sub_abs.param in
  isa ctx sub_abs.body sup_body

and isa_app ctx sub_app sup_app =
  isa ctx sub_app.abs sup_app.abs &&
  is ctx sub_app.arg sup_app.arg

(* TYPE PROMOTION *)

and promote ctx type' =
  map_type ctx (promote_base ctx) type'

and promote_base ctx type' =
  match type' with
  | Var var ->
    let _, upper = Context.get_bounds ctx var.bind in
    promote ctx upper
  | _ ->
    Node.base type'

(* TYPE SUBSTITUTION *)

and substitute ctx bind other (type': Node.type') =
  map_type ctx (substitute_base ctx bind other) type'

and substitute_base ctx bind other (type': Node.base) =
  match type' with
  | Top | Bot | Unit | Bool | Int | String ->
    base type'
  | Var var ->
    substitute_var bind other var
  | Tuple tuple ->
    let elems = List.map (substitute ctx bind other) tuple.elems in
    Node.tuple elems
  | Record record ->
    let attrs = Util.NameMap.map (substitute_attr ctx bind other) record.attrs in
    Node.record attrs
  | Lam lam ->
    let param = substitute ctx bind other lam.param in
    let ret = substitute ctx bind other lam.ret in
    Node.lam param ret
  | Univ univ ->
    let param = substitute_param ctx bind other univ.param in
    let ctx = Context.add_param ctx param in
    let ret = substitute ctx bind other univ.ret in
    Node.univ param ret
  | Abs abs ->
    let param = substitute_param ctx bind other abs.param in
    let ctx = Context.add_param ctx param in
    let body = substitute ctx bind other abs.body in
    Node.abs param body
  | App app ->
    let abs = substitute ctx bind other app.abs in
    let arg = substitute ctx bind other app.arg in
    compute ctx abs arg

and substitute_var bind other var =
  if var.bind == bind then
    other
  else
    Node.var var.bind

and substitute_param ctx bind other param =
  let lower = substitute ctx bind other param.lower in
  let upper = substitute ctx bind other param.upper in
  { param with lower; upper }

and substitute_attr ctx bind other attr =
  let type' = substitute ctx bind other attr.type' in
  { attr with type' }

(* TYPE COMPUTATION *)

and compute ctx (type': Node.type') (arg: Node.type') =
  map_type ctx (Util.flip (compute_base ctx) arg) type'

and compute_base ctx (abs: Node.base) (arg: Node.type') =
  match abs with
  | Abs abs ->
    substitute ctx abs.param.bind arg abs.body
  | _ ->
    Node.app (Node.base abs) arg

(* TYPE MAP *)

and map_type ctx f type' =
  match type' with
  | Dnf dnf ->
    map_dnf_union ctx (map_dnf_inter ctx f) dnf
  | Cnf _ ->
    raise (invalid_arg "TODO")

and map_dnf_union ctx f types =
  let types = List.map f types in
  Util.list_reduce (join ctx) types

and map_dnf_inter ctx f types =
  let types = List.map f types in
  Util.list_reduce (meet ctx) types

(* TYPE JOIN *)

and join ctx left right =
  match left, right with
  | Dnf left, Dnf right ->
    Dnf (join_dnf_union ctx left right)
  | _, _ ->
    raise (invalid_arg "TODO")

and join_dnf_union ctx lefts rights =
  Util.list_collapse (join_dnf_inter ctx) (lefts @ rights)

and join_dnf_inter ctx left right =
  if isa_inter_1 ctx left right then
    Some right
  else
  if isa_inter_1 ctx right left then
    Some left
  else
    None

(* TYPE MEET *)

and meet ctx left right =
  match left, right with
  | Dnf left, Dnf right ->
    Dnf (meet_dnf_union ctx left right)
  | _, _ ->
    raise (invalid_arg "TODO")

and meet_dnf_union ctx lefts rights =
  let types = Util.list_product (meet_dnf_inter ctx) lefts rights in
  Util.list_collapse (join_dnf_inter ctx) types

and meet_dnf_inter ctx lefts rights =
  Util.list_collapse (meet_base ctx) (lefts @ rights)

and meet_base ctx left right =
  match left, right with
  | Top    , right  -> Some right
  | left   , Top    -> Some left
  | Bot    , _      -> Some Bot
  | _      , Bot    -> Some Bot
  | Unit   , Unit   -> Some Unit
  | Bool   , Bool   -> Some Bool
  | Int    , Int    -> Some Int
  | String , String -> Some String
  | Var left_var, _ when isa_var_sub ctx left_var right ->
    Some (Var left_var)
  | _, Var right_var  when isa_var_sub ctx right_var left ->
    Some (Var right_var)
  | Var _, _ -> None
  | _, Var _ -> None
  | Tuple left_tuple, Tuple right_tuple ->
    meet_tuple ctx left_tuple right_tuple
  | Record left_record, Record right_record ->
    meet_record ctx left_record right_record
  | Lam left_lam, Lam right_lam ->
    meet_lam ctx left_lam right_lam
  | Univ left_univ, Univ right_univ ->
    meet_univ ctx left_univ right_univ
  | Abs left_abs, Abs right_abs ->
    meet_abs ctx left_abs right_abs
  (* For the two next rules, find the maximum type of app and check if subtype of other *)
  | App _, _ ->
    None
  | _, App _ ->
    None
  | _, _ ->
    Some Bot

and meet_tuple ctx left right =
  if List.compare_lengths left.elems right.elems != 0 then
    Some Bot
  else
  let elems = List.map2 (meet ctx) left.elems right.elems in
  Some (Tuple { elems })

and meet_record ctx left right =
  let attrs = Util.NameMap.merge (meet_record_attr ctx) left.attrs right.attrs in
  Some (Record { attrs })

and meet_record_attr ctx label left right =
  match left, right with
  | Some left, Some right ->
    let type' = meet ctx left.type' right.type' in
    Some { label; type' }
  | Some left, None ->
    Some left
  | None, Some right ->
    Some right
  | None, None ->
    None

and meet_lam ctx left right =
  match is ctx left.param right.param, is ctx left.ret right.ret with
  | false, false ->
    None
  | _, _ ->
    let param = join ctx left.param right.param in
    let ret = meet ctx left.ret right.ret in
    Some (Lam { param; ret })

and meet_univ ctx left right =
  if not (is_param ctx left.param right.param) then
    Some Bot
  else
  let right_ret = Rename.rename right.param.bind left.param.bind right.ret in
  let ctx = Context.add_param ctx left.param in
  let ret = meet ctx left.ret right_ret in
  Some (Univ { param = left.param; ret })

and meet_abs ctx left right =
  if not (is_param ctx left.param right.param) then
    Some Bot
  else
  let right_body = Rename.rename right.param.bind left.param.bind right.body in
  let ctx = Context.add_param ctx left.param in
  let body = meet ctx left.body right_body in
  Some (Abs { param = left.param; body })

(* PUBLIC FUNCTIONS *)

let substitute ctx type' bind other =
  substitute ctx bind other type'

(* KIND EQUALITY *)

(**
  Determines if two given kinds are equal.
*)
let rec is_kind ctx left right =
  match left, right with
  | Kind.Type, Kind.Type ->
    true
  | Kind.Abs left_abs, Kind.Abs right_abs ->
    is ctx left_abs.lower right_abs.lower &&
    is ctx left_abs.upper right_abs.upper &&
    is_kind ctx left_abs.ret right_abs.ret
  | _, _ ->
    false
