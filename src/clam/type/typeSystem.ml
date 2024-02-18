(*
  This file contains various algorithms used for type checking. These algorithms predate type
  constraining and do not implement some new rules such as higher-rank polymorphism subtyping.
  I should work on unifying them with constraining when I have the time.
*)

(* BOTTOM TYPE EQUIVALENCE *)

(**
  These functions check if a type is equivalent to the bottom type. This requires
  to recursively check the bounds of type variables as variables whose bounds are
  equivalent to `Bot` are themselves equivalent to `Bot`.
*)

let rec type_is_bot ctx (type': Type.type') =
  union_is_bot ctx type'

and union_is_bot ctx (union: Type.union) =
  List.for_all (inter_is_bot ctx) union.union

and inter_is_bot ctx (inter: Type.inter) =
  List.exists (base_is_bot ctx) inter.inter

and base_is_bot ctx (type': Type.base) =
  match type' with
  | Bot     -> true
  | Var var -> var_is_bot ctx var
  | _       -> false

and var_is_bot ctx (var: Type.var) =
  let bound = TypeContext.get_bind_type ctx var.bind in
  type_is_bot ctx bound

(* TYPE EQUIVALENCE *)

(**
  This function uses mutual subtyping to check for type equivalence so that it
  can handle cases such as the following:
  `A <: B |- A | B = A`
  This introduces a dependency of type equivalence to subtyping. It should however
  be noted that the above type should always appear simplified to `B`, therefore
  the handling of this case is not strictly necessary.
*)
let rec is ctx left right =
  isa ctx left right && isa ctx right left

and is_param ctx (left: Type.param) (right: Type.param) =
  is ctx left.bound right.bound

(* SUBTYPING *)

and isa ctx (sub: Type.type') (sup: Type.type') =
  isa_union_1 ctx sub sup

and isa_union_1 ctx (sub: Type.union) (sup: Type.union) =
  List.for_all (Util.flip (isa_union_2 ctx) sup) sub.union

and isa_union_2 ctx (sub: Type.inter) (sup: Type.union) =
  List.exists (isa_inter_1 ctx sub) sup.union

and isa_inter_1 ctx (sub: Type.inter) (sup: Type.inter) =
  List.for_all (isa_inter_2 ctx sub) sup.inter

and isa_inter_2 ctx (sub: Type.inter) (sup: Type.base) =
  List.exists (Util.flip (isa_base ctx) sup) sub.inter

and isa_base ctx (sub: Type.base) (sup: Type.base) =
  match sub, sup with
  |        _, Top    ->
    isa_top ctx sub
  | Bot    ,       _ ->
    true (* TODO: kinding *)
  | Unit   , Unit    ->
    true
  | Bool   , Bool    ->
    true
  | Int    , Int     ->
    true
  | String , String  ->
    true
  | Var sub_var, _ ->
    isa_var ctx sub_var sup
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
    isa ctx sub_type (Type.base sup)
  | _ ->
    false

and isa_top ctx sub =
  match TypeKind.get_kind_base ctx sub with
  | TypeKind.Type ->
    true
  | TypeKind.Abs _ ->
    false

and isa_var ctx sub_var sup =
  var_is_bot ctx sub_var ||
  match sup with
  | Var sup_var when sub_var.bind == sup_var.bind ->
    true
  | _ ->
    let sub_bound = TypeContext.get_bind_type ctx sub_var.bind in
    isa ctx sub_bound (Type.base sup)

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
  let sup_ret = substitute_body ctx sub_univ.param sup_univ.param sup_univ.ret in
  let ctx = TypeContext.add_bind_type ctx sub_univ.param.bind sub_univ.param.bound in
  isa ctx sub_univ.ret sup_ret

and isa_abs ctx sub_abs sup_abs =
  is_param ctx sub_abs.param sup_abs.param &&
  let sup_body = substitute_body ctx sub_abs.param sup_abs.param sup_abs.body in
  let ctx = TypeContext.add_bind_type ctx sub_abs.param.bind sub_abs.param.bound in
  isa ctx sub_abs.body sup_body

and isa_app ctx sub_app sup_app =
  isa ctx sub_app.abs sup_app.abs &&
  is ctx sub_app.arg sup_app.arg

(* TYPE PROMOTION *)

and promote ctx type' =
  map_type ctx (promote_base ctx) type'

and promote_base ctx type' =
  match type' with
  | Type.Var var ->
    let type' = TypeContext.get_bind_type ctx var.bind in
    promote ctx type'
  | _ ->
    Type.base type'

(* TYPE SUBSTITUTION *)

and substitute_arg ctx bind arg type' =
  let entry = TypeContext.entry bind arg in
  substitute ctx entry type'

and substitute_body ctx param param_body body =
  let ctx = TypeContext.add_bind_type ctx param.bind param.bound in
  let ctx = TypeContext.add_bind_type ctx param_body.bind param_body.bound in
  let var = Type.var param.bind in
  let entry = TypeContext.entry param_body.bind var in
  substitute ctx entry body

and substitute ctx entry (type': Type.type') =
  map_type ctx (substitute_base ctx entry) type'

and substitute_base ctx entry (type': Type.base) =
  match type' with
  | Top    -> Type.base Top
  | Bot    -> Type.base Bot
  | Unit   -> Type.base Unit
  | Bool   -> Type.base Bool
  | Int    -> Type.base Int
  | String -> Type.base String
  | Var var ->
    substitute_var entry var
  | Tuple tuple ->
    let elems = List.map (substitute ctx entry) tuple.elems in
    Type.tuple elems
  | Record record ->
    let attrs = Util.NameMap.map (substitute_attr ctx entry) record.attrs in
    Type.record attrs
  | Lam lam ->
    let param = substitute ctx entry lam.param in
    let ret = substitute ctx entry lam.ret in
    Type.lam param ret
  | Univ univ ->
    let param = substitute_param ctx entry univ.param in
    let ctx = TypeContext.add_bind_type ctx param.Type.bind param.bound in
    let ret = substitute ctx entry univ.ret in
    Type.univ param ret
  | Abs abs ->
    let param = substitute_param ctx entry abs.param in
    let ctx = TypeContext.add_bind_type ctx param.bind param.bound in
    let body = substitute ctx entry abs.body in
    Type.abs param body
  | App app ->
    let abs = substitute ctx entry app.abs in
    let arg = substitute ctx entry app.arg in
    compute ctx abs arg

and substitute_var entry var =
  if var.bind == entry.bind then
    entry.bound
  else
    Type.var var.bind

and substitute_param ctx entry param =
  let bound = substitute ctx entry param.bound in
  { param with bound }

and substitute_attr ctx entry attr =
  let type' = substitute ctx entry attr.type' in
  { attr with type' }

(* TYPE COMPUTATION *)

and compute ctx (type': Type.type') (arg: Type.type') =
  map_type ctx (Util.flip (compute_base ctx) arg) type'

and compute_base ctx (abs: Type.base) (arg: Type.type') =
  match abs with
  | Abs abs ->
    substitute_arg ctx abs.param.bind arg abs.body
  | _ ->
    Type.app (Type.base abs) arg

(* TYPE MAP *)

and map_type ctx f type' =
  map_union ctx (map_inter ctx f) type'

and map_union ctx f union =
  let types = List.map f union.union in
  Util.list_reduce (join ctx) types

and map_inter ctx f inter =
  let types = List.map f inter.inter in
  Util.list_reduce (meet ctx) types

(* TYPE JOIN *)

and join ctx (left: Type.type') (right: Type.type') =
  let types = Util.list_collapse (join_inter ctx) (left.union @ right.union) in
  { Type.union = types }

and join_inter ctx left right =
  if isa_inter_1 ctx left right then
    Some right
  else
  if isa_inter_1 ctx right left then
    Some left
  else
    None

(* TYPE MEET *)

and meet ctx left right =
  let types = Util.list_product (meet_inter ctx) left.union right.union in
  let types = Util.list_collapse (join_inter ctx) types in
  { Type.union = types }

and meet_inter ctx (left: Type.inter) (right: Type.inter) =
  let types = Util.list_collapse (meet_base ctx) (left.inter @ right.inter) in
  { Type.inter = types }

and meet_base ctx (left: Type.base) (right: Type.base) =
  match left, right with
  | Top    , right  -> Some right
  | left   , Top    -> Some left
  | Bot    , _      -> Some Bot
  | _      , Bot    -> Some Bot
  | Unit   , Unit   -> Some Unit
  | Bool   , Bool   -> Some Bool
  | Int    , Int    -> Some Int
  | String , String -> Some String
  | Var left_var, _ when isa_var ctx left_var right ->
    Some (Var left_var)
  | _, Var right_var  when isa_var ctx right_var left ->
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
  let right_ret = substitute_body ctx left.param right.param right.ret in
  let ctx = TypeContext.add_bind_type ctx left.param.bind left.param.bound in
  let ret = meet ctx left.ret right_ret in
  Some (Univ { param = left.param; ret })

and meet_abs ctx left right =
  if not (is_param ctx left.param right.param) then
    Some Bot
  else
  let right_body = substitute_body ctx left.param right.param right.body in
  let ctx = TypeContext.add_bind_type ctx left.param.bind left.param.bound in
  let body = meet ctx left.body right_body in
  Some (Abs { param = left.param; body })
