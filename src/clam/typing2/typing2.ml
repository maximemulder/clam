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
  List.for_all (Utils.flip (isa_union_2 ctx) sup) sub.union

and isa_union_2 ctx (sub: Type.inter) (sup: Type.union) =
  List.exists (isa_inter_1 ctx sub) sup.union

and isa_inter_1 ctx (sub: Type.inter) (sup: Type.inter) =
  List.for_all (isa_inter_2 ctx sub) sup.inter

and isa_inter_2 ctx (sub: Type.inter) (sup: Type.base) =
  List.exists (Utils.flip (isa_base ctx) sup) sub.inter

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
  | Char   , Char    ->
    true
  | String , String  ->
    true
  | Var sub_var, _ ->
    isa_var ctx sub_var sup
  | Tuple sub_tuple, Tuple sup_tuple ->
    isa_tuple ctx sub_tuple sup_tuple
  | Record sub_record, Record sup_record ->
    isa_record ctx sub_record sup_record
  | AbsExpr sub_abs, AbsExpr sup_abs ->
    isa_abs_expr ctx sub_abs sup_abs
  | AbsTypeExpr sub_abs, AbsTypeExpr sup_abs ->
    isa_abs_type_expr ctx sub_abs sup_abs
  | Abs sub_abs, Abs sup_abs ->
    isa_abs ctx sub_abs sup_abs
  | App left_app, App right_app ->
    isa_app ctx left_app right_app
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
  Utils.NameMap.for_all (fun _ sup_attr -> isa_record_attr ctx sub_record sup_attr) sup_record.attrs

and isa_record_attr ctx sub_record sup_attr =
  match Utils.NameMap.find_opt sup_attr.name sub_record.attrs with
  | Some sub_attr ->
    isa ctx sub_attr.type' sup_attr.type'
  | None ->
    false

and isa_abs_expr ctx sub_abs sup_abs =
  isa ctx sup_abs.param sub_abs.param &&
  isa ctx sub_abs.ret sup_abs.ret

and isa_abs_type_expr ctx sub_abs sup_abs =
  is_param ctx sub_abs.param sup_abs.param &&
  let sup_ret = substitute_right ctx sub_abs.param sup_abs.param sup_abs.ret in
  let ctx = TypeContext.add_bind_type ctx sub_abs.param.bind sub_abs.param.bound in
  isa ctx sub_abs.ret sup_ret

and isa_abs ctx sub_abs sup_abs =
  is_param ctx sub_abs.param sup_abs.param &&
  let sup_body = substitute_right ctx sub_abs.param sup_abs.param sup_abs.body in
  let ctx = TypeContext.add_bind_type ctx sub_abs.param.bind sub_abs.param.bound in
  isa ctx sub_abs.body sup_body

and isa_app ctx sub_app sup_app =
  isa ctx sub_app.abs sup_app.abs &&
  is ctx sub_app.arg sup_app.arg

(* TYPE SUBSTITUTION *)

and substitute_arg ctx bind arg type' =
  let entry = TypeContext.entry bind arg in
  substitute ctx entry type'

and substitute_right ctx param_bind param_arg type' =
  let bind = param_bind.Type.bind in
  (* TODO: Check if this is necessary *)
  let ctx = TypeContext.add_bind_type ctx param_bind.bind param_bind.bound in
  let ctx = TypeContext.add_bind_type ctx param_arg.bind param_arg.bound in
  let arg = Type.base (Var { bind = param_arg.Type.bind }) in
  let entry = TypeContext.entry bind arg in
  substitute ctx entry type'

and substitute ctx entry (type': Type.type')  =
  map_type ctx (substitute_base ctx entry) type'

and substitute_base ctx entry (type': Type.base) =
  match type' with
  | Top    -> Type.base Top
  | Bot    -> Type.base Bot
  | Unit   -> Type.base Unit
  | Bool   -> Type.base Bool
  | Int    -> Type.base Int
  | Char   -> Type.base Char
  | String -> Type.base String
  | Var var ->
    substitute_var entry var
  | Tuple tuple ->
    let elems = List.map (substitute ctx entry) tuple.elems in
    Type.base (Tuple { elems })
  | Record record ->
    let attrs = Utils.NameMap.map (substitute_attr ctx entry) record.attrs in
    Type.base (Record { attrs })
  | AbsExpr abs ->
    let param = substitute ctx entry abs.param in
    let ret = substitute ctx entry abs.ret in
    Type.base (AbsExpr { param; ret })
  | AbsTypeExpr abs ->
    let param = substitute_param ctx entry abs.param in
    let ret = substitute ctx entry abs.ret in
    Type.base (AbsTypeExpr { param; ret })
  | Abs abs ->
    let param = substitute_param ctx entry abs.param in
    let body = substitute ctx entry abs.body in
    Type.base (Abs { param; body })
  | App app ->
    let abs = substitute ctx entry app.abs in
    let arg = substitute ctx entry app.arg in
    compute ctx abs arg

and substitute_var entry var =
  if var.bind == entry.bind then
    entry.bound
  else
    Type.base (Var var)

and substitute_param ctx entry param =
  let bound = substitute ctx entry param.bound in
  { param with bound }

and substitute_attr ctx entry attr =
  let type' = substitute ctx entry attr.type' in
  { attr with type' }

(* TYPE COMPUTATION *)

and compute ctx (type': Type.type') (arg: Type.type') =
  map_type ctx (Utils.flip (compute_base ctx) arg) type'

and compute_base ctx (abs: Type.base) (arg: Type.type') =
  match abs with
  | Abs abs ->
    substitute_arg ctx abs.param.bind arg abs.body
  | _ ->
    Type.base (Type.App { abs = Type.base abs; arg })

(* TYPE MAP *)

and map_type ctx f type' =
  map_union ctx (map_inter ctx f) type'

and map_union ctx f union =
  let types = List.map f union.union in
  Utils.reduce_list (join ctx) types

and map_inter ctx f inter =
  let types = List.map f inter.inter in
  Utils.reduce_list (meet ctx) types

(* TYPE JOIN *)

and join ctx (left: Type.type') (right: Type.type') =
  let types = Utils.collapse (join_inter ctx) (left.union @ right.union) in
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
  let types = Utils.product_lists (meet_inter ctx) left.union right.union in
  let types = Utils.collapse (join_inter ctx) types in
  { Type.union = types }

and meet_inter ctx (left: Type.inter) (right: Type.inter) =
  let types = Utils.collapse (meet_base ctx) (left.inter @ right.inter) in
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
  | Char   , Char   -> Some Char
  | String , String -> Some String
  | Var left_var, _ ->
    meet_var ctx left_var right
  | _, Var right_var ->
    meet_var ctx right_var left
  | Tuple left_tuple, Tuple right_tuple ->
    meet_tuple ctx left_tuple right_tuple
  | Record left_record, Record right_record ->
    meet_record ctx left_record right_record
  | AbsExpr left_abs, AbsExpr right_abs ->
    meet_abs_expr ctx left_abs right_abs
  | AbsTypeExpr left_abs, AbsTypeExpr right_abs ->
    meet_abs_type_expr ctx left_abs right_abs
  | Abs left_abs, Abs right_abs ->
    meet_abs ctx left_abs right_abs
  (* For the two next rules, find the maximum type of app and check if subtype of other *)
  | App _, _ ->
    None
  | _, App _ ->
    None
  | _, _ ->
    Some Bot

and meet_var ctx var other =
  if isa_var ctx var other then
    Some (Var var)
  else match other with
  | Var other_var ->
    Some (Var other_var)
  | _ ->
    None

and meet_tuple ctx left right =
  if List.compare_lengths left.elems right.elems != 0 then
    Some Bot
  else
  let elems = List.map2 (meet ctx) left.elems right.elems in
  Some (Tuple { elems })

and meet_record ctx left right =
  let attrs = Utils.NameMap.merge (meet_record_attr ctx) left.attrs right.attrs in
  Some (Record { attrs })

and meet_record_attr ctx name left right =
  match left, right with
  | Some left, Some right ->
    let type' = meet ctx left.type' right.type' in
    Some { name; type' }
  | Some left, None ->
    Some left
  | None, Some right ->
    Some right
  | None, None ->
    None

and meet_abs_expr ctx left right =
  match is ctx left.param right.param, is ctx left.ret right.ret with
  | true, true ->
    None
  | _, _ ->
    let param = join ctx left.param right.param in
    let ret = meet ctx left.ret right.ret in
    Some (AbsExpr { param; ret })

and meet_abs_type_expr ctx left right =
  if not (is_param ctx left.param right.param) then
    Some Bot
  else
  let right_ret = substitute_right ctx left.param right.param right.ret in
  let ctx = TypeContext.add_bind_type ctx left.param.bind left.param.bound in
  let ret = meet ctx left.ret right_ret in
  Some (AbsTypeExpr { param = left.param; ret })

and meet_abs ctx left right =
  if not (is_param ctx left.param right.param) then
    Some Bot
  else
  let right_body = substitute_right ctx left.param right.param right.body in
  let ctx = TypeContext.add_bind_type ctx left.param.bind left.param.bound in
  let body = meet ctx left.body right_body in
  Some (Abs { param = left.param; body })
