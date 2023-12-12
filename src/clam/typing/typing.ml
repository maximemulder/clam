open Model

(* TYPE PROMOTION *)

let rec promote type' =
  match type' with
  | TypeVar var -> promote_var var
  | _ -> type'

and promote_var var =
  promote var.param.bound

(* BOTTOM EQUIVALENCE *)

let rec type_is_bot type' =
  match type' with
  | TypeBot _ -> true
  | TypeVar var -> var_is_bot var
  | _ -> false

and var_is_bot var =
  type_is_bot var.param.bound

(* TYPE EQUIVALENCE *)

let rec is left right =
  let left  = normalize left  in
  let right = normalize right in
  match (left, right) with
  | (TypeTop    _, TypeTop    _) -> true
  | (TypeBot    _,            _) -> type_is_bot right
  | (TypeUnit   _, TypeUnit   _) -> true
  | (TypeBool   _, TypeBool   _) -> true
  | (TypeInt    _, TypeInt    _) -> true
  | (TypeChar   _, TypeChar   _) -> true
  | (TypeString _, TypeString _) -> true
  | (TypeUnion _, _) ->
    is_union left right
  | (_, TypeUnion _) ->
    is_union left right
  | (_, TypeInter _) ->
    is_inter left right
  | (TypeInter _, _) ->
    is_inter left right
  | (TypeVar left_var, _) ->
    is_var left_var right
  | (TypeTuple left_tuple, TypeTuple right_tuple) ->
    Utils.compare_lists is left_tuple.elems right_tuple.elems
  | (TypeRecord left_record, TypeRecord right_record) ->
    Utils.compare_maps is_attr left_record.attrs right_record.attrs
  | (TypeAbsExpr left_abs, TypeAbsExpr right_abs) ->
    is left_abs.param right_abs.param
    && is left_abs.body right_abs.body
  | (TypeAbsExprType left_abs, TypeAbsExprType right_abs) ->
    is_abs_expr_type left_abs right_abs
  | (TypeAbs left_abs, TypeAbs right_abs) ->
    is_abs_type left_abs right_abs
  | (TypeApp left_app, TypeApp right_app) ->
    is_app_type left_app right_app
  | _ -> false

and is_union left right =
  let lefts  = collect_union left  in
  let rights = collect_union right in
  List.for_all (fun left -> List.exists (fun right -> isa left right) rights) lefts &&
  List.for_all (fun right -> List.exists (fun left -> isa right left) lefts) rights

and is_inter left right =
  let lefts =  collect_inter left  in
  let rights = collect_inter right in
  List.for_all (fun left -> List.exists (fun right -> isa right left) rights) lefts &&
  List.for_all (fun right -> List.exists (fun left -> isa left right) lefts) rights

and is_var left_var right =
  if var_is_bot left_var && type_is_bot right then
    true
  else match right with
  | TypeVar right_var -> left_var.param == right_var.param
  | _ -> false

and is_abs_expr_type left_abs right_abs =
  is_param left_abs.param right_abs.param
  && let right_body = TypingApp.apply_abs_expr_param right_abs left_abs.param in
  is left_abs.body right_body

and is_abs_type left_abs right_abs =
  is_param left_abs.param right_abs.param
  && let right_body = TypingApp.apply_abs_param right_abs left_abs.param in
  is left_abs.body right_body

and is_app_type left_app right_app =
  is left_app.type' right_app.type'
  && is left_app.arg right_app.arg

and is_attr left_attr right_attr =
  is left_attr.type' right_attr.type'

and is_param left_param right_param =
  is left_param.bound right_param.bound

(* TYPE SUB *)

and isa sub sup: bool =
  let sub = normalize sub in
  let sup = normalize sup in
  match (sub, sup) with
  | (           _, TypeTop    _) ->
    isa_top sub
  | (TypeBot    _,            _) -> true
  | (TypeUnit   _, TypeUnit   _) -> true
  | (TypeBool   _, TypeBool   _) -> true
  | (TypeInt    _, TypeInt    _) -> true
  | (TypeChar   _, TypeChar   _) -> true
  | (TypeString _, TypeString _) -> true
  | (TypeUnion sub_union, _) ->
    let left  = isa sub_union.left sup  in
    let right = isa sub_union.right sup in
    left && right
  | (_, TypeUnion sup_union) ->
    let left  = isa sub sup_union.left  in
    let right = isa sub sup_union.right in
    left || right
  | (_, TypeInter sup_inter) ->
    let left  = isa sub sup_inter.left  in
    let right = isa sub sup_inter.right in
    left && right
  | (TypeInter sub_inter, _) ->
    let left  = isa sub_inter.left sup  in
    let right = isa sub_inter.right sup in
    left || right
  | (TypeVar sub_var, _) ->
    isa_var sub_var sup
  | (TypeTuple sub_tuple, TypeTuple sup_tuple) ->
    isa_tuple sub_tuple sup_tuple
  | (TypeRecord sub_record, TypeRecord sup_record) ->
    isa_record sub_record sup_record
  | (TypeAbsExpr sub_abs, TypeAbsExpr sup_abs) ->
    let params = isa sup_abs.param sub_abs.param in
    let body = isa sub_abs.body sup_abs.body in
    params && body
  | (TypeAbsExprType sub_abs, TypeAbsExprType sup_abs) ->
    isa_abs_expr_type sub_abs sup_abs
  | (TypeAbs sub_abs, TypeAbs sup_abs) ->
    isa_abs_type sub_abs sup_abs
  | (TypeApp left_app, TypeApp right_app) ->
    isa_app_type left_app right_app
  | _ ->
    false

and isa_top sub =
  match TypingKind.get_kind sub with
  | TypingKind.Type -> true
  | TypingKind.Abs _ -> false

and isa_var sub_var sup =
  var_is_bot sub_var
  || match sup with
  | TypeVar sup_var when sub_var.param == sup_var.param ->
    true
  | _ ->
    isa sub_var.param.bound sup

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

and isa_abs_expr_type sub_abs sup_abs =
  is_param sub_abs.param sup_abs.param
  && let sup_body = TypingApp.apply_abs_expr_param sup_abs sub_abs.param in
  isa sub_abs.body sup_body

and isa_abs_type sub_abs sup_abs =
  is_param sub_abs.param sup_abs.param
  && let sup_body = TypingApp.apply_abs_param sup_abs sub_abs.param in
  isa sub_abs.body sup_body

and isa_app_type sub_app sup_app =
  isa sub_app.type' sup_app.type'
  && is sub_app.arg sup_app.arg

(* TYPE NORMALIZATION *)

(* TODO: I am not sure this function works with inter-nested unions, intersections and type abstractions *)
(* It may be easier to implement a simplified form for types than to correct it immediatly *)
and normalize type' =
  let type' = Utils.reduce_list join_type (distribute_unions type') in
  simplify type'

and simplify type' =
  match type' with
  | TypeApp app ->
    simplify_app app
  | _ ->
    type'

and simplify_app app =
  let abs = normalize app.type' in
  match abs with
  | TypeAbs { param; body; _ } ->
    let entry = TypingApp.entry param app.arg in
    normalize (TypingApp.apply body entry)
  | type' -> type'

and distribute_unions type' =
  match type' with
  | TypeUnion { left; right; _ } ->
    let lefts = distribute_unions left in
    let rights = distribute_unions right in
    List.append lefts rights
  | TypeInter { left; right; _ } ->
    let lefts = distribute_inters_over_unions left in
    let rights = distribute_inters_over_unions right in
    Utils.product_lists (fun left right -> Utils.reduce_list meet (List.append left right)) lefts rights
  | _ -> [type']

and distribute_inters_over_unions type' =
  match type' with
  | TypeInter { left; right; _ } ->
    let lefts = distribute_inters_over_unions left in
    let rights = distribute_inters_over_unions right in
    Utils.product_lists List.append lefts rights
  | TypeUnion { left; right; _ } ->
    let lefts = distribute_unions left in
    let rights = distribute_unions right in
    let types = List.append lefts rights in
    List.map (fun type' -> [type']) types
  | _ -> [[type']]

(* COLLECT *)

and collect_union type' =
  match type' with
  | TypeUnion union ->
    List.append (collect_union union.left) (collect_union union.right)
  | _ ->
    [type']

and collect_inter type' =
  match type' with
  | TypeInter inter ->
    List.append (collect_inter inter.left) (collect_inter inter.right)
  | _ ->
    [type']

(* TYPE JOIN *)

and join left right =
  let types = [] in
  let types = List.append types (collect_union left)  in
  let types = List.append types (collect_union right) in
  Utils.reduce_list join_type types

and join_type left right =
  if isa left right then
    right
  else
  if isa right left then
    left
  else
    let pos = type_pos left in
    TypeUnion { pos; left; right }

(* TYPE MEET *)

and meet left right =
  let types = [] in
  let types = List.append types (collect_inter left)  in
  let types = List.append types (collect_inter right) in
  Utils.reduce_list meet_type types

and meet_type left right =
  let pos = type_pos left in
  match (left, right) with
  | (TypeTop    _, right       ) -> right
  | (left        , TypeTop    _) -> left
  | (TypeBot    _,            _) -> TypeBot    { pos }
  | (           _, TypeBot    _) -> TypeBot    { pos }
  | (TypeUnit   _, TypeUnit   _) -> TypeUnit   { pos }
  | (TypeBool   _, TypeBool   _) -> TypeBool   { pos }
  | (TypeInt    _, TypeInt    _) -> TypeInt    { pos }
  | (TypeChar   _, TypeChar   _) -> TypeChar   { pos }
  | (TypeString _, TypeString _) -> TypeString { pos }
  | (TypeVar left_var, _) when isa_var left_var right ->
    TypeVar left_var
  | (_, TypeVar right_var) when isa_var right_var left ->
    TypeVar right_var
  | (TypeVar _, _) ->
    TypeInter { pos; left; right }
  | (_, TypeVar _) ->
    TypeInter { pos; left; right }
  | (TypeTuple left_tuple, TypeTuple right_tuple) ->
    meet_tuple left_tuple right_tuple
  | (TypeRecord left_record, TypeRecord right_record) ->
    meet_record left_record right_record
  | (TypeInter _, _) ->
    TypeInter { pos; left; right }
  | (_, TypeInter _) ->
    TypeInter { pos; left; right }
  | (TypeUnion _, _) ->
    TypeInter { pos; left; right }
  | (_, TypeUnion _) ->
    TypeInter { pos; left; right }
  | (TypeAbsExpr left_abs, TypeAbsExpr right_abs) ->
    meet_abs_expr left_abs right_abs
  | (TypeAbsExprType left_abs, TypeAbsExprType right_abs) ->
    meet_abs_expr_type left_abs right_abs
  | (TypeAbs left_abs, TypeAbs right_abs) ->
    meet_abs left_abs right_abs
  (* TODO: The two next cases are probably wrong but I am too done to fix it now *)
  | (TypeApp left_app, _) ->
    let left = simplify_app left_app in
    meet left right
  | (_, TypeApp right_app) ->
    let right = simplify_app right_app in
    meet left right
  | (_, _) ->
    TypeBot { pos }

and meet_tuple left_tuple right_tuple =
  if List.compare_lengths left_tuple.elems right_tuple.elems != 0 then
    TypeBot { pos = left_tuple.pos }
  else
  let elems = List.map2 meet left_tuple.elems right_tuple.elems in
  TypeTuple { pos = left_tuple.pos; elems }

and meet_record left_record right_record =
  let attrs = Utils.NameMap.merge meet_record_attr left_record.attrs right_record.attrs in
  TypeRecord { pos = left_record.pos; attrs }

and meet_record_attr name left_attr right_attr =
  match (left_attr, right_attr) with
  | (Some left_attr, Some right_attr) ->
    let type' = meet left_attr.type' right_attr.type' in
    Some { pos = left_attr.pos; name; type' }
  | (Some left_attr, None) ->
    Some left_attr
  | (None, Some right_attr) ->
    Some right_attr
  | (None, None) ->
    None

and meet_abs_expr left_abs right_abs =
  let pos = left_abs.pos in
  if is left_abs.param right_abs.param then
    let body = meet left_abs.body right_abs.body in
    TypeAbsExpr { pos = left_abs.pos; param = left_abs.param; body }
  else if is left_abs.body right_abs.body then
    let param = join left_abs.param right_abs.param in
    TypeAbsExpr { pos = left_abs.pos; param; body = left_abs.body }
  else
  TypeInter { pos; left = TypeAbsExpr left_abs; right = TypeAbsExpr right_abs }

and meet_abs_expr_type left_abs right_abs =
  if not (is_param left_abs.param right_abs.param) then
    TypeBot { pos = left_abs.pos }
  else
  let right_body = TypingApp.apply_abs_expr_param right_abs left_abs.param in
  let body = meet left_abs.body right_body in
  TypeAbsExprType { pos = left_abs.pos; param = left_abs.param; body }

and meet_abs left_abs right_abs =
  if not (is_param left_abs.param right_abs.param) then
    TypeBot { pos = left_abs.pos }
  else
  let right_body = TypingApp.apply_abs_param right_abs left_abs.param in
  let body = meet left_abs.body right_body in
  TypeAbs { pos = left_abs.pos; param = left_abs.param; body }
