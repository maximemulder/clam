open Model

module Reader = struct
  type r = TypingContext.context
end

open Monad.Monad(Monad.ReaderMonad(Reader))

(* UTILITIES *)

let rec type_is_bot type' =
  match type' with
  | TypeBot _ -> true
  | TypeVar var -> var_is_bot var
  | _ -> false

and var_is_bot var =
  type_is_bot var.param.type'

(* TYPE EQUIVALENCE *)

let rec is left right =
  let left  = normalize left  in
  let right = normalize right in
  is_union left right ||
  is_inter left right ||
  is_type  left right

and is_union left right =
  let lefts  = collect_union left  in
  let rights = collect_union right in
  List.for_all (fun left -> List.exists (fun right -> isa left right TypingContext.empty) rights) lefts &&
  List.for_all (fun right -> List.exists (fun left -> isa right left TypingContext.empty) lefts) rights

and is_inter left right =
  let lefts =  collect_inter left  in
  let rights = collect_inter right in
  List.for_all (fun left -> List.exists (fun right -> isa left right TypingContext.empty) rights) lefts &&
  List.for_all (fun right -> List.exists (fun left -> isa right left TypingContext.empty) lefts) rights

and is_type left right =
  match (left, right) with
  | (TypeTop    _, TypeTop    _) -> true
  | (TypeBot    _,            _) -> type_is_bot right
  | (TypeUnit   _, TypeUnit   _) -> true
  | (TypeBool   _, TypeBool   _) -> true
  | (TypeInt    _, TypeInt    _) -> true
  | (TypeChar   _, TypeChar   _) -> true
  | (TypeString _, TypeString _) -> true
  | (TypeVar left_var,        _) -> is_var left_var right
  | (TypeTuple left_tuple, TypeTuple right_tuple) ->
    Utils.compare_lists is left_tuple.elems right_tuple.elems
  | (TypeRecord left_record, TypeRecord right_record) ->
    Utils.compare_maps is_attr left_record.attrs right_record.attrs
  | (TypeInter left_inter, TypeInter right_inter) ->
    is left_inter.left right_inter.left
    && is left_inter.right right_inter.right
  | (TypeUnion left_union, TypeUnion right_union) ->
    is left_union.left right_union.left
    && is left_union.right right_union.right
  | (TypeAbsExpr left_abs, TypeAbsExpr right_abs) ->
    Utils.compare_lists is left_abs.params right_abs.params
    && is left_abs.body right_abs.body
  | (TypeAbsExprType left_abs, TypeAbsExprType right_abs) ->
    Utils.compare_lists is_param left_abs.params right_abs.params
    && is left_abs.body right_abs.body
  | (TypeAbs left_abs, TypeAbs right_abs) ->
    Utils.compare_lists is_param left_abs.params right_abs.params
    && is left_abs.body right_abs.body
  | (TypeApp left_app, _) ->
    is (TypingApp.apply_app left_app) right
  | (_, TypeApp right_app) ->
    is left (TypingApp.apply_app right_app)
  | _ -> false
  (* TODO: Adapt this function to unions and intersections *)
  (* TODO: Adapt this function to type abstractions *)

and is_var left_var right =
  if var_is_bot left_var && type_is_bot right then
    true
  else match right with
  | TypeVar right_var -> left_var.param = right_var.param
  | _ -> false

and is_param left_param right_param =
  is left_param.type' right_param.type'

and is_attr left_attr right_attr =
  is left_attr.type' right_attr.type'

(* TYPE SUB *)

(* TODO: Does isa really need a context ? Would it not be simpler to apply substitution ? *)
and isa (sub: type') (sup: type') =
  let sub = normalize sub in
  let sup = normalize sup in
  match (sub, sup) with
  | (           _, TypeTop    _) -> return true
  | (TypeBot    _,            _) -> return true
  | (TypeUnit   _, TypeUnit   _) -> return true
  | (TypeBool   _, TypeBool   _) -> return true
  | (TypeInt    _, TypeInt    _) -> return true
  | (TypeChar   _, TypeChar   _) -> return true
  | (TypeString _, TypeString _) -> return true
  | (TypeUnion sub_union, _) ->
    let* left  = isa sub_union.left sup  in
    let* right = isa sub_union.right sup in
    return (left && right)
  | (_, TypeUnion sup_union) ->
    let* left  = isa sub sup_union.left  in
    let* right = isa sub sup_union.right in
    return (left || right)
  | (_, TypeInter sup_inter) ->
    let* left  = isa sub sup_inter.left  in
    let* right = isa sub sup_inter.right in
    return (left && right)
  | (TypeInter sub_inter, _) ->
    let* left  = isa sub_inter.left sup  in
    let* right = isa sub_inter.right sup in
    return (left || right)
  | (TypeVar sub_var, _) ->
    isa_var sub_var sup
  | (TypeTuple sub_tuple, TypeTuple sup_tuple) ->
    isa_tuple sub_tuple sup_tuple
  | (TypeRecord sub_record, TypeRecord sup_record) ->
    isa_record sub_record sup_record
  | (TypeAbsExpr sub_abs, TypeAbsExpr sup_abs) ->
    let* params = compare_list2 isa sup_abs.params sub_abs.params in
    let* body = isa sub_abs.body sup_abs.body in
    return (params && body)
  | (TypeAbsExprType sub_abs, TypeAbsExprType sup_abs) ->
    isa_abs_expr_type sub_abs sup_abs
  | (TypeAbs sub_abs, TypeAbs sup_abs) ->
    let params = Utils.compare_lists is_param sub_abs.params sup_abs.params in
    let* body = isa sub_abs.body sup_abs.body in
    return (params && body)
  | (TypeApp sub_app, _) ->
    let sub = TypingApp.apply_app sub_app in
    isa sub sup
  | (_, TypeApp sup_app) ->
    let sup = TypingApp.apply_app sup_app in
    isa sub sup
  | _ ->
    return false
    (* TODO: Adapt this function to type abstractions *)

and isa_var sub_var sup =
  if var_is_bot sub_var then
    return true
  else
  let* sub_type = TypingContext.find_arg sub_var.param in
  match sub_type with
  | Some sub_type -> isa sub_type sup
  | None ->
  match sup with
  | TypeVar sup_var ->
    return (sub_var.param = sup_var.param)
  | _ ->
    isa sub_var.param.type' sup

and isa_tuple sub_tuple sup_tuple =
  compare_list2 isa sub_tuple.elems sup_tuple.elems

and isa_record sub_record sup_record context =
  Utils.NameMap.for_all (fun _ sup_attr -> isa_record_attr sub_record sup_attr context) sup_record.attrs

and isa_record_attr sub_record sup_attr =
  match Utils.NameMap.find_opt sup_attr.name sub_record.attrs with
  | Some sub_attr ->
    isa sub_attr.type' sup_attr.type'
  | None ->
    return false

and isa_abs_expr_type sub_abs sup_abs =
  if not (Utils.compare_lists is_param sub_abs.params sup_abs.params) then
    return false
  else
  let sup_body = TypingApp.apply_abs_expr_params sup_abs sub_abs.params in
  isa sub_abs.body sup_body

(* TYPE NORMALIZATION *)

and normalize type' =
  join_many (distribute_unions type')

and distribute_unions type' =
  match type' with
  | TypeUnion { left; right; _ } ->
    let lefts = distribute_unions left in
    let rights = distribute_unions right in
    List.append lefts rights
  | TypeInter { left; right; _ } ->
    let lefts = distribute_inters_over_unions left in
    let rights = distribute_inters_over_unions right in
    Utils.product_lists (fun left right -> meet_many (List.append left right)) lefts rights
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

and join_many types =
  match types with
  | [type'] ->
    type'
  | left :: rights ->
    let right = join_many rights in
    join left right
  | _ ->
    invalid_arg "Typing.join_many"

and join left right =
  if isa left right TypingContext.empty then
    right
  else
  if isa right left TypingContext.empty then
    left
  else
    let pos = type_pos left in
    TypeUnion { pos; left; right }

(* TYPE MEET *)

and meet_many types =
  match types with
  | [type'] ->
    type'
  | left :: rights ->
    meet left (meet_many rights)
  | _ ->
    invalid_arg "Typing.meet_many"

and meet left right =
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
  | (TypeVar left_var, _) when isa_var left_var right TypingContext.empty ->
    TypeVar left_var
  | (_, TypeVar right_var) when isa_var right_var left TypingContext.empty ->
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
  | (TypeUnion left_union, _) ->
    let result_left  = meet left_union.left  right in
    let result_right = meet left_union.right right in
    meet result_left result_right
  | (_, TypeUnion right_union) ->
    let result_left  = meet left right_union.left  in
    let result_right = meet left right_union.right in
    meet result_left result_right
  | (TypeAbsExpr left_abs, TypeAbsExpr right_abs) ->
    meet_abs_expr left_abs right_abs
  | (TypeAbsExprType left_abs, TypeAbsExprType right_abs) ->
    meet_abs_expr_type left_abs right_abs
  | (TypeAbs left_abs, TypeAbs right_abs) ->
    meet_abs left_abs right_abs
  | (TypeApp left_app, _) ->
    let left = TypingApp.apply_app left_app in
    meet left right
  | (_, TypeApp right_app) ->
    let right = TypingApp.apply_app right_app in
    meet left right
  | (_, _) ->
    TypeBot { pos }

and meet_tuple left_tuple right_tuple =
  if List.compare_lengths left_tuple.elems right_tuple.elems != 0 then
    prim_bot
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
  if List.compare_lengths left_abs.params right_abs.params != 0 then
    prim_bot
  else
  let params = List.map2 meet left_abs.params right_abs.params in
  let body = meet left_abs.body right_abs.body in
  TypeAbsExpr { pos = left_abs.pos; params; body }

and meet_abs_expr_type left_abs right_abs =
  if List.compare_lengths left_abs.params right_abs.params != 0 then
    prim_bot
  else if not (List.for_all2 is_param left_abs.params right_abs.params) then
    prim_bot
  else
  let right_body = TypingApp.apply_abs_expr_params right_abs left_abs.params in
  let body = meet left_abs.body right_body in
  TypeAbsExprType { pos = left_abs.pos; params = left_abs.params; body }

and meet_abs left_abs right_abs =
  if List.compare_lengths left_abs.params right_abs.params != 0 then
    prim_bot
  else if not (List.for_all2 is_param left_abs.params right_abs.params) then
    prim_bot
  else
  let right_body = TypingApp.apply_abs_params right_abs left_abs.params in
  let body = meet left_abs.body right_body in
  TypeAbs { pos = left_abs.pos; params = left_abs.params; body }
