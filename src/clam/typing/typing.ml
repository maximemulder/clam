open Model

(* TYPE EQUIVALENCE *)

let rec is left right =
  let left = normalize left in
  let right = normalize right in
  match (left, right) with
  | (TypeTop    _, TypeTop    _) -> true
  | (TypeBot    _, TypeBot    _) -> true
  | (TypeUnit   _, TypeUnit   _) -> true
  | (TypeBool   _, TypeBool   _) -> true
  | (TypeInt    _, TypeInt    _) -> true
  | (TypeChar   _, TypeChar   _) -> true
  | (TypeString _, TypeString _) -> true
  | (TypeVar left_var, TypeVar right_var) ->
    left_var.param = right_var.param
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
    is (TypingApply.apply_app left_app) right
  | (_, TypeApp right_app) ->
    is left (TypingApply.apply_app right_app)
  | _ -> false

and is_param param other =
  is param.type' other.type'

and is_attr attr other =
  is attr.type' other.type'

(* TYPE NORMALIZATION *)

and normalize type' =
  make_union (collect_union type')

and collect_union type' =
  match type' with
  | TypeUnion { left; right; _ } ->
    let lefts = collect_union left in
    let rights = collect_union right in
    List.append lefts rights
  | TypeInter { left; right; _ } ->
    let lefts = collect_inter left in
    let rights = collect_inter right in
    Utils.product_lists (fun left right -> make_inter (List.append left right)) lefts rights
  | _ -> [type']

and collect_inter type' =
  match type' with
  | TypeInter { left; right; _ } ->
    let lefts = collect_inter left in
    let rights = collect_inter right in
    Utils.product_lists List.append lefts rights
  | TypeUnion { left; right; _ } ->
    let lefts = collect_union left in
    let rights = collect_union right in
    let types = List.append lefts rights in
    List.map (fun type' -> [type']) types
  | _ -> [[type']]

and make_union types =
  match types with
  | [type'] -> type'
  | left :: rights ->
    join left (make_union rights)
  | _ ->
    invalid_arg "Typing.make_union"

and make_inter types =
  match types with
  | [type'] -> type'
  | left :: rights ->
    meet left (make_inter rights)
  | _ ->
    invalid_arg "Typing.make_inter"

(* TYPE JOIN *)

and join left right =
  let pos = type_pos left in
  match (left, right) with
  | (TypeTop    _,            _) -> TypeTop    { pos }
  | (_           , TypeTop    _) -> TypeTop    { pos }
  | (TypeBot    _, right       ) -> right
  | (left        , TypeBot    _) -> left
  | (TypeUnit   _, TypeUnit   _) -> TypeUnit   { pos }
  | (TypeBool   _, TypeBool   _) -> TypeBool   { pos }
  | (TypeInt    _, TypeInt    _) -> TypeInt    { pos }
  | (TypeChar   _, TypeChar   _) -> TypeChar   { pos }
  | (TypeString _, TypeString _) -> TypeString { pos }
  | (TypeVar left_var, TypeVar right_var) when left_var = right_var ->
    TypeVar left_var
  | (TypeUnion left_union, _) ->
    let union = join left_union.left left_union.right in
    join union right
  | (_, TypeUnion right_union) ->
    let union = join right_union.left right_union.right in
    join left union
  | (_, _) ->
    TypeUnion { pos; left; right }

(* TYPE MEET *)

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
  | (TypeVar param, TypeVar other) when param = other ->
    TypeVar param
  | (TypeInter other, _) ->
    let inter = meet other.left other.right in
    meet inter right
  | (_, TypeInter other) ->
    let inter = meet other.left other.right in
    meet left inter
  | (TypeAbsExpr left_abs, TypeAbsExpr right_abs) ->
    meet_abs_expr left_abs right_abs
  | (_, _) ->
    TypeInter { pos; left; right }

and meet_abs_expr left_abs right_abs =
  if List.compare_lengths left_abs.params right_abs.params != 0 then
    prim_bot
  else
  let params = List.combine left_abs.params right_abs.params in
  let params = List.map (fun (left_param, right_param) -> meet left_param right_param) params in
  let body = meet left_abs.body right_abs.body in
  TypeAbsExpr { pos = left_abs.pos; params; body }
