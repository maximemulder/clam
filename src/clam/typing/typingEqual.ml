let rec is_type (type': Model.type') (other: Model.type') =
  let type' = TypingSet.normalize type' in
  let other = TypingSet.normalize other in
  match (type', other) with
  | (TypeTop    _, TypeTop    _) -> true
  | (TypeBot    _, TypeBot    _) -> true
  | (TypeUnit   _, TypeUnit   _) -> true
  | (TypeBool   _, TypeBool   _) -> true
  | (TypeInt    _, TypeInt    _) -> true
  | (TypeChar   _, TypeChar   _) -> true
  | (TypeString _, TypeString _) -> true
  | (TypeVar var, TypeVar other_var) ->
    var.param = other_var.param
  | (TypeTuple tuple, TypeTuple other_tuple) ->
    Utils.compare_lists is_type tuple.elems other_tuple.elems
  | (TypeRecord record, TypeRecord other_record) ->
    Utils.compare_maps is_type_attr record.attrs other_record.attrs
  | (TypeInter inter, TypeInter other_inter) ->
    is_type inter.left other_inter.left
    && is_type inter.right other_inter.right
  | (TypeUnion union, TypeUnion other_union) ->
    is_type union.left other_union.left
    && is_type union.right other_union.right
  | (TypeAbsExpr abs, TypeAbsExpr other_abs) ->
    Utils.compare_lists is_type abs.params other_abs.params
    && is_type abs.body other_abs.body
  | (TypeAbsExprType abs, TypeAbsExprType other_abs) ->
    Utils.compare_lists is_type_param abs.params other_abs.params
    && is_type abs.body other_abs.body
  | (TypeAbs abs, TypeAbs other_abs) ->
    Utils.compare_lists is_type_param abs.params other_abs.params
    && is_type abs.body other_abs.body
  | (TypeApp app, _) ->
    is_type (TypingApply.apply_app app) other
  | (_, TypeApp app) ->
    is_type type' (TypingApply.apply_app app)
  | _ -> false

and is_type_param param other =
  is_type param.type' other.type'

and is_type_attr attr other =
  is_type attr.type' other.type'
