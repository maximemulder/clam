let rec compare (left: Model.type') (right: Model.type') =
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
    Utils.compare_lists compare left_tuple.elems right_tuple.elems
  | (TypeRecord left_record, TypeRecord right_record) ->
    Utils.compare_maps compare_attr left_record.attrs right_record.attrs
  | (TypeInter left_inter, TypeInter right_inter) ->
    compare left_inter.left right_inter.left
    && compare left_inter.right right_inter.right
  | (TypeUnion left_union, TypeUnion right_union) ->
    compare left_union.left right_union.left
    && compare left_union.right right_union.right
  | (TypeAbsExpr left_abs, TypeAbsExpr right_abs) ->
    Utils.compare_lists compare left_abs.params right_abs.params
    && compare left_abs.body right_abs.body
  | (TypeAbsExprType left_abs, TypeAbsExprType right_abs) ->
    Utils.compare_lists compare_param left_abs.params right_abs.params
    && compare left_abs.body right_abs.body
  | (TypeAbs left_abs, TypeAbs right_abs) ->
    Utils.compare_lists compare_param left_abs.params right_abs.params
    && compare left_abs.body right_abs.body
  | (TypeApp left_app, TypeApp right_app) ->
    compare left_app.type' right_app.type' &&
    Utils.compare_lists compare left_app.args right_app.args
  | _ -> false

and compare_param left_param right_param =
  compare left_param.type' right_param.type'

and compare_attr left_attr right_attr =
  compare left_attr.type' right_attr.type'
