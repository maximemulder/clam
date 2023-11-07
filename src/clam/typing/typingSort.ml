(* WIP, function to sort types to allow types to have a standard form *)
let rec sort (left: Model.type') (right: Model.type') =
  match (left, right) with
  | (TypeTop _, TypeTop _) -> 0
  | (TypeTop _, _) -> +1
  | (_, TypeTop _) -> -1
  | (TypeBot _, TypeBot _) -> 0
  | (TypeBot _, _) -> +1
  | (_, TypeBot _) -> -1
  | (TypeUnit _, TypeUnit _) -> 0
  | (TypeUnit _, _) -> +1
  | (_, TypeUnit _) -> -1
  | (TypeBool _, TypeBool _) -> 0
  | (TypeBool _, _) -> +1
  | (_, TypeBool _) -> -1
  | (TypeInt _, TypeInt _) -> 0
  | (TypeInt _, _) -> +1
  | (_, TypeInt _) -> -1
  | (TypeChar _, TypeChar _) -> 0
  | (TypeChar _, _) -> +1
  | (_, TypeChar _) -> -1
  | (TypeString _, TypeString _) -> 0
  | (TypeString _, _) -> +1
  | (_, TypeString _) -> -1
  | (TypeVar left_var, TypeVar right_var) ->
    String.compare left_var.param.name right_var.param.name
  | (TypeVar _, _) -> +1
  | (_, TypeVar _) -> -1
  | (TypeUnion left_union, TypeUnion right_union) ->
    let res = sort left_union.left right_union.left in
    if res != 0 then
      res
    else
    let res = sort left_union.right right_union.right in
    if res <> 0 then
      res
    else
    0
  | (TypeUnion _, _) -> +1
  | (_, TypeUnion _) -> -1
  | (TypeInter left_inter, TypeInter right_inter) ->
    let res = sort left_inter.left right_inter.left in
    if res != 0 then
      res
    else
    let res = sort left_inter.right right_inter.right in
    if res <> 0 then
      res
    else
    0
  | (TypeInter _, _) -> +1
  | (_, TypeInter _) -> -1
  | _ -> 0
