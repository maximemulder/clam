open Model

let rec merge_union left right =
  match (snd left, snd right) with
  | (TypeAny, _) ->
    TypeAny
  | (_, TypeAny) ->
    TypeAny
  | (TypeVoid, TypeVoid) ->
    TypeVoid
  | (TypeBool, TypeBool) ->
    TypeBool
  | (TypeInt, TypeInt) ->
    TypeInt
  | (TypeChar, TypeChar) ->
    TypeChar
  | (TypeString, TypeString) ->
    TypeString
  | (TypeVar param, TypeVar other_param) when param = other_param ->
    TypeVar param
  | (TypeUnion (other_left, other_right), _) ->
    let union = (fst left, (merge_union other_left other_right)) in
    merge_union union right
  | (_, TypeUnion (other_left, other_right)) ->
    let union = (fst left, (merge_union other_left other_right)) in
    merge_union union right
  | (_, _) ->
    TypeUnion (left, right)

let rec merge_inter left right =
  match (snd left, snd right) with
  | (TypeAny, right) ->
    right
  | (left, TypeAny) ->
    left
  | (TypeVoid, TypeVoid) ->
    TypeVoid
  | (TypeBool, TypeBool) ->
    TypeBool
  | (TypeInt, TypeInt) ->
    TypeInt
  | (TypeChar, TypeChar) ->
    TypeChar
  | (TypeString, TypeString) ->
    TypeString
  | (TypeVar param, TypeVar other_param) when param = other_param ->
    TypeVar param
  | (TypeInter (other_left, other_right), _) ->
    let inter = (fst left, (merge_inter other_left other_right)) in
    merge_inter inter right
  | (_, TypeInter (other_left, other_right)) ->
    let inter = (fst left, (merge_inter other_left other_right)) in
    merge_inter inter right
  | (_, _) ->
    TypeInter (left, right)
