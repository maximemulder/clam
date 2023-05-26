open Model

let rec merge_union left right =
  match (snd left, snd right) with
  | (TypeAny, TypeAny) ->
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
  | (_, _) ->
    TypeUnion (left, right)

let merge_inter left right =
  match (snd left, snd right) with
  | (TypeAny, TypeAny) ->
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
  | (_, _) ->
    TypeInter (left, right)
