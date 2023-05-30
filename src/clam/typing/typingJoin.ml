open Model

let rec join left right =
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
    let union = (fst left, (join other_left other_right)) in
    join union right
  | (_, TypeUnion (other_left, other_right)) ->
    let union = (fst left, (join other_left other_right)) in
    join union right
  | (_, _) ->
    TypeUnion (left, right)
