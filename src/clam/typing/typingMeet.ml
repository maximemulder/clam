open Model

let rec meet left right =
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
    let inter = (fst left, (meet other_left other_right)) in
    meet inter right
  | (_, TypeInter (other_left, other_right)) ->
    let inter = (fst left, (meet other_left other_right)) in
    meet inter right
  | (_, _) ->
    TypeInter (left, right)
