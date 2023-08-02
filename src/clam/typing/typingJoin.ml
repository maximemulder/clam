open Model

let rec join (left: type') (right: type'): type' =
  let pos = type_pos left in
  match (left, right) with
  | (TypeTop _, _) ->
    TypeTop { pos }
  | (_, TypeTop _) ->
    TypeTop { pos }
  | (TypeUnit _, TypeUnit _) ->
    TypeUnit { pos }
  | (TypeBool _, TypeBool _) ->
    TypeBool { pos}
  | (TypeInt _, TypeInt _) ->
    TypeInt { pos }
  | (TypeChar _, TypeChar _) ->
    TypeChar { pos }
  | (TypeString _, TypeString _) ->
    TypeString { pos }
  | (TypeVar param, TypeVar other) when param = other ->
    TypeVar param
  | (TypeUnion other, _) ->
    let union = join other.left other.right in
    join union right
  | (_, TypeUnion other) ->
    let union = join other.left other.right in
    join left union
  | (_, _) ->
    TypeUnion { pos; left; right }
