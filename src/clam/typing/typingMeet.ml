open Model

let rec meet (left: type') (right: type'): type' =
  let pos = type_pos left in
  match (left, right) with
  | (TypeTop _, right) ->
    right
  | (left, TypeTop _) ->
    left
  | (TypeBot _, _) ->
    TypeBot { pos }
  | (_, TypeBot _) ->
    TypeBot { pos }
  | (TypeUnit _, TypeUnit _) ->
    TypeUnit { pos }
  | (TypeBool _, TypeBool _) ->
    TypeBool { pos }
  | (TypeInt _, TypeInt _) ->
    TypeInt { pos }
  | (TypeChar _, TypeChar _) ->
    TypeChar { pos }
  | (TypeString _, TypeString _) ->
    TypeString { pos }
  | (TypeVar param, TypeVar other) when param = other ->
    TypeVar param
  | (TypeInter other, _) ->
    let inter = meet other.left other.right in
    meet inter right
  | (_, TypeInter other) ->
    let inter = meet other.left other.right in
    meet left inter
  | (_, _) ->
    TypeInter { pos; left; right }
