open Model

let rec meet left right =
  let pos = type_pos left in
  match (left, right) with
  | (TypeTop _, right) ->
    right
  | (left, TypeTop _) ->
    left
  | (TypeUnit _, TypeUnit _) ->
    TypeUnit {
      type_unit_pos = pos;
    }
  | (TypeBool _, TypeBool _) ->
    TypeBool {
      type_bool_pos = pos;
    }
  | (TypeInt _, TypeInt _) ->
    TypeInt {
      type_int_pos = pos;
    }
  | (TypeChar _, TypeChar _) ->
    TypeChar {
      type_char_pos = pos;
    }
  | (TypeString _, TypeString _) ->
    TypeString {
      type_string_pos = pos;
    }
  | (TypeVar param, TypeVar other_param) when param = other_param ->
    TypeVar param
  | (TypeInter other, _) ->
    let inter = meet other.type_inter_left other.type_inter_right in
    meet inter right
  | (_, TypeInter other) ->
    let inter = meet other.type_inter_left other.type_inter_right in
    meet left inter
  | (_, _) ->
    TypeInter {
      type_inter_pos = pos;
      type_inter_left = left;
      type_inter_right = right;
    }
