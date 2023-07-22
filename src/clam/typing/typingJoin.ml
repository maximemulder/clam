open Model

let rec join left right =
  let pos = type_pos left in
  match (left, right) with
  | (TypeTop _, _) ->
    TypeTop {
      type_top_pos = pos;
    }
  | (_, TypeTop _) ->
    TypeTop {
      type_top_pos = pos;
    }
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
  | (TypeUnion other, _) ->
    let union = join other.type_union_left other.type_union_right in
    join union right
  | (_, TypeUnion other) ->
    let union = join other.type_union_left other.type_union_right in
    join left union
  | (_, _) ->
    TypeUnion {
      type_union_pos = pos;
      type_union_left = left;
      type_union_right = right;
    }
