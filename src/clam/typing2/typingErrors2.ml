open Model
open TypingDisplay

let raise message pos =
  Error.raise "TYPE ERROR" (message ^ "\n" ^ Error.display_pos pos)

let raise_validate_proper (type': Model.type') =
  let pos = type_pos type' in
  let type' = display type' in
  raise
    ("expected proper type but found type `" ^ type' ^ "`")
    pos

let raise_validate_inter_kind (inter: Model.type_inter) =
  let pos = inter.pos in
  let inter = display (TypeInter inter) in
  raise
    ("both operands of intersection `" ^ inter ^ "` must be of the same kind")
    pos

let raise_validate_union_kind (union: Model.type_union) =
  let pos = union.pos in
  let union = display (TypeUnion union) in
  raise
    ("both operands of union `" ^ union ^ "` must be of the same kind")
    pos
