open Model

let raise message pos =
  Error.raise "MODEL ERROR" (message ^ "\n" ^ (Error.display_pos pos))

let raise_expr_duplicate name =
  Error.raise "MODEL ERROR" ("duplicate expression definition `" ^ name ^ "`")

let raise_expr_bound expr name =
  let pos = fst expr in
  raise ("unbound expression `" ^ name ^ "`") pos

let raise_expr_operator expr op =
  let pos = fst expr in
  raise ("unknown operator `" ^ op ^ "`") pos

let raise_expr_integer expr value =
  let pos = fst expr in
  raise ("invalid integer literal `" ^ value ^ "`") pos

let raise_expr_product expr =
  raise "product expression cannot have both indexed and labeled fields" (fst  expr)

let raise_type_duplicate name =
  Error.raise "MODEL ERROR" ("duplicate type definition `" ^ name ^ "`")

let raise_type_bound type' name =
  raise ("unbound type `" ^ name ^ "`") (fst type')

let raise_type_recursive type' name =
  raise ("recursive type `" ^ name ^ "`") (fst type')

let raise_type_duplicate_attribute (attr: Model.attr_type) =
  raise ("duplicate attribute `" ^ attr.name ^ "`") attr.pos

let raise_type_product type' =
  raise "product type cannot have both indexed and labeled fields" (fst type')
