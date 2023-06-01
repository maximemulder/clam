open TypingDisplay
open Model

let raise message pos =
  Error.raise "TYPE ERROR" (message ^ "\n" ^ Error.display_pos pos)

let raise_expr_recursive def =
  raise ("recursive definition `" ^ def.def_expr_name ^ "`, type annotation needed") def.def_expr_pos

let raise_expr_constraint expr type' constraint' =
  let pos = expr_pos expr in
  let type' = display type' in
  let constraint' = display constraint' in
  raise ("expected expression of type `" ^ constraint' ^ "` but found expression of type `" ^ type' ^ "`") pos

let raise_expr_elem elem type' =
  let pos = elem.expr_elem_pos in
  let type' = display type' in
  let index = string_of_int elem.expr_elem_index in
  raise ("expected tuple expression with element `" ^ index ^"` but found expression of type `" ^ type' ^ "`") pos

let raise_expr_attr attr type' =
  let pos = attr.expr_attr_pos in
  let name = attr.expr_attr_name in
  let type' = display type' in
  raise ("expected record expression with attribute `" ^ name ^ "` but found expression of type `" ^ type' ^ "`") pos

let raise_expr_app_kind app type' =
  let pos = app.expr_app_pos in
  let type' = display type' in
  raise ("expected expression abstraction but found expression of type `" ^ type' ^ "`") pos

let raise_expr_app_arity app params =
  let length_params = string_of_int (List.length params) in
  let length_args = string_of_int (List.length app.expr_app_args) in
  let pos = app.expr_app_pos in
  raise ("expected " ^ length_params ^ " arguments but found " ^ length_args ^ " arguments") pos

let raise_expr_type_app_kind app type' =
  let pos = app.expr_type_app_pos in
  let type' = display type' in
  raise ("expected type abstraction but found type `" ^ type' ^ "`") pos

let raise_expr_type_app_arity app params =
  let length_params = string_of_int (List.length params) in
  let length_args = string_of_int (List.length app.expr_type_app_args) in
  let pos = app.expr_type_app_pos in
  raise ("expected " ^ length_params ^ " arguments but found " ^ length_args ^ " arguments") pos

let raise_param param =
  raise ("require type annotation for parameter `" ^ param.param_expr_name ^ "`") param.param_expr_pos

let raise_type_constraint type' constraint' =
  let pos = type_pos type' in
  let type' = display type' in
  let constraint' = display constraint' in
  raise ("expected type `" ^ type' ^ "` but found type `" ^ constraint' ^ "`") pos

let raise_type_app_kind type' =
  let pos = type_pos type' in
  let type' = display type' in
  raise ("expected type abstraction but found type `" ^ type' ^ "`") pos

let raise_type_app_arity app args =
  let pos = app.type_abs_pos in
  let length_params = string_of_int (List.length app.type_abs_params) in
  let length_args = string_of_int (List.length args) in
  raise ("expected " ^ length_params ^ " arguments but found " ^ length_args ^ " arguments") pos

let raise_type_proper type' =
  let pos = type_pos type' in
  let type' = display type' in
  raise ("expected proper type but found type `" ^ type' ^ "`") pos

let raise_unexpected _ =
  Error.raise "TYPE ERROR" "unexpected error"
