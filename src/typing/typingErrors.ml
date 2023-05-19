open TypingDisplay
open Model

let raise message =
  Error.raise "TYPE ERROR" message

let raise2 message pos =
  Error.raise "TYPE ERROR" (message ^ "\n" ^ Error.display_pos pos)

let raise_expr_constraint expr type' constraint' =
  let type' = display type' in
  let constraint' = display constraint' in
  raise2 ("expected expression of type `" ^ constraint' ^ "` but found expression of type `" ^ type' ^ "`") (fst expr)

let raise_expr_app_kind expr type' =
  let type' = display type' in
  raise2 ("expected expression abstraction but found expression of type `" ^ type' ^ "`") (fst expr)

let raise_expr_app_arity expr params args =
  let length_params = string_of_int (List.length params) in
  let length_args = string_of_int (List.length args) in
  raise2 ("expected " ^ length_params ^ " arguments but found " ^ length_args ^ " arguments") (fst expr)

let raise_expr_type_app_kind expr type' =
  let type' = display type' in
  raise2 ("expected type abstraction but found type `" ^ type' ^ "`") (fst expr)

let raise_expr_type_app_arity expr params args =
  let length_params = string_of_int (List.length params) in
  let length_args = string_of_int (List.length args) in
  raise2 ("expected " ^ length_params ^ " arguments but found " ^ length_args ^ " arguments") (fst expr)

let raise_type type' constraint' =
  raise ("expected type `" ^ (display constraint') ^ "` but found type `" ^ (display type') ^ "`")

let raise_type_app_arity length_params length_args =
  raise ("expected " ^ (string_of_int length_params) ^ " arguments but found " ^ (string_of_int length_args) ^ " arguments")

let raise_type_app_kind type' =
  raise ("expected type abstraction but found type `" ^ (display type') ^ "`")

let raise_param param =
  raise ("require type annotation for parameter `" ^ param.param_expr_name ^ "`")

let raise_recursive def =
  raise ("recursive definition `" ^ def.Model.def_expr_name ^ "`")

let raise_unexpected _ =
  raise "unexpected error"
