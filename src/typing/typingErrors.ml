open TypingDisplay
open Model

let raise message =
  let _ = print_endline ("\nTYPE ERROR: " ^ message) in
  exit (-1)

let raise_type type' constraint' =
  raise ("expected type `" ^ (display constraint') ^ "` but found type `" ^ (display type') ^ "`")

let raise_type_app_arity length_params length_args =
  raise ("expected " ^ (string_of_int length_params) ^ " arguments but found " ^ (string_of_int length_args) ^ " arguments")

let raise_type_app_kind type' =
  raise ("expected type abstraction but found type `" ^ (display type') ^ "`")

let raise_param param =
  raise ("require type annotation for parameter `" ^ param.param_expr_name ^ "`")

let raise_expr_app_kind type' =
  raise ("expected expression abstraction but found expression of type `" ^ (display type') ^ "`")

let raise_expr_app_arity length_params length_args =
  raise ("expected " ^ (string_of_int length_params) ^ " arguments but found " ^ (string_of_int length_args) ^ " arguments")

let raise_recursive def =
  raise ("recursive definition `" ^ def.Model.def_expr_name ^ "`")
