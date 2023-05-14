open Display_type

let raise message =
  let _ = print_endline ("\nTYPE ERROR: " ^ message) in
  exit (-1)

let raise_type type' constraint' =
  raise ("expected type `" ^ (display constraint') ^ "` but found type `" ^ (display type') ^ "`")

let raise_app_arity length_params length_args =
  raise ("expected " ^ (string_of_int length_params) ^ " arguments but found " ^ (string_of_int length_args) ^ " arguments")

let raise_app_kind type' =
  raise ("expected abstraction type but found type `" ^ (display type') ^ "`")
