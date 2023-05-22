open TypingDisplay
open Model

let raise message pos =
  Error.raise "TYPE ERROR" (message ^ "\n" ^ Error.display_pos pos)


let raise_expr_recursive def =
  raise ("recursive definition `" ^ def.def_expr_name ^ "`, type annotation needed") def.def_expr_pos

let raise_expr_constraint expr type' constraint' =
  let type' = display type' in
  let constraint' = display constraint' in
  raise ("expected expression of type `" ^ constraint' ^ "` but found expression of type `" ^ type' ^ "`") (fst expr)

let raise_expr_tuple_kind expr type' =
  let type' = display type' in
  raise ("expected tuple expression but found expression of type `" ^ type' ^ "`") (fst expr)

let raise_expr_tuple_index expr type' index =
  let index = string_of_int index in
  let type' = display type' in
  raise ("index " ^ index ^ " is too high for expression of type `" ^ type' ^ "`") (fst expr)

let raise_expr_record_kind expr type' =
  let type' = display type' in
  raise ("expected record expression but found expression of type `" ^ type' ^ "`") (fst expr)

let raise_expr_record_attr expr type' attr =
  let type' = display type' in
  raise ("attribute `" ^ attr ^ "` is not in expression of type `" ^ type' ^ "`") (fst expr)

let raise_expr_app_kind expr type' =
  let type' = display type' in
  raise ("expected expression abstraction but found expression of type `" ^ type' ^ "`") (fst expr)

let raise_expr_app_arity expr params args =
  let length_params = string_of_int (List.length params) in
  let length_args = string_of_int (List.length args) in
  raise ("expected " ^ length_params ^ " arguments but found " ^ length_args ^ " arguments") (fst expr)

let raise_expr_type_app_kind expr type' =
  let type' = display type' in
  raise ("expected type abstraction but found type `" ^ type' ^ "`") (fst expr)

let raise_expr_type_app_arity expr params args =
  let length_params = string_of_int (List.length params) in
  let length_args = string_of_int (List.length args) in
  raise ("expected " ^ length_params ^ " arguments but found " ^ length_args ^ " arguments") (fst expr)

let raise_param param =
  raise ("require type annotation for parameter `" ^ param.param_expr_name ^ "`") param.param_expr_pos

let raise_type_constraint type' constraint' =
  let pos = fst type' in
  let type' = display type' in
  let constraint' = display constraint' in
  raise ("expected type `" ^ type' ^ "` but found type `" ^ constraint' ^ "`") pos

let raise_type_app_kind type' =
  let pos = fst type' in
  let type' = display type' in
  raise ("expected type abstraction but found type `" ^ type' ^ "`") pos

let raise_type_app_arity type' params args =
  let length_params = string_of_int (List.length params) in
  let length_args = string_of_int (List.length args) in
  raise ("expected " ^ length_params ^ " arguments but found " ^ length_args ^ " arguments") (fst type')

let raise_unexpected _ =
  Error.raise "TYPE ERROR" "unexpected error"
