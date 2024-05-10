let handle error message print_err =
  print_err(error ^ ": " ^ message);
  false

let handle_parser message =
  handle "SYNTAX ERROR" message

let handle_sugar error =
  handle "SYNTAX ERROR" ( error.Sugar.Errors.message ^ "\n" ^ Code.display error.span)

let handle_type error =
  handle "TYPE ERROR" ( error.Type.Error.message ^ "\n" ^ Code.display error.span)

let handle_infer error =
  handle "TYPE ERROR" ( error.Infer.Error.message ^ "\n" ^ Code.display error.span)

let handle_eval error =
  handle "RUNTIME ERROR" error.Eval.Error.message

let handle_main () =
  handle "RUNTIME ERROR" "missing `main` definition"
