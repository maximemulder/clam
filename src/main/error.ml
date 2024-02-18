let display_span (span: Code.span) =
  let filename = span.code.name in
  let x = Code.get_span_start_x span in
  let y = Code.get_span_start_y span in
  let line = Code.get_span_line span in
  let padding = span.start - (Code.get_span_line_start span) in
  let line_end = Code.get_span_line_end span in
  "in file `" ^ filename ^ "` line " ^ string_of_int y ^ " column " ^ string_of_int x
  ^ "\n\n" ^ line ^ "\n" ^ (String.make padding ' ') ^ (String.make (min (span.end' - span.start) line_end) '^')

let handle error message =
  print_endline(error ^ ": " ^ message);
  exit(-1)

let handle_parser message =
  handle "SYNTAX ERROR" message

let handle_sugar error =
  handle "SYNTAX ERROR" ( error.Sugar.Errors.message ^ display_span error.span)

let handle_type error =
  handle "TYPE ERROR" ( error.Clam.Error.message ^ display_span error.span)

let handle_eval error =
  handle "RUNTIME ERROR" error.Eval.Error.message

let handle_main () =
  handle "RUNTIME ERROR" "missing `main` definition"
