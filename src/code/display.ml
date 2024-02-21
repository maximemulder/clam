open Span

let display span =
  let filename = span.code.name in
  let x = get_span_start_x span in
  let y = get_span_start_y span in
  let line = get_span_line span in
  let line_end = get_span_line_end span in
  let padding = span.start - (get_span_line_start span) in
  let underline = min (span.end' - span.start) line_end in
  "in file `" ^ filename ^ "` line " ^ string_of_int y ^ " column " ^ string_of_int x ^"\n"
  ^ "| \n"
  ^ "| " ^ line ^ "\n"
  ^ "| " ^ (String.make padding ' ') ^ (String.make underline '^')
