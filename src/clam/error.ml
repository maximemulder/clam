open Lexing
open Ast

exception Error of string

let raise error message =
  raise (Error (error ^ ": " ^ message))

let display_pos pos =
  let filename = pos.pos_fname in
  let line = string_of_int pos.pos_lnum in
  let column = string_of_int (pos.pos_cnum - pos.pos_bol) in
  "in file `" ^ filename ^ "` " ^ "line " ^ line ^ " " ^ "column " ^ column

let display_span (span: span) =
  let filename = span.code.name in
  let x = Code.get_span_start_x span in
  let y = Code.get_span_start_y span in
  let line = Code.get_span_line span in
  let padding = span.start - (Code.get_span_line_start span) in
  let line_end = Code.get_span_line_end span in
  "in file `" ^ filename ^ "` line " ^ string_of_int y ^ " column " ^ string_of_int x
  ^ "\n\n" ^ line ^ "\n" ^ (String.make padding ' ') ^ (String.make (min (span.end' - span.start) line_end) '^')

let raise_error message =
  raise "ERROR" message

let raise_lexing lexbuf message =
  let pos = lexeme_start_p lexbuf in
  raise "LEXING ERROR" (message ^ "\n" ^ display_pos pos)

let raise_parsing lexbuf =
  let pos = lexeme_start_p lexbuf in
  raise "PARSING ERROR" ("incorrect syntax" ^ "\n" ^ display_pos pos)

let raise_main _ =
  raise_error ("missing main definition")
