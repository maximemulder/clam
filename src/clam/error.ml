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
  let line = string_of_int (Code.get_span_line span) in
  let column = string_of_int (Code.get_span_column span) in
  "in file `" ^ filename ^ "` line " ^ line ^ " column " ^ column

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
