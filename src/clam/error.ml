open Lexing

let raise error message =
  print_endline (error ^ ": " ^ message);
  exit(-1)

let display_pos pos =
  let filename = pos.pos_fname in
  let line = string_of_int pos.pos_lnum in
  let column = string_of_int (pos.pos_cnum - pos.pos_bol) in
  "in file `" ^ filename ^ "` " ^ "line " ^ line ^ " " ^ "column " ^ column

let raise_error message =
  raise "ERROR" message

let raise_file_name _ =
  raise_error ("missing filename argument")

let raise_file_open file_name =
  raise_error ("cannot open file `" ^ file_name ^ "` (does the file exist ?)")

let raise_file_read file_name =
  raise_error ("cannot read file `" ^ file_name ^ "`")

let raise_lexing lexbuf message =
  let pos = lexeme_start_p lexbuf in
  raise "LEXING ERROR" (message ^ "\n" ^ display_pos pos)

let raise_parsing lexbuf =
  let pos = lexeme_start_p lexbuf in
  raise "PARSING ERROR" ("incorrect syntax" ^ "\n" ^ display_pos pos)

let raise_main _ =
  raise_error ("missing main definition")
