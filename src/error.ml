let raise_error message =
  print_endline ("ERROR: " ^ message);
  exit(-1)

let raise_file_name _ =
  raise_error ("missing filename argument")

let raise_file_open file_name =
  raise_error ("cannot open file `" ^ file_name ^ "` (does the file exist ?)")

let raise_file_read file_name =
  raise_error ("cannot read file `" ^ file_name  ^ "`")

let raise_lexing lexbuf message =
  let pos = Lexing.lexeme_start_p lexbuf in
  print_endline ("LEXING ERROR: " ^ message ^ "\n"
    ^ "in file `" ^ pos.pos_fname ^ "` "
    ^ "line " ^ (string_of_int pos.pos_lnum) ^ " "
    ^ "column " ^ (string_of_int (pos.pos_cnum - pos.pos_bol))
  );
  exit(-1)

let raise_parsing lexbuf =
  let pos = Lexing.lexeme_start_p lexbuf in
  print_endline ("PARSING ERROR: incorrect program\n"
    ^ "in file `" ^ pos.pos_fname ^ "` "
    ^ "line " ^ (string_of_int pos.pos_lnum) ^ " "
    ^ "column " ^ (string_of_int (pos.pos_cnum - pos.pos_bol))
  );
  exit(-1)

let raise_main _ =
  raise_error ("missing main definition")
