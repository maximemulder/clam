open Lexing

let raise message =
  print_endline ("MODEL ERROR: " ^ message);
  exit (-1)

let raise_expr_duplicate name =
  raise ("duplicate expression definition `" ^ name ^ "`")

let raise_expr_bound expr name =
  let pos = fst expr in
  raise ("unbound expression `" ^ name ^ "`\n"
    ^ "in file `" ^ pos.pos_fname ^ "` "
    ^ "line " ^ (string_of_int pos.pos_lnum) ^ " "
    ^ "column " ^ (string_of_int (pos.pos_cnum - pos.pos_bol))
  )

let raise_expr_integer expr value =
  let pos = fst expr in
  raise ("invalid integer literal `" ^ value ^ "`\n"
    ^ "in file `" ^ pos.pos_fname ^ "` "
    ^ "line " ^ (string_of_int pos.pos_lnum) ^ " "
    ^ "column " ^ (string_of_int (pos.pos_cnum - pos.pos_bol))
  )
