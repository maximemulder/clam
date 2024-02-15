exception Error of string

let raise_lexer code lexbuf =
  let pos = (Lexing.lexeme_start_p lexbuf).pos_cnum in
  let file = code.Code.name in
  let x = string_of_int (Code.get_x code pos) in
  let y = string_of_int (Code.get_y code pos) in
  let error = Error ("incorrect token in file `" ^ file ^ "` line " ^ y ^ " column " ^ x) in
  raise error

let raise_parser code lexbuf =
  let pos = (Lexing.lexeme_start_p lexbuf).pos_cnum in
  let file = code.Code.name in
  let x = string_of_int (Code.get_x code pos) in
  let y = string_of_int (Code.get_y code pos) in
  let error = Error ("incorrect syntax in file `" ^ file ^ "` line " ^ y ^ " column " ^ x) in
  raise error
