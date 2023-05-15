open Ast
open Display_ast
open Display_type

let parse (s : string) : program =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.program Lexer.read lexbuf in
  ast

let read_file name =
  let channel = open_in_bin name in
  let string = really_input_string channel (in_channel_length channel) in
  close_in channel;
  string

let () =
  let program = parse (read_file Sys.argv.(1)) in
  let (types, all_types) = Modelize_types.modelize_program program in
  let (exprs, types) = Modelize_exprs.modelize_program program types all_types in
  let _ = Typing.check_exprs exprs in
  Typing.check_types types
  (*print_endline (display_program program)*)
