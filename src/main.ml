open Ast
open Display

open Scope
open Model

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
  let _ = modelize program in
  print_endline (display_program program)
