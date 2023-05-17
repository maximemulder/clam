open Ast
open Display_ast

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
  let (types, all_types) = ModelizeTypes.modelize_program program in
  let (exprs, types) = ModelizeExprs.modelize_program program types all_types in
  let _ = TypingInfer.check_exprs exprs in
  let _ = TypingInfer.check_types types in
  let main = (match List.find_opt (fun expr -> expr.Model.def_expr_name  = "main") exprs with
  | Some main -> main
  | None -> let _ = print_endline "\nNo main definition" in exit (-1)) in
  print_endline ("\n" ^ RuntimeDisplay.display (RuntimeEval.eval_def main))
