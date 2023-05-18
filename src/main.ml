open Ast
open Display_ast
open Error

let read_file_name _ =
  if Array.length Sys.argv != 2
    then Error.raise_file_name ()
  else
  Sys.argv.(1)

let read_file file_name =
  let input =
  try
    open_in file_name
    with _ ->
    Error.raise_file_open file_name
  in
  try
    let text = really_input_string input (in_channel_length input) in
    close_in input;
    text
  with _ ->
    close_in input;
    Error.raise_file_read file_name

let parse file_name text =
  let lexbuf = Lexing.from_string text in
  Lexing.set_filename lexbuf file_name;
  try
    Parser.program Lexer.read lexbuf
  with
  | Lexer.Error message ->
    Error.raise_lexing lexbuf message
  | Parser.Error ->
    Error.raise_parsing lexbuf

let () =
  let file_name = read_file_name () in
  let text = read_file file_name in
  let program = parse file_name text in
  let (types, all_types) = ModelizeTypes.modelize_program program in
  let (exprs, types) = ModelizeExprs.modelize_program program types all_types in
  let _ = TypingInfer.check_exprs exprs in
  let _ = TypingInfer.check_types types in
  let main = (match List.find_opt (fun expr -> expr.Model.def_expr_name = "main") exprs with
  | Some main -> main
  | None -> Error.raise_main ()
  ) in
  print_endline ("\n" ^ RuntimeDisplay.display (RuntimeEval.eval_def main))
