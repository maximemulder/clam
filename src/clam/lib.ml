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

let run code_name code_text writer =
  let program = parse code_name code_text in
  (* TODO: Refactor this *)
  let (types, all_types) = ModelizeTypes.modelize_program program in
  let (exprs, types) = ModelizeExprs.modelize_program program types all_types in
  let _ = TypeInfer.check_types types in
  let _ = TypeInfer.check_defs exprs Primitive.types in
  let main = (match List.find_opt (fun def -> def.Abt.bind.name = "main") exprs with
  | Some main -> main
  | None -> Error.raise_main ()
  ) in
  let _ = RuntimeEval.eval_def main exprs writer in ()
