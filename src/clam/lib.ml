type ast = Ast.program

type abt = {
  types: Abt.type' list;
  exprs: Abt.def_expr list;
}

type writer = RuntimeValue.writer

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

(**
  Modelize a program, building an abstract biding tree from an abstract syntax tree.
*)
let modelize ast =
  let (types, all_types) = ModelizeTypes.modelize_program ast in
  let (exprs, types) = ModelizeExprs.modelize_program ast types all_types in
  { types; exprs }

(**
  Type check a program.
*)
let type' abt =
  let _ = TypeInfer.check_types abt.types in
  TypeInfer.check_defs abt.exprs Primitive.types
  |> List.filter (fun def -> not(List.exists (fun primitive -> fst primitive == fst def) Primitive.types))

(**
  Evaluate a program.
*)
let eval abt writer =
  let main = (match List.find_opt (fun def -> def.Abt.bind.name = "main") abt.exprs with
  | Some main -> main
  | None -> Error.raise_main ()
  ) in
  let _ = RuntimeEval.eval_def main abt.exprs writer in ()
