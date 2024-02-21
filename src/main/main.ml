let parse code show_ast =
  let ast = Parser.parse code in
  if show_ast then
    print_endline(Ast.display_program ast);
  ast

let desugar ast =
  Sugar.desugar ast Prim.binds

let type_check abt show_types =
  let types = Infer.check abt Prim.types in
  if show_types then
    List.iter (fun (def, type') ->
      print_endline((def: Abt.bind_expr).name ^ ": " ^ Type.display type')
    ) types;
  ()

let eval abt print_out print_err =
  let main = (match List.find_opt (fun def -> def.Abt.bind.name = "main") abt.Abt.exprs with
  | Some main -> main
  | None -> Error.handle_main () print_err
  ) in
  Eval.eval main abt.exprs Prim.values print_out

let run code show_ast show_types _show_values print_out print_err =
  try
    let ast = parse code show_ast in
    let abt = desugar ast in
    type_check abt show_types;
    eval abt print_out print_err
  with
  | Parser.Error error ->
    Error.handle_parser error print_err
  | Sugar.Error error ->
    Error.handle_sugar error print_err
  | Type.Error error ->
    Error.handle_type error print_err
  | Infer.Error error ->
    Error.handle_infer error print_err
  | Eval.Error error ->
    Error.handle_eval error print_err
