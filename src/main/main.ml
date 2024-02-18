open Config

let read_file file_name =
  let input =
  try
    open_in file_name
  with _ ->
    Failure.raise ("Cannot open file `" ^ file_name ^ "`. (does the file exist ?)")
  in
  try
    let text = really_input_string input (in_channel_length input) in
    close_in input;
    text
  with _ ->
    close_in input;
    Failure.raise ("Cannot read file `" ^ file_name ^ "`.")

let parse code config =
  let ast = Parser.parse code in
  if config.show_ast then
    print_endline(Ast.display_program ast);
  ast

let desugar ast =
  Sugar.desugar ast Prim.binds

let type_check abt config =
  let types = Clam.Lib.check abt Prim.types in
  if config.show_types then
    List.iter (fun (def, type') ->
      print_endline((def: Abt.bind_expr).name ^ ": " ^ Clam.TypeDisplay.display type')
    ) types;
  ()

let eval abt =
  let main = (match List.find_opt (fun def -> def.Abt.bind.name = "main") abt.Abt.exprs with
  | Some main -> main
  | None -> Error.handle_main ()
  ) in
  Eval.eval main abt.exprs Prim.values print_endline

let interpret config file_name =
  let file_text = read_file file_name in
  let code = { Code.name = file_name; text = file_text } in
  try
    let ast = parse code config in
    let abt = desugar ast in
    type_check abt config;
    eval abt
  with
  | Parser.Error error ->
    Error.handle_parser error
  | Sugar.Error error ->
    Error.handle_sugar error
  | Clam.Error.Error error ->
    Error.handle_type error
  | Eval.Error error ->
    Error.handle_eval error

let () =
  try
    let config = parse_args (Sys.argv |> Array.to_list |> List.tl) in
    if config.help then
      print_endline Help.help
    else match config.file with
    | Some file_name ->
      interpret config file_name
    | None ->
      Failure.raise "Missing filename argument."
  with Failure.Error message ->
    Failure.exit message
