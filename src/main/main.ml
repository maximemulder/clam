open Clam
open Clam.Lib

open Config
open Error

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

let interpret config file_name =
  let file_text = read_file file_name in
  try
    let ast = parse file_name file_text in
    if config.show_ast then
      print_endline(Ast.display_program ast);
    let abt = modelize ast in
    let def_types = type' abt in
    if config.show_types then
      List.iter (fun (def, type') ->
        print_endline((def: Abt.bind_expr).name ^ ": " ^ TypeDisplay.display type')
      ) def_types;
    eval abt print_endline
  with Error message ->
    print_endline message;
    exit(-1)

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
