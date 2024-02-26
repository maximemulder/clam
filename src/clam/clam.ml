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

let read_code file_name =
  match file_name with
  | Some file_name ->
    let file_text = read_file file_name in
    { Code.name = file_name; text = file_text }
  | None ->
    Failure.raise "Missing filename argument."

let () =
  try
    let config = parse_args (Sys.argv |> Array.to_list |> List.tl) in
    if config.help then
      print_endline Help.help
    else
    let code = read_code config.file in
    Main.run code {
      show_ast    = Util.bool_then config.show_ast   print_endline;
      show_types  = Util.bool_then config.show_types print_endline;
      show_values = None;
      print_out   = print_endline;
      print_err   = print_endline;
    }
  with Failure.Error message ->
    Failure.exit message
