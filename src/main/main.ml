open Clam
open Clam.Error
open Clam.Lib

let arg_file_name = ref None
let arg_types = ref false
let arg_values = ref false

let read_file_name () =
  match !arg_file_name with
  | Some file_name -> file_name
  | None -> raise_file_name ()

let read_file file_name =
  let input =
  try
    open_in file_name
  with _ ->
    raise_file_open file_name
  in
  try
    let text = really_input_string input (in_channel_length input) in
    close_in input;
    text
  with _ ->
    close_in input;
    raise_file_read file_name


let main () =
  let file_name = read_file_name () in
  let file_text = read_file file_name in
  try
    let ast = parse file_name file_text in
    let abt = modelize ast in
    let def_types = type' abt in
    if !arg_types then
      List.iter (fun (def, type') ->
        print_endline((def: Abt.bind_expr).name ^ ": " ^ TypeDisplay.display type')
      ) def_types;
    eval abt print_endline
  with Error message ->
    print_endline message;
    exit(-1)

let usage = "clam [-types] [-values] <file>"

let specs = [
  "-types",  Arg.Unit (fun () -> arg_types  := true), "Print definition types";
  "-values", Arg.Unit (fun () -> arg_values := true), "Print definition values";
]

let () =
  Arg.parse specs (fun value -> arg_file_name := Some value) usage;
  main ()
