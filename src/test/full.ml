let directory = "../../../../tests/"

let rec list_files path =
  let full_path = directory ^ path in
  if Sys.is_directory full_path then
    Sys.readdir full_path
    |> Array.to_list
    |> List.map (Filename.concat path)
    |> List.map (fun path -> list_files path)
    |> List.flatten
  else if Filename.extension full_path = ".clam" then
    [path]
  else
    []

let read_file file_name =
  let input = open_in (directory ^ file_name) in
  let text = really_input_string input (in_channel_length input) in
  close_in input;
  text

let read_file_opt file_name =
  if Sys.file_exists (directory ^ file_name) then
    read_file file_name
  else
    ""

type buffer = {
  mutable text: string;
}

let make_buffer () =
  { text = "" }

let write_buffer buffer message =
  buffer.text <- buffer.text ^ message ^ "\n"

type channel = {
  name: string;
  expect: string;
  result: buffer;
}

let make_channel file_name name ext =
  {
    name;
    expect = read_file_opt (file_name ^ ext);
    result = make_buffer ();
  }

let make_channel_opt file_name name ext =
  if Sys.file_exists (directory ^ file_name ^ ext) then
    Some (make_channel file_name name ext)
  else
    None

let compare_channel file_name channel =
  if channel.result.text <> channel.expect then (
    print_endline("TEST ERROR (" ^ file_name ^ ", " ^ channel.name ^ "):");
    print_endline("EXPECTED: \n" ^ channel.expect);
    print_endline("FOUND: \n" ^ channel.result.text);
    false)
  else
    true

let compare_channel_opt file_name channel =
  match channel with
  | Some channel ->
    compare_channel file_name channel
  | None ->
    true

let test file_name =
  let file_text = read_file file_name in
  let code = { Code.name = file_name; text = file_text } in
  let channel_ast    = make_channel_opt file_name "ast"             ".ast"    in
  let channel_kinds  = make_channel_opt file_name "kinds"           ".kinds"  in
  let channel_types  = make_channel_opt file_name "types"           ".types"  in
  let channel_values = make_channel_opt file_name "values"          ".values" in
  let channel_out    = make_channel     file_name "standard output" ".out"    in
  let channel_err    = make_channel     file_name "standard error"  ".err"    in
  (try
    Main.run code {
      show_ast    = Option.map (fun channel -> write_buffer channel.result) channel_ast;
      show_kinds  = Option.map (fun channel -> write_buffer channel.result) channel_kinds;
      show_types  = Option.map (fun channel -> write_buffer channel.result) channel_types;
      show_values = Option.map (fun channel -> write_buffer channel.result) channel_values;
      print_out   = write_buffer channel_out.result;
      print_err   = write_buffer channel_err.result;
    }
  with
  Failure failure ->
    print_endline ("INTERPRETER ERROR: " ^ failure);
    false
  ) &&
  compare_channel_opt file_name channel_ast    &&
  compare_channel_opt file_name channel_types  &&
  compare_channel_opt file_name channel_values &&
  compare_channel     file_name channel_out    &&
  compare_channel     file_name channel_err

let file_to_test file_name =
  let name = "full `" ^ file_name ^ "`" in
  let test = fun (_: unit) -> Alcotest.(check bool) name true (test file_name) in
  Alcotest.test_case name `Quick test

let tests = List.map file_to_test (list_files "")
