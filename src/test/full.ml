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
  let input =
  try
    open_in (directory ^ file_name)
  with _ ->
    Clam.Error.raise_file_open file_name
  in
  try
    let text = really_input_string input (in_channel_length input) in
    close_in input;
    text
  with _ ->
    close_in input;
    Clam.Error.raise_file_read file_name

type buffer = {
  mutable string: string;
}

let make_buffer () =
  { string = "" }

let write_buffer buffer message =
  buffer.string <- buffer.string ^ message ^ "\n"

let test file_name =
  let file_text = read_file file_name in
  let out_buffer = make_buffer () in
  let err_result = try
    let open Clam.Lib in
    let ast = parse file_name file_text in
    let abt = modelize ast in
    let _ = type' abt in
    let () = eval abt (write_buffer out_buffer) in
    ""
  with Clam.Error.Error message ->
    message
  in
  let out_result = out_buffer.string in
  let out_expect = read_file (file_name ^ ".out") in
  if out_result <> out_expect then (
    print_endline "TEST ERROR:";
    print_endline ("Expected output \"" ^ (String.escaped out_expect) ^ "\"");
    print_endline ("Found output:   \"" ^ (String.escaped out_result) ^ "\"");
    false
  )
  else if String.length err_result != 0 then
    false
  else
    true

let file_to_test file_name =
  let name = "full `" ^ file_name ^ "`" in
  let test = fun (_: unit) -> Alcotest.(check bool) name true (test file_name) in
  Alcotest.test_case name `Quick test

let tests = List.map file_to_test (list_files "")
