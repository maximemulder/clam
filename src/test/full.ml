type buffer = {
  mutable string: string;
}

let make_buffer () =
  { string = "" }

let write_buffer buffer message =
  buffer.string <- buffer.string ^ message ^ "\n"

let read file_name =
  let input =
  try
    open_in file_name
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

let rec list path extension =
  if Sys.is_directory path then
    Sys.readdir path
    |> Array.to_list
    |> List.map (Filename.concat path)
    |> List.map (fun path -> list path extension)
    |> List.flatten
  else if Filename.extension path = "." ^ extension then
    [path]
  else
    []

let test_directory = "../../../../tests/"

let test file_name =
  let file_text = read file_name in
  let output_buffer = make_buffer () in
  Clam.Lib.run file_name file_text (write_buffer output_buffer);
  let output = output_buffer.string in
  let expected_output = read (file_name ^ ".out") in
  if output <> expected_output then (
    print_endline "TEST ERROR:";
    print_endline ("Expected output \"" ^ (String.escaped expected_output) ^ "\"");
    print_endline ("Found output:   \"" ^ (String.escaped output) ^ "\"");
    false
  )
  else
    true


let map_file name =
  let test = fun (_: unit) -> Alcotest.(check bool) name true (test name) in
  Alcotest.test_case name `Quick test

let tests = List.map map_file (list test_directory "clam")
