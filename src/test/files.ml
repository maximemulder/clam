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
