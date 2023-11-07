open Clam.Error
open Clam.Lib

let read_file_name _ =
  if Array.length Sys.argv != 2
    then raise_file_name ()
  else
  Sys.argv.(1)

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

let () =
  let file_name = read_file_name () in
  let file_text = read_file file_name in
  try
    run file_name file_text print_endline
  with Error message ->
    print_endline message;
    exit(-1)
