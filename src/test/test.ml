let test_directory = "../../../../tests/"

type buffer = {
  mutable string: string;
}

let make_buffer () =
  { string = "" }

let write_buffer buffer message =
  buffer.string <- buffer.string ^ message ^ "\n"

let read_file file_name =
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

let list_tests () =
  Sys.readdir test_directory
  |> Array.to_list
  |> List.filter (fun x -> Filename.extension x = ".clam")
  |> List.map (fun x -> test_directory ^ "/" ^ x)

let test file_name =
  let file_text = read_file file_name in
  let output_buffer = make_buffer () in
  Clam.Lib.run file_name file_text (write_buffer output_buffer);
  let output = output_buffer.string in
  let expected_output = read_file (file_name ^ ".out") in
  if output <> expected_output then (
    print_endline "TEST ERROR:";
    print_endline ("Expected output \"" ^ (String.escaped expected_output) ^ "\"");
    print_endline ("Found output:   \"" ^ (String.escaped output) ^ "\"");
    exit(-1)
    )
  else
    ()

let () =
  let tests = list_tests () in
  List.iter test tests;

(*

let pos = Clam.Model.prim_pos
let unit = Clam.Model.prim_unit
let bool = Clam.Model.prim_bool
let char = Clam.Model.prim_char
let test = fun t ->
  let a = Clam.TypingDisplay.display t in
  let b = Clam.TypingDisplay.display (Clam.TypingBool.normalize t) in
  print_endline(a ^ " ===> " ^ b)

let union left right =
  Clam.Model.TypeUnion { pos; left; right }

let inter left right =
  Clam.Model.TypeInter { pos; left; right }

;; test (union unit (inter bool char))
;; test (inter unit (union bool char))
;; test (union unit (inter bool char))
;; test (union (union unit bool) char)
;; test (inter (inter unit bool) char)
;; test (inter (union bool unit) (union bool unit))

*)