open Files

let test_directory = "../../../../tests/"

let test file_name =
  let file_text = Files.read file_name in
  let output_buffer = Files.make_buffer () in
  Clam.Lib.run file_name file_text (Files.write_buffer output_buffer);
  let output = output_buffer.string in
  let expected_output = Files.read (file_name ^ ".out") in
  if output <> expected_output then (
    print_endline "TEST ERROR:";
    print_endline ("Expected output \"" ^ (String.escaped expected_output) ^ "\"");
    print_endline ("Found output:   \"" ^ (String.escaped output) ^ "\"");
    exit(-1)
    )
  else
    ()

let () =  Alcotest.run "clam" [
  "set", Sub.tests;
  "display", Display.tests;
]

let () =
  let tests = Files.list test_directory "clam" in
  List.iter test tests;
