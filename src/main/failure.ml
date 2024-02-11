exception Error of string

let raise message =
  raise (Error message)

let exit message =
  Printf.eprintf "COMMAND ERROR: %s\n%s\n"
    message
    "Try 'clam --help' to display the list of options.";
  exit(-1)
