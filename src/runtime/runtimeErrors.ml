let raise message =
  let _ = print_endline ("\nRUNTIME ERROR: " ^ message) in
  exit (-1)

let raise_operator op =
  raise ("Unexpected operator `" ^ op ^ "`")

let raise_value _ =
  raise ("Unexpected value type")
