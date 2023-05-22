let raise message =
  print_endline ("RUNTIME ERROR: " ^ message);
  exit (-1)

let raise_operator op =
  raise ("unexpected operator `" ^ op ^ "`")

let raise_value _ =
  raise ("unexpected value type")
