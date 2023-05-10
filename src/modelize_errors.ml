let raise message =
  let _ = print_endline ("\n" ^ message) in
  exit (-1)
