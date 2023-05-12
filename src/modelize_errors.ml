let raise message =
  let _ = print_endline ("\nMODEL ERROR: " ^ message) in
  exit (-1)
