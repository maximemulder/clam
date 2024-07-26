open Build
open Core

let constrain sub sup =
  print_endline (constrain sub sup |> string_of_bool)

let check term type' =
  print_endline (check term type' |> string_of_bool)

let () =
  constrain top top;
  constrain bot top;
  constrain top bot;
  constrain (var "A") (var "A");
