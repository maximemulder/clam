open Vars

let test name type' (_: unit) =
  let result = Clam.TypingDisplay.display type' in
  Alcotest.(check string) name name result

let case name type' =
  let test = test name type' in
  Alcotest.test_case name `Quick test

let tests = [
  case "Top" top;
  case "Bot" bot;
  case "Unit" unit;
  case "Bool" bool;
  case "Int" int;
  case "Char" char;
  case "String" string;
  case "A" a;
  case "A & B" (inter a b);
  case "A | B" (union a b);
  case "A & (B | C)" (inter a (union b c))
]
