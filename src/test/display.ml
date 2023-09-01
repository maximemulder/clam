open Vars

let test_display name type' (_: unit) =
  let result = Clam.TypingDisplay.display type' in
  Alcotest.(check string) name name result

let case_display name type' =
  let test = test_display name type' in
  Alcotest.test_case name `Quick test

let tests = [
  case_display "Top"
    prim_top;
  case_display "Bot"
    prim_bot;
  case_display "Unit"
    prim_unit;
  case_display "Bool"
    prim_bool;
  case_display "Int"
    prim_int;
  case_display "Char"
    prim_char;
  case_display "String"
    prim_string;
  case_display "A"
    a;
  case_display "A & B"
    (inter a b);
  case_display "A | B"
    (union a b);
  case_display "A & (B | C)"
    (inter a (union b c))
]
