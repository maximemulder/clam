open Vars

let test name type' (_: unit) =
  let result = Clam.TypingDisplay.display type' in
  Alcotest.(check string) name name result

let case name type' =
  let test = test name type' in
  Alcotest.test_case name `Quick test

let tests = [
  (* atomics *)
  case "Top" top;
  case "Bot" bot;
  case "Unit" unit;
  case "Bool" bool;
  case "Int" int;
  case "Char" char;
  case "String" string;
  case "A" a;

  (* products *)
  case "()" (tuple []);
  case "(A)" (tuple [a]);
  case "(A, B)" (tuple [a; b]);
  case "{}" (record []);
  case "{a: A}" (record [("a", a)]);
  case "{a: A, b: B}" (record [("a", a); ("b", b)]);

  (* unions and intersections *)
  case "A & B" (inter a b);
  case "A | B" (union a b);
  case "(A | B) & C" (inter (union a b) c);
  case "(A & B) | C" (union (inter a b) c);
  case "A & (B | C)" (inter a (union b c));
  case "A | (B & C)" (union a (inter b c));
  case "(A & B) & (C & D)" (inter (inter a b) (inter c d));
  case "(A | B) & (C | D)" (inter (union a b) (union c d));
  case "(A & B) | (C & D)" (union (inter a b) (inter c d));
  case "(A | B) | (C | D)" (union (union a b) (union c d));
]
