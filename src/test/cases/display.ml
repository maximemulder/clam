open Vars2

let test type' (_: unit) =
  Clam.TypeDisplay.display type'

let case name type' =
  Case.make_case2 Case.string name (test type') name

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
  case "A & B" (inter [a; b]);
  case "A | B" (union [a; b]);
  case "(A & B) | C" (union [inter [a; b]; c]);
  case "A | (B & C)" (union [a; inter [b; c]]);
  case "A & B & C" (inter [a; b; c]);
  case "A | B | C" (union [a; b; c]);
  case "(A & B) | (C & D)" (union [inter [a; b]; inter [c; d]]);
]
