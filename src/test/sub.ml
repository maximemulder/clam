open Vars

let test_sub name sub sup (_: unit) =
  let result = Clam.TypingSub.is_subtype sub sup Clam.TypingContext.context_empty in
  Alcotest.(check bool) name true result

let case_sub name sub sup =
  let test = test_sub name sub sup in
  Alcotest.test_case name `Quick test

let tests = [
  case_sub "A : A"
    a a;
  case_sub "A : A | B"
    a (union a b);
  case_sub "A | B : A | B"
    (union a b) (union a b);
  case_sub "A & B : A & B"
    (inter a b) (union b a);
  case_sub "(A & B) | (A & C) : A & (B | C)"
    (union (inter a b) (inter a c)) (inter a (union b c));
  case_sub "A & (B | C) : (A & B) | (A & C)"
    (inter a (union b c)) (union (inter a b) (inter a c));
]
