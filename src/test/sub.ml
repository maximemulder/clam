open Vars

let test_sub name sub sup (_: unit) =
  let result = Clam.TypingSub.isa sub sup Clam.TypingContext.context_empty in
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
    (inter a b) (inter b a);
  case_sub "(A & B) | (A & C) : A & (B | C)"
    (union (inter a b) (inter a c)) (inter a (union b c));
  case_sub "A & (B | C) : (A & B) | (A & C)"
    (inter a (union b c)) (union (inter a b) (inter a c));
  case_sub "((A) -> B) | ((A) -> C) : (A) -> (B | C)"
    (union (abs_expr [a] b) (abs_expr [a] c)) (abs_expr [a] (union b c));
  case_sub "(A) -> (B | C) : ((A) -> B) | ((A) -> C)"
    (abs_expr [a] (union b c)) (union (abs_expr [a] b) (abs_expr [a] c));
  case_sub "((A) -> C) | ((B) -> C) : (A | B) -> C"
    (union (abs_expr [a] b) (abs_expr [a] c)) (abs_expr [a] (union b c));
  case_sub "(A | B) -> C : ((A) -> C) | ((B) -> C)"
    (abs_expr [(union a b)] c) (union (abs_expr [a] c) (abs_expr [b] c));
  case_sub "((A) -> C) | ((B) -> D) : (A | B) -> (C | D)"
    (union (abs_expr [a] c) (abs_expr [b] d)) (abs_expr [(union a b)] (union c d));
]
