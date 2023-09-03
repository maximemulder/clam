open Vars

let test name sub sup (_: unit) =
  let result = Clam.TypingSub.isa sub sup Clam.TypingContext.context_empty in
  Alcotest.(check bool) name true result

let case sub sup =
  let name_sub = Clam.TypingDisplay.display sub in
  let name_sup = Clam.TypingDisplay.display sup in
  let name = "isa `" ^ name_sub ^ "` `" ^ name_sup ^ "`" in
  let test = test name sub sup in
  Alcotest.test_case name `Quick test

let tests = [
  case a a;
  case a (union a b);
  case a (union b a);
  case (inter a b) a;
  case (inter b a) b;
  case (union a b) (union a b);
  case (union a b) (union b a);
  case (inter a b) (inter a b);
  case (inter a b) (inter b a);
  case (union (inter a b) (inter a c)) (inter a (union b c));
  case (inter a (union b c)) (union (inter a b) (inter a c));
  case (inter (abs_expr [a] b) (abs_expr [a] c)) (abs_expr [a] (inter b c));
  case (abs_expr [(inter a b)] c) (inter (abs_expr [a] c) (abs_expr [b] c));
  case (inter (abs_expr [a] b) (abs_expr [a] c)) (abs_expr [a] (inter b c));
  case (abs_expr [a] (inter b c)) (inter (abs_expr [a] b) (abs_expr [a] c));
  case (inter (abs_expr [a] c) (abs_expr [b] d)) (abs_expr [(inter a b)] (inter c d));
  case (abs_expr [(inter a b)] (inter c d)) (inter (abs_expr [a] c) (abs_expr [b] d));
]
