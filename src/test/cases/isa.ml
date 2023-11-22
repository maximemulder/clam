open Clam
open Vars

let test name sub sup (_: unit) =
  let result = Typing.isa sub sup TypingContext.empty in
  Alcotest.(check bool) name true result

let name sub sup =
  let sub = TypingDisplay.display sub in
  let sup = TypingDisplay.display sup in
  "isa `" ^ sub ^ "` `" ^ sup ^ "`"

let case sub sup =
  let name = name sub sup in
  let test = test name sub sup in
  Alcotest.test_case name `Quick test

let tests = [
  (* bottom *)
  case bot top;
  case bot bot;
  case bot unit;

  case (var "A" bot) top;
  case (var "A" bot) bot;
  case (var "A" bot) unit;
  case (var "A" bot) (var "B" bot);
  case (var "A" (var "B" bot)) bot;

  (* variables *)
  case a a;
  case ea a;
  case (var "A" top) top;
  case (var "A" unit) top;
  case (var "A" unit) unit;

  (* unions *)
  case a (union a b);
  case a (union b a);
  case (union a a) (union a a);
  case (union a b) (union a b);
  case (union a b) (union b a);

  (* intersections *)
  case (inter a b) a;
  case (inter b a) b;
  case (inter a a) (inter a a);
  case (inter a b) (inter a b);
  case (inter a b) (inter b a);

  (* distributivity *)
  case (union (inter a b) (inter a c)) (inter a (union b c));
  case (inter a (union b c)) (union (inter a b) (inter a c));

  (* meets *)
  case (inter (abs_expr a b) (abs_expr a c)) (abs_expr a (inter b c));
  case (abs_expr (inter a b) c) (inter (abs_expr a c) (abs_expr b c));
  case (inter (abs_expr a b) (abs_expr a c)) (abs_expr a (inter b c));
  case (abs_expr a (inter b c)) (inter (abs_expr a b) (abs_expr a c));
  case (inter (abs_expr a c) (abs_expr b d)) (abs_expr (inter a b) (inter c d));
  case (abs_expr (inter a b) (inter c d)) (inter (abs_expr a c) (abs_expr b d));

  (* type to expression abstractions *)
  case (abs_expr_type ("A", top) (inline a)) (abs_expr_type ("A", top) (inline a));
  case (abs_expr_type ("A", top) (fun a -> a)) (abs_expr_type ("A", top) (fun a -> a));
  case (abs_expr_type ("A", top) (fun a -> a)) (abs_expr_type ("B", top) (fun b -> b));
]
