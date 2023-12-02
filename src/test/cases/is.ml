open Clam
open Vars

let test name left right expect (_: unit) =
  let result = Typing.is left right in
  Alcotest.(check bool) name result expect

let name left right expect =
  let left  = TypingDisplay.display left in
  let right = TypingDisplay.display right in
  let suffix = if expect then "" else "!" in
  "is" ^ suffix ^ " `" ^ left ^ "` `" ^ right ^ "`"

let case_base left right expect =
  let name = name left right expect in
  let test = test name left right expect in
  Alcotest.test_case name `Quick test

let case left right = case_base left right true

let tests = [
  (* bottom *)
  case bot bot;
  case bot (var "A" bot);
  case (var "A" bot) bot;
  case (var "A" bot) (var "B" bot);
  case bot (var "A" (var "B" bot));
  case (var "A" (var "B" bot)) bot;

  (* primitives *)
  case top top;
  case unit unit;
  case bool bool;
  case int int;
  case char char;
  case string string;
  case a a;

  (* unions *)
  case a (union a a);
  case (union a a) a;
  case (union a a) (union a a);
  case (union a b) (union a b);
  case (union a b) (union b a);
  case (union a (union b c)) (union a (union b c));
  case (union a (union b c)) (union (union a b) c);
  case (union (union a b) c) (union a (union b c));
  case (union top a) top;
  case top (union top a);

  (* intersections *)
  case a (inter a a);
  case (inter a a) a;
  case (inter a a) (inter a a);
  case (inter a b) (inter a b);
  case (inter a b) (inter b a);
  case (inter top a) a;
  case a (inter top a);

  (* distributivity *)
  case (union (inter a b) (inter a c)) (inter a (union b c));
  case (inter a (union b c)) (union (inter a b) (inter a c));

  (* meets *)
  case (inter (abs_expr a c) (abs_expr b c)) (abs_expr (union a b) c);
  case (abs_expr (union a b) c) (inter (abs_expr a c) (abs_expr b c));
  case (inter (abs_expr a b) (abs_expr a c)) (abs_expr a (inter b c));
  case (abs_expr a (inter b c)) (inter (abs_expr a b) (abs_expr a c));
  case (inter (abs_expr a c) (abs_expr b d)) (abs_expr (union a b) (inter c d));
  case (abs_expr (union a b) (inter c d)) (inter (abs_expr a c) (abs_expr b d));

  (* type to expression abstractions *)
  case (abs_expr_type ("A", top) (inline a)) (abs_expr_type ("A", top) (inline a));
  case (abs_expr_type ("A", top) (fun a -> a)) (abs_expr_type ("A", top) (fun a -> a));
  case (abs_expr_type ("A", top) (fun a -> a)) (abs_expr_type ("B", top) (fun b -> b));

  (* type applications *)
  case top (app (abs "T" top id) top);
  case (app (abs "T" top id) top) top;
  case top (app (abs "T" top (inline top)) top);
  case (app (abs "T" top (inline top)) top) top;
]

let case left right = case_base left right false

let tests_not = [
  (* top and bottom types *)
  case top bot;
  case bot top;
  case unit top;
  case top unit;
  case int bot;
  case bot int;

  (* primitives *)
  case unit bool;
  case bool int;
  case int char;
  case char string;
  case string unit;

  (* variables *)
  case a b;
  case ea a;
  case a ea;
  case ea fa;

  (* unions *)
  case a (union a b);
  case (union a b) a;
  case (union top a) a;
  case a (union top a);

  (* interesections *)
  case a (inter a b);
  case (inter a b) a;
  case (inter top a) top;
  case top (inter top a);

  (* ambiguous names *)
  case (var "A" top) (var "A" top);
  case (abs_expr_type ("A", top) (fun a -> a)) (abs_expr_type ("A", top) (inline a));

  (* type abstractions and vars*)
  case (with_var "T" (abs "X" top id) (fun t -> (app t top))) (with_var "T" (abs "X" top id) (fun t -> (app t top)));
]
