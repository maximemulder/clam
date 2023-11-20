open Vars

let test name left right expect (_: unit) =
  let result = Clam.Typing.meet left right in
  let result = Clam.TypingCompare.compare result expect in
  Alcotest.(check bool) name true result

let case left right expect =
  let name_left   = Clam.TypingDisplay.display_inline left   in
  let name_right  = Clam.TypingDisplay.display_inline right  in
  let name_expect = Clam.TypingDisplay.display_inline expect in
  let name = "meet " ^ name_left ^ " " ^ name_right ^ " " ^ name_expect in
  let test = test name left right expect in
  Alcotest.test_case name `Quick test

let tests = [
  (* top *)
  case top top top;
  case top a a;
  case a top a;

  (* bottom *)
  case bot bot bot;
  case bot a bot;
  case a bot bot;

  (* primitives *)
  case unit unit unit;
  case bool bool bool;
  case int int int;
  case char char char;
  case string string string;

  (* variables *)
  case a a a;
  case a b (inter a b);
  case a ea ea;
  case ea a ea;
  case ea fa (inter ea fa);

  (* tuples *)
  case (tuple []) (tuple []) (tuple []);
  case (tuple [a]) (tuple [a]) (tuple [a]);
  case (tuple [a]) (tuple [b]) (tuple [(inter a b)]);
  case (tuple [a]) (tuple [a; b]) bot;
  case (tuple [a; b]) (tuple [a]) bot;
  case (tuple [a; b]) (tuple [c; d]) (tuple [(inter a c); (inter b d)]);

  (* records *)
  case (record []) (record []) (record []);
  case (record [("foo", a)]) (record []) (record [("foo", a)]);
  case (record []) (record [("foo", a)]) (record [("foo", a)]);
  case (record [("foo", a)]) (record [("foo", b)]) (record [("foo", (inter a b))]);
  case (record [("foo", a)]) (record [("bar", b)]) (record [("foo", a); ("bar", b)]);

  (* intersections *)
  case a (inter b c) (inter a (inter b c));
  case (inter a b) c (inter a (inter b c));

  (* expression to expression abstractions *)
  case (abs_expr a b) (abs_expr a b) (abs_expr a b);
  case (abs_expr a c) (abs_expr b c) (abs_expr (inter a b) c);
  case (abs_expr a b) (abs_expr a c) (abs_expr a (inter b c));
  case (abs_expr a c) (abs_expr b d) (abs_expr (inter a b) (inter c d));

  (* type to expression abstractions *)
  case (abs_expr_type ("A", top) (inline c)) (abs_expr_type ("B", top) (inline d)) (abs_expr_type ("A", top) (inline ((inter c d))));
  case (abs_expr_type ("A", unit) (inline a)) (abs_expr_type ("B", top) (inline b)) bot;
  case (abs_expr_type ("A", top) (inline a)) (abs_expr_type ("B", unit) (inline b)) bot;

  case (abs_expr_type ("A", top) (fun a -> a)) (abs_expr_type ("A", top) (fun a -> a)) (abs_expr_type ("A", top) (fun a -> a));
  case (abs_expr_type ("A", top) (fun a -> a)) (abs_expr_type ("B", top) (fun b -> b)) (abs_expr_type ("A", top) (fun a -> a));
  case (abs_expr_type ("A", top) (fun a -> (tuple [a]))) (abs_expr_type ("B", top) (fun b -> (tuple [b]))) (abs_expr_type ("A", top) (fun a -> tuple[a]));

  (* TODO: Add these tests once duplicates are gone (either through sorting or exhaustive meet) *)
  (* case (abs_expr_type ("A", top) (fun a -> (inter int a))) (abs_expr_type ("B", top) (fun b -> b)) (abs_expr_type ("A", top) (fun a -> (inter int a)));
  case (abs_expr_type ("A", top) (fun a -> a)) (abs_expr_type ("B", top) (fun b -> (inter b int))) (abs_expr_type ("A", top) (fun a -> (inter int a))); *)
]
