open Vars

let test name types expect (_: unit) =
  let result = Clam.Typing.join_many types in
  let result = Clam.TypingCompare.compare result expect in
  Alcotest.(check bool) name true result

let case types expect =
  let name_types = List.map Clam.TypingDisplay.display_inline types in
  let name_types = String.concat " v " name_types in
  let name_expect = Clam.TypingDisplay.display_inline expect in
  let name = "join " ^ name_types ^ " " ^ name_expect in
  let test = test name types expect in
  Alcotest.test_case name `Quick test

let case2 left right expect =
  case [left; right] expect

let tests = [
  (* top *)
  case2 top top top;
  case2 top a top;
  case2 a top top;

  (* bottom *)
  case2 bot bot bot;
  case2 bot a a;
  case2 a bot a;

  (* primitives *)
  case2 unit unit unit;
  case2 bool bool bool;
  case2 int int int;
  case2 char char char;
  case2 string string string;

  (* variables *)
  case2 a a a;
  case2 a b (union a b);
  case2 a ea a;
  case2 ea a a;
  case2 ea fa (union ea fa);

  (* unions *)
  case2 a (union b c) (union a (union b c));
  case2 (union a b) c (union (union a b) c);

  (* tuples *)
  case2 (tuple []) (tuple []) (tuple []);
  case2 (tuple [top]) (tuple [a]) (tuple [top]);
  case2 (tuple [a]) (tuple [top]) (tuple [top]);
  case2 (tuple [a]) (tuple [b]) (union (tuple [a]) (tuple [b]));
  case2 (tuple [a]) (tuple [a; b]) (union (tuple [a]) (tuple [a; b]));
  case2 (tuple [a; b]) (tuple [a]) (union (tuple [a; b]) (tuple [a]));
  case2 (tuple [a; b]) (tuple [c; d]) (union (tuple [a; b]) (tuple [c; d]));

  (* records *)
  case2 (record []) (record []) (record []);
  case2 (record [("foo", a)]) (record []) (record []);
  case2 (record []) (record [("foo", a)]) (record []);
  case2 (record [("foo", top)]) (record [("foo", a)]) (record [("foo", top)]);
  case2 (record [("foo", a)]) (record [("foo", top)]) (record [("foo", top)]);
  case2 (record [("foo", a)]) (record [("foo", b)]) (union (record [("foo", a)]) (record [("foo", b)]));
  case2 (record [("foo", a)]) (record [("bar", b)]) (union (record [("foo", a)]) (record [("bar", b)]));

  (* expression to expression abstractions *)
  case2 (abs_expr [a] b) (abs_expr [a] b) (abs_expr [a] b);
  case2 (abs_expr [top] b) (abs_expr [a] b) (abs_expr [a] b);
  case2 (abs_expr [a] b) (abs_expr [top] b) (abs_expr [a] b);
  case2 (abs_expr [a] top) (abs_expr [a] b) (abs_expr [a] top);
  case2 (abs_expr [a] b) (abs_expr [a] top) (abs_expr [a] top);
  case2 (abs_expr [a] c) (abs_expr [b] c) (union (abs_expr [a] c) (abs_expr [b] c));
  case2 (abs_expr [a] b) (abs_expr [a] c) (union (abs_expr [a] b) (abs_expr [a] c));
  case2 (abs_expr [a] b) (abs_expr [c] d) (union (abs_expr [a] b) (abs_expr [c] d));

  (* type to expression abstractions *)
  case2 (abs_expr_type_0 a) (abs_expr_type_0 a) (abs_expr_type_0 a);
  case2 (abs_expr_type_0 a) (abs_expr_type_0 top) (abs_expr_type_0 top);
  case2 (abs_expr_type_0 top) (abs_expr_type_0 a) (abs_expr_type_0 top);
  case2 (abs_expr_type_0 a) (abs_expr_type_0 b) (union (abs_expr_type_0 a) (abs_expr_type_0 b));

  (* others *)
  case [a] a;
  case [a; b] (union a b);
  case [a; b; c] (union a (union b c));
]
