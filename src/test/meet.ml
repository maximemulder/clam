open Vars

let test name types expect (_: unit) =
  let result = Clam.Typing.meet_many types in
  let result = Clam.TypingCompare.compare result expect in
  Alcotest.(check bool) name true result

let case types expect =
  let name_types = List.map Clam.TypingDisplay.display_inline types in
  let name_types = String.concat " ^ " name_types in
  let name_expect = Clam.TypingDisplay.display_inline expect in
  let name = "meet " ^ name_types ^ " " ^ name_expect in
  let test = test name types expect in
  Alcotest.test_case name `Quick test

let case2 left right expect =
  case [left; right] expect

let tests = [
  (* top *)
  case2 top top top;
  case2 top a a;
  case2 a top a;

  (* bottom *)
  case2 bot bot bot;
  case2 bot a bot;
  case2 a bot bot;

  (* primitives *)
  case2 unit unit unit;
  case2 bool bool bool;
  case2 int int int;
  case2 char char char;
  case2 string string string;

  (* variables *)
  case2 a a a;
  case2 a b (inter a b);
  case2 a ea ea;
  case2 ea a ea;
  case2 ea fa (inter ea fa);

  (* tuples *)
  case2 (tuple []) (tuple []) (tuple []);
  case2 (tuple [a]) (tuple [a]) (tuple [a]);
  case2 (tuple [a]) (tuple [b]) (tuple [(inter a b)]);
  case2 (tuple [a]) (tuple [a; b]) bot;
  case2 (tuple [a; b]) (tuple [a]) bot;
  case2 (tuple [a; b]) (tuple [c; d]) (tuple [(inter a c); (inter b d)]);

  (* records *)
  case2 (record []) (record []) (record []);
  case2 (record [("foo", a)]) (record []) (record [("foo", a)]);
  case2 (record []) (record [("foo", a)]) (record [("foo", a)]);
  case2 (record [("foo", a)]) (record [("foo", b)]) (record [("foo", (inter a b))]);
  case2 (record [("foo", a)]) (record [("bar", b)]) (record [("foo", a); ("bar", b)]);

  (* intersections *)
  case2 a (inter b c) (inter a (inter b c));
  case2 (inter a b) c (inter (inter a b) c);

  (* expression to expression abstractions *)
  case2 (abs_expr [a] b) (abs_expr [a] b) (abs_expr [a] b);
  case2 (abs_expr [a] c) (abs_expr [b] c) (abs_expr [(inter a b)] c);
  case2 (abs_expr [a] b) (abs_expr [a] c) (abs_expr [a] (inter b c));
  case2 (abs_expr [a] c) (abs_expr [b] d) (abs_expr [(inter a b)] (inter c d));

  (* type to expression abstractions *)
  case2 (abs_expr_type_0 a) (abs_expr_type_0 a) (abs_expr_type_0 a);
  case2 (abs_expr_type_0 a) (abs_expr_type_0 b) (abs_expr_type_0 (inter a b));
  case2 (abs_expr_type_1 ("A", top) (inline c)) (abs_expr_type_1 ("B", top) (inline d)) (abs_expr_type_1 ("A", top) (inline ((inter c d))));
  case2 (abs_expr_type_1 ("A", top) (inline a)) (abs_expr_type_0 b) bot;
  case2 (abs_expr_type_0 a) (abs_expr_type_1 ("B", top) (inline b)) bot;
  case2 (abs_expr_type_1 ("A", unit) (inline a)) (abs_expr_type_1 ("B", top) (inline b)) bot;
  case2 (abs_expr_type_1 ("A", top) (inline a)) (abs_expr_type_1 ("B", unit) (inline b)) bot;

  case2 (abs_expr_type_1 ("A", top) (fun a -> a)) (abs_expr_type_1 ("A", top) (fun a -> a)) (abs_expr_type_1 ("A", top) (fun a -> a));
  case2 (abs_expr_type_1 ("A", top) (fun a -> a)) (abs_expr_type_1 ("B", top) (fun b -> b)) (abs_expr_type_1 ("A", top) (fun a -> a));
  case2 (abs_expr_type_1 ("A", top) (fun a -> (tuple [a]))) (abs_expr_type_1 ("B", top) (fun b -> (tuple [b]))) (abs_expr_type_1 ("A", top) (fun a -> tuple[a]));

  (* others *)
  case [a] a;
  case [a; b] (inter a b);
  case [a; b; c] (inter a (inter b c));
]
