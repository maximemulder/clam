open Vars

let test name left right expect (_: unit) =
  let result = Clam.Typing.meet left right in
  let result = Clam.TypingCompare.compare result expect in
  Alcotest.(check bool) name true result

let case left right expect =
  let name_left   = Clam.TypingDisplay.display left in
  let name_right  = Clam.TypingDisplay.display right in
  let name_expect = Clam.TypingDisplay.display expect in
  let name = "meet `" ^ name_left ^ "` `" ^ name_right ^ "` `" ^ name_expect ^ "`" in
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

  (* tuples *)
  case (tuple []) (tuple []) (tuple []);
  case (tuple [a]) (tuple [a]) (tuple [a]);
  case (tuple [a; c]) (tuple [b; d]) (tuple [(inter a b); (inter c d)]);
  case (tuple [a]) (tuple [a; b]) bot;
  case (tuple [a; b]) (tuple [a]) bot;

  (* records *)
  case (record []) (record []) (record []);
  case (record [("foo", a)]) (record []) (record [("foo", a)]);
  case (record []) (record [("foo", a)]) (record [("foo", a)]);
  case (record [("foo", a)]) (record [("foo", b)]) (record [("foo", (inter a b))]);
  case (record [("foo", a)]) (record [("bar", b)]) (record [("foo", a); ("bar", b)]);

  (* expression to expression abstractions *)
  case (abs_expr [a] b) (abs_expr [a] b) (abs_expr [a] b);
  case (abs_expr [a] c) (abs_expr [b] c) (abs_expr [(inter a b)] c);
  case (abs_expr [a] b) (abs_expr [a] c) (abs_expr [a] (inter b c));
  case (abs_expr [a] c) (abs_expr [b] d) (abs_expr [(inter a b)] (inter c d));
]
