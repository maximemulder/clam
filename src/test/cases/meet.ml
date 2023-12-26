open Clam
open Vars2

let test ctx left right (_: unit) =
  TypeSystem.meet ctx left right

let name left right expect =
  let left   = TypeDisplay.display left   in
  let right  = TypeDisplay.display right  in
  let expect = TypeDisplay.display expect in
  "meet `" ^ left ^ "` `" ^ right ^ "` `" ^ expect ^ "`"

let case left right expect ctx =
  Case.make_case Case.type' (name left right expect) (test ctx left right) expect

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
  case string string string;

  (* variables *)
  case a a a;
  case a b (inter [a; b]);
  case a ea ea;
  case ea a ea;
  case ea fa (inter [ea; fa]);

  (* tuples *)
  case (tuple []) (tuple []) (tuple []);
  case (tuple [a]) (tuple [a]) (tuple [a]);
  case (tuple [a]) (tuple [b]) (tuple [inter [a; b]]);
  case (tuple [a]) (tuple [a; b]) bot;
  case (tuple [a; b]) (tuple [a]) bot;
  case (tuple [a; b]) (tuple [c; d]) (tuple [inter [a; c]; inter [b; d]]);

  (* records *)
  case (record []) (record []) (record []);
  case (record ["foo", a]) (record []) (record ["foo", a]);
  case (record []) (record ["foo", a]) (record ["foo", a]);
  case (record ["foo", a]) (record ["foo", b]) (record ["foo", inter [a; b]]);
  case (record ["foo", a]) (record ["bar", b]) (record ["foo", a; "bar", b]);

  (* intersections *)
  case a (inter [b; c]) (inter [a; inter [b; c]]);
  case (inter [a; b]) c (inter [a; inter [b; c]]);

  (* expression to expression abstractions *)
  case (abs_expr a b) (abs_expr a b) (abs_expr a b);
  case (abs_expr a b) (abs_expr a c) (abs_expr a (inter [b; c]));
  case (abs_expr a c) (abs_expr b c) (abs_expr (union [a; b]) c);
  case (abs_expr a c) (abs_expr b d) (inter [abs_expr a c; abs_expr b d]);

  (* type to expression abstractions *)
  case (abs_expr_type ("A", top) (inline c)) (abs_expr_type ("B", top) (inline d)) (abs_expr_type ("A", top) (inline (inter [c; d])));
  case (abs_expr_type ("A", unit) (inline a)) (abs_expr_type ("B", top) (inline b)) bot;
  case (abs_expr_type ("A", top) (inline a)) (abs_expr_type ("B", unit) (inline b)) bot;

  case (abs_expr_type ("A", top) (fun a -> a)) (abs_expr_type ("A", top) (fun a -> a)) (abs_expr_type ("A", top) (fun a -> a));
  case (abs_expr_type ("A", top) (fun a -> a)) (abs_expr_type ("B", top) (fun b -> b)) (abs_expr_type ("A", top) (fun a -> a));
  case (abs_expr_type ("A", top) (fun a -> tuple [a])) (abs_expr_type ("B", top) (fun b -> tuple [b])) (abs_expr_type ("A", top) (fun a -> tuple[a]));

  (* TODO: Add these tests once duplicates are gone (either through sorting or exhaustive meet) *)
  (* case (abs_expr_type ("A", top) (fun a -> inter [int; a])) (abs_expr_type ("B", top) id) (abs_expr_type ("A", top) (fun a -> inter [int; a]));
  case (abs_expr_type ("A", top) id) (abs_expr_type ("B", top) (fun b -> inter [b; int])) (abs_expr_type ("A", top) (fun a -> inter [int; a])); *)
]
|> List.map (Utils.apply ctx)
