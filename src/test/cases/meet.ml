open Vars

let test ctx left right (_: unit) =
  System.meet left right ctx |> fst

let name left right expect =
  let left   = display left   in
  let right  = display right  in
  let expect = display expect in
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

  (* lambda abstractions *)
  case (lam a b) (lam a b) (lam a b);
  case (lam a b) (lam a c) (lam a (inter [b; c]));
  case (lam a c) (lam b c) (lam (union [a; b]) c);
  case (lam a c) (lam b d) (inter [lam a c; lam b d]);

  (* universal abstractions *)
  case (univ "A" top (inline c)) (univ "B" top (inline d)) (univ "A" top (inline (inter [c; d])));
  case (univ "A" unit (inline a)) (univ "B" top (inline b)) bot;
  case (univ "A" top (inline a)) (univ "B" unit (inline b)) bot;

  case (univ "A" top (fun a -> a)) (univ "A" top id) (univ "A" top id);
  case (univ "A" top (fun a -> a)) (univ "B" top id) (univ "A" top id);
  case (univ "A" top (fun a -> tuple [a])) (univ "B" top (fun b -> tuple [b])) (univ "A" top (fun a -> tuple[a]));

  (* TODO: Add these tests once duplicates are gone (either through sorting or exhaustive meet) *)
  (* case (abs_expr_type ("A", top) (fun a -> inter [int; a])) (abs_expr_type ("B", top) id) (abs_expr_type ("A", top) (fun a -> inter [int; a]));
  case (abs_expr_type ("A", top) id) (abs_expr_type ("B", top) (fun b -> inter [b; int])) (abs_expr_type ("A", top) (fun a -> inter [int; a])); *)
]
|> List.map (Util.Func.apply ctx)
