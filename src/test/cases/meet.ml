open Type.Build
open Type.Build.Default
open Util.Func

let test ctx left right (_: unit) =
  Type.System.meet left right ctx |> fst

let name left right expect =
  let left   = Abt.Display.display left   in
  let right  = Abt.Display.display right  in
  let expect = Abt.Display.display expect in
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
  case a b (inter a b);
  case a a1 a1;
  case a1 a a1;
  case a1 a2 (inter a1 a2);

  (* tuples *)
  case (tuple []) (tuple []) (tuple []);
  case (tuple [a]) (tuple [a]) (tuple [a]);
  case (tuple [a]) (tuple [b]) (tuple [inter a b]);
  case (tuple [a]) (tuple [a; b]) (inter (tuple [a]) (tuple [a; b]));
  case (tuple [a; b]) (tuple [a]) (inter (tuple [a; b]) (tuple [a]));
  case (tuple [a; b]) (tuple [c; d]) (tuple [inter a c; inter b d]);

  (* records *)
  case (record []) (record []) (record []);
  case (record ["foo", a]) (record []) (record ["foo", a]);
  case (record []) (record ["foo", a]) (record ["foo", a]);
  case (record ["foo", a]) (record ["foo", b]) (record ["foo", inter a b]);
  case (record ["foo", a]) (record ["bar", b]) (record ["foo", a; "bar", b]);

  (* intersections *)
  case a (inter b c) (inter a (inter b c));
  case (inter a b) c (inter (inter a b) c);

  (* lambda abstractions *)
  case (lam a b) (lam a b) (lam a b);
  case (lam a b) (lam a c) (lam a (inter b c));
  case (lam a c) (lam b c) (lam (union a b) c);
  case (lam a c) (lam b d) (inter (lam a c) (lam b d));

  case (univ_0 "A" id) (univ_0 "A" id) (univ_0 "A" id);
  case (univ_0 "A" id) (univ_0 "B" id) (univ_0 "A" id);
  case (univ_0 "A" (fun a -> tuple [a])) (univ_0 "B" (fun b -> tuple [b])) (univ_0 "A" (fun a -> tuple[a]));

  (* TODO: Add these tests once duplicates are gone (either through sorting or exhaustive meet) *)
  (* case (abs_expr_type ("A", top) (fun a -> inter [int; a])) (abs_expr_type ("B", top) id) (abs_expr_type ("A", top) (fun a -> inter [int; a]));
  case (abs_expr_type ("A", top) id) (abs_expr_type ("B", top) (fun b -> inter [b; int])) (abs_expr_type ("A", top) (fun a -> inter [int; a])); *)
]
|> List.map (Util.Func.apply ctx)
