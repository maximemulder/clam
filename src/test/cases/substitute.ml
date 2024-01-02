open Clam
open Vars2

let test ctx type' bind other (_: unit) =
  let entry = TypeContext.entry bind other in
  TypeSystem.substitute ctx entry type'

let name name' other type' expect =
  let other = TypeDisplay.display other in
  let type' = TypeDisplay.display type' in
  let expect = TypeDisplay.display expect in
  "app [`" ^  name' ^ "` --> `" ^ other ^ "`] `" ^ type' ^ "` `" ^ expect ^ "`"

let case name' other type' expect =
  let bind = { Abt.name = name' } in
  let var = Type.var bind in
  let type' = type' var in
  Case.make_case Case.type' (name name' other type' expect) (test ctx type' bind other) expect

let tests = [
  (* primitives *)
  case "A" top (inline unit) unit;
  case "A" bot (inline unit) unit;
  case "A" z (inline unit) unit;
  case "A" top id top;
  case "A" bot id bot;
  case "A" z id z;

  (* products *)
  case "A" z (fun a -> tuple [a; b]) (tuple [z; b]);
  case "A" z (fun a -> tuple [b; a]) (tuple [b; z]);
  case "A" z (fun a -> record ["a", a; "b", b]) (record ["a", z; "b", b]);

  (* unions and intersections *)
  (* case "A" z (fun a -> union [a; b]) (union [z; b]);
  case "A" z (fun a -> union [b; a]) (union [b; z]);
  case "A" z (fun a -> inter [a; b]) (inter [z; b]);
  case "A" z (fun a -> inter [b; a]) (inter [b; z]); *)

  (* abstractions and applications *)
  case "A" z (fun a -> abs_expr a b) (abs_expr z b);
  case "A" z (fun a -> abs_expr b a) (abs_expr b z);
  case "A" z (fun a -> abs_expr_type ("B", top) (inline a)) (abs_expr_type ("B", top) (inline z));
  case "A" z (fun a -> abs_expr_type ("B", a) id) (abs_expr_type ("B", z) (fun b -> b));
  case "A" z (fun a -> abs_expr_type ("B", a) (fun b -> tuple [a; b])) (abs_expr_type ("B", z) (fun b -> tuple [z; b]));

  (* complex substitutes *)
  (* case "A" (union [b; c]) id (union [b; c]);
  case "A" (inter [b; c]) id (inter [b; c]); *)
  case "A" (abs_expr b c) id (abs_expr b c);
]
