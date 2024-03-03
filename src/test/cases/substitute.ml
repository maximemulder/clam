open Vars

let test ctx type' bind other (_: unit) =
  System.substitute ctx type' bind other

let name name' other type' expect =
  let other  = display other  in
  let type'  = display type'  in
  let expect = display expect in
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
  case "A" z (fun a -> lam a b) (lam z b);
  case "A" z (fun a -> lam b a) (lam b z);
  case "A" z (fun a -> univ "B" top (inline a)) (univ "B" top (inline z));
  case "A" z (fun a -> univ "B" a id) (univ "B" z id);
  case "A" z (fun a -> univ "B" a (fun b -> tuple [a; b])) (univ "B" z (fun b -> tuple [z; b]));

  (* complex substitutes *)
  (* case "A" (union [b; c]) id (union [b; c]);
  case "A" (inter [b; c]) id (inter [b; c]); *)
  case "A" (lam b c) id (lam b c);
]
