open Type.Build
open Type.Build.Default
open Util.Func

let test ctx type' bind other (_: unit) =
  Type.System.substitute bind other type' ctx |> fst

let name name' other type' expect =
  let other  = Type.display other  in
  let type'  = Type.display type'  in
  let expect = Type.display expect in
  "app [`" ^  name' ^ "` --> `" ^ other ^ "`] `" ^ type' ^ "` `" ^ expect ^ "`"

let case name' other type' expect =
  let bind = { Abt.name = name' } in
  let var = var bind in
  let type' = type' var in
  Case.make_case Case.type' (name name' other type' expect) (test ctx type' bind other) expect

let tests = [
  (* primitives *)
  case "A" top (const unit) unit;
  case "A" bot (const unit) unit;
  case "A" z (const unit) unit;
  case "A" top id top;
  case "A" bot id bot;
  case "A" z id z;

  (* products *)
  case "A" z (fun a -> tuple [a; b]) (tuple [z; b]);
  case "A" z (fun a -> tuple [b; a]) (tuple [b; z]);
  case "A" z (fun a -> record ["a", a; "b", b]) (record ["a", z; "b", b]);

  (* unions and intersections *)
  case "A" z (fun a -> union a b) (union z b);
  case "A" z (fun a -> union b a) (union b z);
  case "A" z (fun a -> inter a b) (inter z b);
  case "A" z (fun a -> inter b a) (inter b z);

  (* abstractions and applications *)
  case "A" z (fun a -> lam a b) (lam z b);
  case "A" z (fun a -> lam b a) (lam b z);
  case "A" z (fun a -> univ_0 "B" (const a)) (univ_0 "B" (const z));
  case "A" z (fun a -> univ "B" bot a id) (univ "B" bot z id);
  case "A" z (fun a -> univ "B" bot a (fun b -> tuple [a; b])) (univ "B" bot z (fun b -> tuple [z; b]));

  (* complex substitutes *)
  case "A" (union b c) id (union b c);
  case "A" (inter b c) id (inter b c);
  case "A" (lam b c) id (lam b c);
]
