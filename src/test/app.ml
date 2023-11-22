open Clam
open Vars

let test name entry_param entry_type type' expect (_: unit) =
  let entry = TypingApp.entry entry_param entry_type in
  let result = TypingApp.apply type' entry in
  let result = TypingCompare.compare result expect in
  Alcotest.(check bool) name true result

let name param sub type' expect =
  let param = (param: Model.param_type).name in
  let sub = TypingDisplay.display sub in
  let type' = TypingDisplay.display type' in
  let expect = TypingDisplay.display expect in
  "app [`" ^  param ^ "` --> `" ^ sub ^ "`] `" ^ type' ^ "` `" ^ expect ^ "`"

let case pair type' expect =
  let entry_param = { Model.name = fst pair; type' = top } in
  let entry_type = snd pair in
  let type' =  type' (Model.TypeVar { Model.pos; param = entry_param }) in
  let name = name entry_param entry_type type' expect in
  let test = test name entry_param entry_type type' expect in
  Alcotest.test_case name `Quick test

let tests = [
  (* primitives *)
  case ("A", top) (fun _ -> unit) unit;
  case ("A", bot) (fun _ -> unit) unit;
  case ("A", z) (fun _ -> unit) unit;
  case ("A", top) id top;
  case ("A", bot) id bot;
  case ("A", z) id z;

  (* products *)
  case ("A", z) (fun a -> (tuple [a; b])) (tuple [z; b]);
  case ("A", z) (fun a -> (tuple [b; a])) (tuple [b; z]);
  case ("A", z) (fun a -> (record [("a", a); ("b", b)])) (record [("a", z); ("b", b)]);

  (* unions and intersections *)
  case ("A", z) (fun a -> (union a b)) (union z b);
  case ("A", z) (fun a -> (union b a)) (union b z);
  case ("A", z) (fun a -> (inter a b)) (inter z b);
  case ("A", z) (fun a -> (inter b a)) (inter b z);

  (* abstractions and applications *)
  case ("A", z) (fun a -> (abs_expr a b)) (abs_expr z b);
  case ("A", z) (fun a -> (abs_expr b a)) (abs_expr b z);
  case ("A", z) (fun a -> (abs_expr_type ("B", top) (fun _ -> a))) (abs_expr_type ("B", top) (fun _ -> z));
  case ("A", z) (fun a -> (abs_expr_type ("B", a) id)) (abs_expr_type ("B", z) (fun b -> b));
  case ("A", z) (fun a -> (abs_expr_type ("B", a) (fun b -> (tuple [a; b])))) (abs_expr_type ("B", z) (fun b -> (tuple [z; b])));

  (* complex substitutes *)
  case ("A", (union b c)) id (union b c);
  case ("A", (inter b c)) id (inter b c);
  case ("A", (abs_expr b c)) id (abs_expr b c);
]
