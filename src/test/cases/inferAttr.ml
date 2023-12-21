open Clam
open Vars

let test name type' name' expect (_: unit) =
  let result = TypingInfer.InfererProj2.infer (TypingInfer.infer_attr_base name') type' in
  let result = match (result, expect) with
  | (Some result, Some expect) -> TypingCompare.compare result expect
  | (None, None) -> true
  | (_, _) -> false
  in
  Alcotest.(check bool) name true result

let name type' name' expect =
  let type' = TypingDisplay.display type' in
  let expect = match expect with
  | Some expect -> TypingDisplay.display expect
  | None -> "" in
  "attr `" ^ type' ^ "` `" ^ name' ^ "` `" ^ expect ^ "`"

let case type' name' expect =
  let name = name type' name' expect in
  let test = test name type' name' expect in
  Alcotest.test_case name `Quick test

let tests = [
  (* primitives *)
  case top "foo" None;
  case unit "foo" None;
  case bool "foo" None;
  case int "foo" None;
  case string "foo" None;

  (* records *)
  case (record ["foo", a]) "foo" (Some a);
  case (record ["foo", a; "bar", b]) "bar" (Some b);
  case (record []) "foo" None;
  case (record ["bar", a]) "foo" None;

  (* bottom *)
  case bot "foo" (Some bot);
  case bot "bar" (Some bot);

  (* vars *)
  case (var "A" (record [])) "foo" None;
  case (var "A" (record ["foo", a])) "foo" (Some a);
  case (var "A" (var "B" (record ["foo", a]))) "foo" (Some a);

  (* vars & bottom *)
  case (var "A" bot) "foo" (Some bot);
  case (var "A" (var "B" bot)) "foo" (Some bot);

  (* unions *)
  case (union top top) "foo" None;
  case (union (record ["foo", a]) top) "foo" None;
  case (union top (record ["foo", a])) "foo" None;
  case (union (record []) (record [])) "foo" None;
  case (union (record ["foo", a]) (record [])) "foo" None;
  case (union (record []) (record ["foo", a])) "foo" None;
  case (union (record ["foo", a]) (record ["foo", a])) "foo" (Some a);
  case (union (record ["foo", a]) (record ["foo", b])) "foo" (Some (union a b));

  (* unions & bottom *)
  case (union bot bot) "foo" (Some bot);
  case (union bot (record [("foo", a)])) "foo" (Some a);
  case (union (record [("foo", a)]) bot) "foo" (Some a);

  (* unions & intersections *)
  case (union (inter (record [("foo", a)]) (record [("foo", b)])) (record [("foo", c)])) "foo" (Some (union (inter a b) c));

  (* intersections *)
  case (inter top top) "foo" None;
  case (inter (record ["foo", a]) top) "foo" (Some a);
  case (inter top (record ["foo", a])) "foo" (Some a);
  case (inter (record []) (record [])) "foo" None;
  case (inter (record ["foo", a]) (record [])) "foo" (Some a);
  case (inter (record []) (record ["foo", a])) "foo" (Some a);
  case (inter (record ["foo", a]) (record ["foo", a])) "foo" (Some a);
  case (inter (record ["foo", a]) (record ["foo", b])) "foo" (Some (inter a b));

  (* intersections & bottom *)
  case (inter bot bot) "foo" (Some bot);
  case (inter bot top) "foo" (Some bot);
  case (inter top bot) "foo" (Some bot);
  case (inter unit bool) "foo" (Some bot);
  case (inter bool unit) "foo" (Some bot);

(* intersections & unions *)
  case (inter (union (record ["foo", a]) (record ["foo", b])) (record ["foo", c])) "foo" (Some (union (inter a c) (inter b c)));

(* constructors *)
  case (abs "T" top (fun a -> (record ["foo", a]))) "foo" None;
  case (app (abs "T" top id) (record ["foo", a])) "foo" (Some a);
  case (app (abs "T" top (fun t -> (record ["foo", t]))) a) "foo" (Some a);

(* constructors & unions *)
  case (app (abs "T" top id) (union (record ["foo", a]) (record []))) "foo" None;
  case (app (abs "T" top id) (union (record ["foo", a]) (record ["foo", b]))) "foo" (Some (union a b));

(* constructors & intersections *)
  case (app (abs "T" top (fun t -> (inter (record ["foo", t]) (record ["foo", unit])))) top) "foo" (Some unit);
  case (app (abs "T" top (fun t -> (inter (record ["foo", unit]) (record ["foo", t])))) top) "foo" (Some unit);

(* constructors & intersections & bottom *)
  case (app (abs "T" top id) (inter unit int)) "foo" (Some bot);
  case (app (abs "T" top (fun t -> (inter (record ["foo", t]) (record ["foo", int])))) unit) "foo" (Some bot);
  case (app (abs "T" top (fun t -> (inter (record ["foo", int]) (record ["foo", t])))) unit) "foo" (Some bot);
]
