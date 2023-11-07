open Vars

let test name type' attr expect (_: unit) =
  let result = Clam.TypingInfer.infer_attr_type type' attr Clam.TypingContext.empty in
  let result = match (result, expect) with
  | (Some result, Some expect) -> Clam.TypingCompare.compare result expect
  | (None, None) -> true
  | (_, _) -> false
  in
  Alcotest.(check bool) name true result

let case type' attr expect =
  let name_type = Clam.TypingDisplay.display type' in
  let name_expect = match expect with
  | Some expect -> Clam.TypingDisplay.display expect
  | None -> ""
  in
  let name = "attr `" ^ name_type ^ "` `" ^ attr ^ "` `" ^ name_expect ^ "`" in
  let test = test name type' attr expect in
  Alcotest.test_case name `Quick test

let tests = [
  (* primitives *)
  case top "foo" None;
  case unit "foo" None;

  (* record *)
  case (record []) "foo" None;
  case (record [("foo", a)]) "foo" (Some a);
  case (record [("foo", a); ("bar", b)]) "bar" (Some b);

  (* bottom *)
  case bot "foo" (Some bot);

  (* var *)
  case (var "A" (record [])) "foo" None;
  case (var "A" (record [("foo", a)])) "foo" (Some a);
  case (var "A" (var "B" (record [("foo", a)]))) "foo" (Some a);
  case (var "A" bot) "foo" (Some bot);

  (* union *)
  case (union top top) "foo" None;
  case (union (record [("foo", a)]) top) "foo" None;
  case (union (record []) (record [])) "foo" None;
  case (union (record [("foo", a)]) (record [])) "foo" None;
  case (union (record [("foo", a)]) (record [("foo", a)])) "foo" (Some a);
  case (union (record [("foo", a)]) (record [("foo", b)])) "foo" (Some (union a b));
  case (union bot bot) "foo" (Some bot);

  (* intersection *)
  case (inter top top) "foo" None;
  case (inter (record [("foo", a)]) top) "foo" (Some a);
  case (inter (record []) (record [])) "foo" None;
  case (inter (record [("foo", a)]) (record [])) "foo" (Some a);
  case (inter (record [("foo", a)]) (record [("foo", a)])) "foo" (Some a);
  case (inter (record [("foo", a)]) (record [("foo", b)])) "foo" (Some (inter a b));
  case (inter bot top) "foo" (Some bot);
  case (inter top bot) "foo" (Some bot);
]
