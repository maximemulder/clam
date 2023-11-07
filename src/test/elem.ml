open Vars

let test name type' elem expect (_: unit) =
  let result = Clam.TypingInfer.infer_elem_type type' elem Clam.TypingContext.empty in
  let result = match (result, expect) with
  | (Some result, Some expect) -> Clam.TypingCompare.compare result expect
  | (None, None) -> true
  | (_, _) -> false
  in
  Alcotest.(check bool) name true result

let case type' elem expect =
  let name_type = Clam.TypingDisplay.display type' in
  let name_expect = match expect with
  | Some expect -> Clam.TypingDisplay.display expect
  | None -> ""
  in
  let name = "elem `" ^ name_type ^ "` `" ^ (string_of_int elem) ^ "` `" ^ name_expect ^ "`" in
  let test = test name type' elem expect in
  Alcotest.test_case name `Quick test

let tests = [
  (* primitives *)
  case top 0 None;
  case unit 0 None;

  (* record *)
  case (tuple []) 0 None;
  case (tuple [a]) 0 (Some a);
  case (tuple [a]) 1 None;
  case (tuple [a; b]) 1 (Some b);

  (* bottom *)
  case bot 0 (Some bot);
  case bot 1 (Some bot);

  (* var *)
  case (var "A" (tuple [])) 0 None;
  case (var "A" (tuple [a])) 0 (Some a);
  case (var "A" (var "B" (tuple [a]))) 0 (Some a);
  case (var "A" bot) 0 (Some bot);

  (* union *)
  case (union top top) 0 None;
  case (union (tuple [a]) top) 0 None;
  case (union (tuple []) (tuple [])) 0 None;
  case (union (tuple [a]) (tuple [])) 0 None;
  case (union (tuple [a]) (tuple [a])) 0 (Some a);
  case (union (tuple [a]) (tuple [b])) 0 (Some (union a b));
  case (union bot bot) 0 (Some bot);

  (* intersection *)
  case (inter top top) 0 None;
  case (inter (tuple [a]) top) 0 (Some a);
  case (inter (tuple []) (tuple [])) 0 None;
  case (inter (tuple [a]) (tuple [])) 0 (Some bot);
  case (inter (tuple [a]) (tuple [a])) 0 (Some a);
  case (inter (tuple [a]) (tuple [b])) 0 (Some (inter a b));
  case (inter bot top) 0 (Some bot);
  case (inter top bot) 0 (Some bot);
]
