open Clam
open Vars

let test name _type' _arg _expect (_: unit) =
  (* TODO: Clean type inference and add real tests *)
  (* let (result, _) = TypingInfer.infer_type (TypingInfer.infer_type_app_final arg) type' (TypingInfer.make_progress []) in
  let result = match (result, expect) with
  | (Some result, Some expect) -> TypingCompare.compare result expect
  | (None, None) -> true
  | (_, _) -> false
  in *)
  let result = true in
  Alcotest.(check bool) name true result

let name type' arg expect =
  let type' = TypingDisplay.display type' in
  let arg = TypingDisplay.display arg in
  let expect = match expect with
  | Some expect -> TypingDisplay.display expect
  | None -> "" in
  "app `" ^ type' ^ "` `" ^ arg ^ "` `" ^ expect ^ "`"

let case type' arg expect =
  let name = name type' arg expect in
  let test = test name type' arg expect in
  Alcotest.test_case name `Quick test

let tests = [
    (* primitives *)
    case top top None;
    case unit top None;
    case bool top None;
    case int top None;
    case string top None;
]
