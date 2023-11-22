open Clam
open Vars

let test name type' expect (_: unit) =
  let result = Typing.promote type' in
  let result = TypingCompare.compare result expect in
  Alcotest.(check bool) name true result

let name type' expect =
  let type' = TypingDisplay.display type' in
  let expect = TypingDisplay.display expect in
  "prom `" ^ type' ^ "` `" ^ expect ^ "`"

let case type' expect =
  let name = name type' expect in
  let test = test name type' expect in
  Alcotest.test_case name `Quick test

let tests = [
  (* primitives *)
  case unit unit;
  case top top;
  case bot bot;

  (* variables *)
  case (with_var ("A", top) (fun a -> a)) top;
  case (with_var ("A", int) (fun a -> a)) int;
  case (with_var ("A", (tuple [bool; int])) id) (tuple [bool; int]);
  case (with_var ("A", int) (fun a -> with_var ("B", a) id)) int;
  case (with_var ("A", (record [("foo", int)])) (fun a -> with_var ("B", a) id)) (record[("foo", int)]);
]
