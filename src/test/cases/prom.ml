open Clam
open Vars

let test type' expect (_: unit) =
  let result = Typing.promote type' in
  TypingCompare.compare result expect

let name type' expect =
  let type' = TypingDisplay.display type' in
  let expect = TypingDisplay.display expect in
  "prom `" ^ type' ^ "` `" ^ expect ^ "`"

let case type' expect =
  Case.make_case (name type' expect) (test type' expect) true

let tests = [
  (* primitives *)
  case unit unit;
  case top top;
  case bot bot;

  (* variables *)
  case (with_var "T" top id) top;
  case (with_var "T" int id) int;
  case (with_var "T" (tuple [bool; int]) id) (tuple [bool; int]);
  case (with_var "T" int (fun t -> with_var "B" t id)) int;
  case (with_var "T" (record [("foo", int)]) (fun t -> with_var "B" t id)) (record[("foo", int)]);
]
