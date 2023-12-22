open Clam
open Vars2

let test _type' _arg _expect (_: unit) =
  (* TODO: Add real tests *)
  true

let name type' arg expect =
  let type' = TypeDisplay.display type' in
  let arg = TypeDisplay.display arg in
  let expect = match expect with
  | Some expect -> TypeDisplay.display expect
  | None -> "" in
  "app `" ^ type' ^ "` `" ^ arg ^ "` `" ^ expect ^ "`"

let case type' arg expect =
  Case.make_case Case.bool (name type' arg expect) (test type' arg expect) true

let tests = [
    (* primitives *)
    case top top None;
    case unit top None;
    case bool top None;
    case int top None;
    case string top None;
]
