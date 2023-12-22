open Clam

let print_test ppf x =
  let string = TypeDisplay.display x in
  Format.pp_print_string ppf string

let bool   = Alcotest.bool
let string = Alcotest.string
let type'  = Alcotest.testable print_test TypeCompare.compare

let make_test name subject expect (_: unit) =
  let result = subject () in
  Alcotest.(check bool) name result expect

let make_case name subject expect =
  let test = make_test name subject expect in
  Alcotest.test_case name `Quick test

let make_test2 testable name subject expect (_: unit) =
  let result = subject () in
  Alcotest.check testable name result expect

let make_case2 testable name subject expect =
  let test = make_test2 testable name subject expect in
  Alcotest.test_case name `Quick test
