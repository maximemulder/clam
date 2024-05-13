let print_type ppf x =
  let string = Type.display x in
  Format.pp_print_string ppf string

let bool   = Alcotest.bool
let string = Alcotest.string
let type'  = Alcotest.testable print_type Type.compare

let make_test testable name subject expect (_: unit) =
  let result = subject () in
  Alcotest.check testable name expect result

let make_case testable name subject expect =
  let test = make_test testable name subject expect in
  Alcotest.test_case name `Quick test
