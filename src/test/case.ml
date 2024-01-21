open Clam

let print_type ppf x =
  let string = TypeDisplay.display x in
  Format.pp_print_string ppf string

let print_type_option ppf x =
  let string = match x with
  | Some x -> TypeDisplay.display x
  | None -> "" in
  Format.pp_print_string ppf string

let print_type_type_option ppf x =
  let string = match x with
  | Some (x, y) -> TypeDisplay.display x ^ " " ^ TypeDisplay.display y
  | None -> "" in
  Format.pp_print_string ppf string

let compare_type_type x y =
  TypeCompare.compare (fst x) (fst y) &&
  TypeCompare.compare (snd x) (snd y)

let bool   = Alcotest.bool
let string = Alcotest.string
let type'  = Alcotest.testable print_type TypeCompare.compare
let type_option = Alcotest.testable print_type_option (Option.equal TypeCompare.compare)
let type_type_option = Alcotest.testable print_type_type_option (Option.equal compare_type_type)

let make_test testable name subject expect (_: unit) =
  let result = subject () in
  Alcotest.check testable name expect result

let make_case testable name subject expect =
  let test = make_test testable name subject expect in
  Alcotest.test_case name `Quick test
