let print_type ppf x =
  let string = Type2.Display.display x in
  Format.pp_print_string ppf string

let print_type_type_option ppf x =
  let string = match x with
  | Some (x, y) -> Type.display x ^ " " ^ Type.display y
  | None -> "" in
  Format.pp_print_string ppf string

let compare_type_type x y =
  Type.Compare.compare (fst x) (fst y) &&
  Type.Compare.compare (snd x) (snd y)

let bool   = Alcotest.bool
let string = Alcotest.string
let type'  = Alcotest.testable print_type Type2.Compare.compare
let type_type_option = Alcotest.testable print_type_type_option (Option.equal compare_type_type)

let make_test testable name subject expect (_: unit) =
  let result = subject () in
  Alcotest.check testable name expect result

let make_case testable name subject expect =
  let test = make_test testable name subject expect in
  Alcotest.test_case name `Quick test
