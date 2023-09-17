open Vars

let display_entry (param, type') =
  let param: Clam.Model.param_type = param in
  let name_param = param.name in
  let name_type = Clam.TypingDisplay.display type' in
  "`" ^ name_param ^ "`: `" ^ name_type ^ "`"

let test name entries type' expect (_: unit) =
  let result = Clam.TypingApp.apply type' entries in
  let result = Clam.TypingCompare.compare result expect in
  Alcotest.(check bool) name true result

let case entries type' expect =
  let name_entries = List.map display_entry entries in
  let name_entries = String.concat ", " name_entries in
  let name_type = Clam.TypingDisplay.display type' in
  let name_expect = Clam.TypingDisplay.display expect in
  let name = "app [" ^ name_entries ^ "] `" ^ name_type ^ "` `" ^ name_expect ^ "`" in
  let test = test name entries type' expect in
  Alcotest.test_case name `Quick test

let case_0 type' expect =
  case [] type' expect

let case_1 (app_name, app_type') type' expect =
  let param = { Clam.Model.name = app_name; type' = top } in
  let var = Clam.Model.TypeVar { pos; param } in
  let params = [(param, app_type')] in
  let type' = type' var in
  case params type' expect

let tests = [
  (* nullary *)
  case_0 top top;
  case_0 bot bot;
  case_0 unit unit;
  case_0 a a;

  (* atomic *)
  case_1 ("A", top) (fun _ -> unit) unit;
  case_1 ("A", bot) (fun _ -> unit) unit;
  case_1 ("A", d) (fun _ -> unit) unit;
  case_1 ("A", top) (fun a -> a) top;
  case_1 ("A", bot) (fun a -> a) bot;
  case_1 ("A", d) (fun a -> a) d;
  case_1 ("A", (union c d)) (fun a -> a) (union c d);

  (* composite *)
  case_1 ("A", d) (fun a -> (tuple [a; b])) (tuple [d; b]);
  case_1 ("A", d) (fun a -> (tuple [b; a])) (tuple [b; d]);
  case_1 ("A", d) (fun a -> (record [("a", a); ("b", b)])) (record [("a", d); ("b", b)]);
  case_1 ("A", d) (fun a -> (union a b)) (union d b);
  case_1 ("A", d) (fun a -> (union b a)) (union b d);
  case_1 ("A", d) (fun a -> (inter a b)) (inter d b);
  case_1 ("A", d) (fun a -> (inter b a)) (inter b d);
  case_1 ("A", d) (fun a -> (abs_expr_type_0 a)) (abs_expr_type_0 d);
  case_1 ("A", d) (fun a -> (abs_expr_type_1 ("B", a) (fun b -> b))) (abs_expr_type_1 ("B", d) (fun b -> b));
]
