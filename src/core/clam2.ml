module Res = Result
open Core
open Ctx
open Display

open Build

let print_constrain constrain =
   match constrain with
  | SubType constrain ->
    print_endline ("  " ^ display constrain.sub ^ " < " ^ display constrain.sup)
  | HasType constrain ->
    print_endline ("  "  ^ display constrain.term ^ " : "  ^ display constrain.type')

let rec print_proof indent proof =
  print_string (Util.string_repeat indent "  ");
  print_constrain proof.constrain;
  List.iter (print_proof (indent + 1)) proof.subproofs

let print_result res =
  match res with
  | Ok (proof, _) ->
    print_endline ("TRUE");
    print_proof 0 proof
  | Error constraints ->
    print_endline ("FALSE");
    List.iter print_constrain constraints

let type_pair_abs = abs "A" (Some type') (fun a -> (record ["0", a; "1", a]))

let type_pair_bool = app type_pair_abs bool

let expr_pair = record ["0", true'; "1", false']

let type_list = abs "A" (Some type') (fun a -> rec' "List" (fun list -> union (record ["cons", record ["elem", a; "next", list]]) (record ["nil", unit])))

let constrain sub sup =
  let res = sub_type sub sup ctx in
  print_endline (display sub ^ " < "  ^ display sup);
  print_result res

let check term type' =
  let res = has_type term type' ctx in
  print_endline (display term ^ " : "  ^ display type');
  print_result res

let () =
  (* check expr_pair type_list *)
  (* check bool type' *)
  check expr_pair type_pair_bool;
  check (record ["a", bool]) (record ["a", bool]);
