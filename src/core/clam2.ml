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
  List.iter (print_proof (indent + 1)) proof.children

let print_result res =
  match res with
  | Ok (proofs, _) ->
    print_endline ("TRUE");
    List.iter (print_proof 0) proofs
  | Error constraints ->
    print_endline ("FALSE");
    List.iter print_constrain constraints

let type_pair_abs = abs "A" (Some type') (fun a -> (inter (row "0" a) (row "1" a)))

let type_pair_bool = app type_pair_abs bool

let expr_pair = record ["0", true'; "1", false']

let type_list = abs "A" (Some type') (fun a -> rec' "List" (fun list -> union (row "cons" (inter (row "elem" a) (row "next" list))) (row "nil" unit)))

let constrain sub sup =
  let res = constrain sub sup ctx in
  print_endline (display sub ^ " < "  ^ display sup);
  print_result res

let check term type' =
  let res = check term type' ctx in
  print_endline (display term ^ " : "  ^ display type');
  print_result res

let () =
  (* check expr_pair type_list *)
  (* check bool type' *)
  check expr_pair type_pair_bool
