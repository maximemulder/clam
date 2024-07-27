open Core
open Ctx
open Display

open Build

open Ctx.Monad

let print_result res =
  match res with
  | Ok ((), _) ->
    print_endline ("TRUE")
  | Error constraints ->
    print_endline ("FALSE");
    List.iter(fun constrain -> match constrain with
      | SubType constrain ->
        print_endline ("  " ^ display constrain.sub ^ " < " ^ display constrain.sup)
      | HasType constrain ->
        print_endline ("  "  ^ display constrain.term ^ " : "  ^ display constrain.type')
    ) constraints

let type_pair_abs = abs "A" (Some type') (fun a -> (inter (row "0" a) (row "1" a)))

let type_pair_bool = app type_pair_abs bool

let expr_pair = record ["0", true'; "1", false']

let constrain sub sup =
  let res = constrain sub sup ctx in
  print_endline (display sub ^ " < "  ^ display sup);
  print_result res

let check term type' =
  let res = check term type' ctx in
  print_endline (display term ^ " : "  ^ display type');
  print_result res

let () =
  (* check expr_pair type_pair_bool *)
  check bool type'
