open Clam
open Vars

let test name abs expect (_: unit) =
  let result = TypingInfer.InfererApp2.infer TypingInfer.infer_app_base abs in
  let result = match (result, expect) with
  | (Some result, Some expect) ->
    TypingCompare.compare result.arg (fst expect)
    && TypingCompare.compare result.ret (snd expect)
  | (None, None) -> true
  | (_, _) -> false
  in
  Alcotest.(check bool) name true result

let name abs expect =
  let abs = TypingDisplay.display abs in
  let expect_arg = match expect with
  | Some expect -> TypingDisplay.display (fst expect)
  | None -> "" in
  let expect_ret = match expect with
  | Some expect -> TypingDisplay.display (snd expect)
  | None -> "" in
  "app `" ^ abs ^ "` `" ^ expect_arg ^ "` `" ^ expect_ret ^ "`"

let case abs expect =
  let name = name abs expect in
  let test = test name abs expect in
  Alcotest.test_case name `Quick test

let tests = [
  (* primitives *)
  case top None;
  case unit None;
  case bool None;
  case int None;
  case string None;

  (* abstractions *)
  case (abs_expr a b) (Some (a, b));
  case (abs_expr top top) (Some (top, top));
  case (abs_expr bot bot) (Some (bot, bot));

  (* bottom *)
  case bot (Some (top, bot));

  (* vars *)
  case (var "A" a) None;
  case (var "A" (abs_expr a b)) (Some (a, b));
  case (var "A" (var "B" (abs_expr a b))) (Some (a, b));

  (* vars & bottom *)
  case (var "A" bot) (Some (top, bot));
  case (var "A" (var "B" bot)) (Some (top, bot));

  (* unions *)
  case (union top top) None;
  case (union (abs_expr a b) top) None;
  case (union top (abs_expr a b)) None;
  case (union (abs_expr a b) (abs_expr a b)) (Some (a, b));
  case (union (abs_expr a b) (abs_expr a c)) (Some (a, union b c));
  case (union (abs_expr a c) (abs_expr b c)) (Some (inter a b, c));
  case (union (abs_expr a c) (abs_expr b d)) (Some (inter a b, union c d));

  (* unions & bottom *)
  case (union bot bot) (Some (top, bot));
  case (union bot (abs_expr a b)) (Some (a, b));
  case (union (abs_expr a b) bot) (Some (a, b));

  (* intersections *)
  case (inter top top) None;
  case (inter (abs_expr a b) top) (Some (a, b));
  case (inter top (abs_expr a b)) (Some (a, b));
  case (inter (abs_expr a b) (abs_expr a b)) (Some (a, b));
  case (inter (abs_expr a b) (abs_expr a c)) (Some (a, (inter b c)));
  case (inter (abs_expr a c) (abs_expr b c)) (Some (union a b, c));
  case (inter (abs_expr a c) (abs_expr b d)) (Some (union a b, inter c d));

  (* intersections & bottom *)
  case (inter bot bot) (Some (top, bot));
  case (inter bot top) (Some (top, bot));
  case (inter top bot) (Some (top, bot));
  case (inter unit bool) (Some (top, bot));
  case (inter bool unit) (Some (top, bot));

  (* intersections & unions *)
  case (inter (union (abs_expr a d) (abs_expr b e)) (abs_expr c f)) (Some (inter (union a c) (union b c), (union (inter d f) (inter e f))));

  (* constructors *)
  case (abs "T" top (fun a -> (abs_expr a b))) None;
  case (app (abs "T" top id) (abs_expr a b)) (Some (a, b));
  case (app (abs "T" top (fun t -> (abs_expr t b))) a) (Some (a, b));

  (* constructors & unions *)
  case (app (abs "T" top id) (union (abs_expr a c) top)) None;
  case (app (abs "T" top id) (union (abs_expr a c) (abs_expr b d))) (Some (inter a b, union c d));

  (* constructors & intersections *)
  case (app (abs "T" top (fun t -> (inter (abs_expr t b) top))) a) (Some (a, b));
  case (app (abs "T" top (fun t -> (inter (abs_expr t b) (abs_expr t c)))) a) (Some (a, inter b c));
  case (app (abs "T" top (fun t -> (inter (abs_expr a t) (abs_expr b t)))) c) (Some (union a b, c));

  (* constructors & intersections & bottom *)
  case (app (abs "T" top id) (inter unit bool)) (Some (top, bot));
  case (app (abs "T" top (fun t -> (inter (abs_expr t b) unit))) bool) (Some (top, bot));
  case (app (abs "T" top (fun t -> (inter unit (abs_expr a t)))) bool) (Some (top, bot));
]
