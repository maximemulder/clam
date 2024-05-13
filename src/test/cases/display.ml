open Type.Build
open Type.Build.Default
open Util.Func

let test type' (_: unit) =
  Type.display type'

let case name type' =
  Case.make_case Case.string name (test type') name

let tests = [
  (* atomics *)
  case "Top" top;
  case "Bot" bot;
  case "Unit" unit;
  case "Bool" bool;
  case "Int" int;
  case "String" string;
  case "A" a;

  (* tuples *)
  case "{}" (tuple []);
  case "{A}" (tuple [a]);
  case "{A, B}" (tuple [a; b]);

  (* records *)
  case "{}" (record []);
  case "{a: A}" (record [("a", a)]);
  case "{a: A, b: B}" (record [("a", a); ("b", b)]);

  (* unions and intersections *)
  case "A & B" (inter a b);
  case "A | B" (union a b);
  case "(A & B) | C" (union (inter a b) c);
  case "A | (B & C)" (union a (inter b c));
  case "A & (B & C)" (inter a (inter b c));
  case "A | (B | C)" (union a (union b c));
  case "(A & B) | (C & D)" (union (inter a b) (inter c d));

  (* lambda abstractions *)
  case "(A) -> B" (lam a b);
  case "(A) -> (B | C)" (lam a (union b c));
  case "(A, B) -> C" (lam a (lam b c));

  (* universal abstractions *)
  case "[A] -> A" (univ_0 "A" id);
  case "[A] -> B" (univ_0 "A" (const b));
  case "[A] -> (B | C)" (univ_0 "A" (const (union b c)));
  case "[A, B] -> {A, B}" (univ_0 "A" (fun a -> (univ_0 "B" (fun b -> tuple [a; b]))));

  (* type abstractions *)
  case "[A] => A" (abs_0 "A" id);
  case "[A] => B" (abs_0 "A" (const b));
  case "[A] => (B | C)" (abs_0 "A" (const (union b c)));
  case "[A, B] => {A, B}" (abs_0 "A" (fun a -> (abs_0 "B" (fun b -> tuple [a; b]))));

  (* abstractions *)
  case "(A) -> [B] -> C" (lam a (univ_0 "B" (const c)));
  case "[A] -> (B) -> C" (univ_0 "A" (const (lam b c)));

  (* type applications *)
  (* NOTE: the variable bounds are wrong but it is not a problem for now *)
  case "A[B]" (app a b);
  case "(A | B)[C]" (app (union a b) c);
  case "A[B | C]" (app a (union b c));
  case "A[B, C]" (app (app a b) c);
]
