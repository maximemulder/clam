open Vars

let test type' (_: unit) =
  display type'

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
  case "A & B" (inter [a; b]);
  case "A | B" (union [a; b]);
  case "(A & B) | C" (union [inter [a; b]; c]);
  case "A | (B & C)" (union [a; inter [b; c]]);
  case "A & B & C" (inter [a; b; c]);
  case "A | B | C" (union [a; b; c]);
  case "(A & B) | (C & D)" (union [inter [a; b]; inter [c; d]]);

  (* lambda abstractions *)
  case "(A) -> B" (lam a b);
  case "(A) -> (B | C)" (lam a (union [b; c]));
  case "(A, B) -> C" (lam a (lam b c));

  (* universal abstractions *)
  case "[A] -> A" (univ "A" top id);
  case "[A] -> B" (univ "A" top (inline b));
  case "[A] -> (B | C)" (univ "A" top (inline (union [b; c])));
  case "[A, B] -> {A, B}" (univ "A" top (fun a -> (univ "B" top (fun b -> tuple [a; b]))));

  (* type abstractions *)
  case "[A] => A" (abs "A" top id);
  case "[A] => B" (abs "A" top (inline b));
  case "[A] => (B | C)" (abs "A" top (inline (union [b; c])));
  case "[A, B] => {A, B}" (abs "A" top (fun a -> (abs "B" top (fun b -> tuple [a; b]))));

  (* abstractions *)
  case "(A) -> [B] -> C" (lam a (univ "B" top (inline c)));
  case "[A] -> (B) -> C" (univ "A" top (inline (lam b c)));

  (* type applications *)
  (* NOTE: the variable bounds are wrong but it is not a problem for now *)
  case "A[B]" (app a b);
  case "(A | B)[C]" (app (union [a; b]) c);
  case "A[B | C]" (app a (union [b; c]));
  case "A[B, C]" (app (app a b) c);
]
