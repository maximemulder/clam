open Clam
open Vars2

let test ctx left right (_: unit) =
  TypeSystem.is ctx left right

let name left right expect =
  let left  = TypeDisplay.display left in
  let right = TypeDisplay.display right in
  let suffix = if expect then "" else "!" in
  "is" ^ suffix ^ " `" ^ left ^ "` `" ^ right ^ "`"

let case left right expect ctx =
  Case.make_case Case.bool (name left right expect) (test ctx left right) expect

let case_var name bound case expect ctx =
  let bind = { Abt.name } in
  let ctx = TypeContext.add_bind_type ctx bind bound in
  let var = Type.base (Type.Var { bind }) in
  case var expect ctx

let tests = [
  (* bottom *)
  case bot bot;
  case_var "A" bot (fun a -> case a bot);
  case_var "A" bot (fun a -> case bot a);
  case_var "A" bot (fun a -> case_var "B" bot (fun b -> case a b));
  case_var "A" bot (fun a -> case_var "B" a (fun b -> case b bot));
  case_var "A" bot (fun a -> case_var "B" a (fun b -> case bot b));

  (* primitives *)
  case top top;
  case unit unit;
  case bool bool;
  case int int;
  case string string;
  case a a;

  (* unions *)
  case a (union [a; a]);
  case (union [a; a]) a;
  case (union [a; a]) (union [a; a]);
  case (union [a; b]) (union [a; b]);
  case (union [a; b]) (union [b; a]);
  case (union [a; union [b; c]]) (union [a; union [b; c]]);
  case (union [a; union [b; c]]) (union [union [a; b]; c]);
  case (union [union [a; b]; c]) (union [a; union [b; c]]);
  case (union [top; a]) top;
  case top (union [top; a]);
  case (union [a; union [b; ea]]) (union [a; b]);
  case (union [a; b]) (union [a; union [b; ea]]);

  (* intersections *)
  case a (inter [a; a]);
  case (inter [a; a]) a;
  case (inter [a; a]) (inter [a; a]);
  case (inter [a; b]) (inter [a; b]);
  case (inter [a; b]) (inter [b; a]);
  case (inter [top; a]) a;
  case a (inter [top; a]);
  case (inter [a; inter [b; ea]]) (inter [ea; b]);
  case (inter [ea; b]) (inter [a; inter [b; ea]]);

  (* distributivity *)
  case (union [inter [a; b]; inter [a; c]]) (inter [a; union [b; c]]);
  case (inter [a; union [b; c]]) (union [inter [a; b]; inter [a; c]]);

  (* meets *)
  case (inter [abs_expr a c; abs_expr b c]) (abs_expr (union [a; b]) c);
  case (abs_expr (union [a; b]) c) (inter [abs_expr a c; abs_expr b c]);
  case (inter [abs_expr a b; abs_expr a c]) (abs_expr a (inter [b; c]));
  case (abs_expr a (inter [b; c])) (inter [abs_expr a b; abs_expr a c]);

  (* type to expression abstractions *)
  case (abs_expr_type ("A", top) (inline a)) (abs_expr_type ("A", top) (inline a));
  case (abs_expr_type ("A", top) (fun a -> a)) (abs_expr_type ("A", top) (fun a -> a));
  case (abs_expr_type ("A", top) (fun a -> a)) (abs_expr_type ("B", top) (fun b -> b));
]
|> List.map (fun case -> case true ctx)

let tests_not = [
  (* top and bottom types *)
  case top bot;
  case bot top;
  case unit top;
  case top unit;
  case int bot;
  case bot int;

  (* primitives *)
  case unit bool;
  case bool int;
  case int string;
  case string unit;

  (* variables *)
  case a b;
  case ea a;
  case a ea;
  case ea fa;

  (* unions *)
  case a (union [a; b]);
  case (union [a; b]) a;
  case (union [top; a]) a;
  case a (union [top; a]);
  case (union [a; union [b; ea]]) (union [ea; b]);
  case (union [ea; b]) (union [a; union [b; ea]]);

  (* interesections *)
  case a (inter [a; b]);
  case (inter [a; b]) a;
  case (inter [top; a]) top;
  case top (inter [top; a]);
  case (inter [a; inter [b; ea]]) (inter [a; b]);
  case (inter [a; b]) (inter [a; inter [b; ea]]);

  (* meets *)
  case (inter [abs_expr a c; abs_expr b d]) (abs_expr (union [a; b]) (inter [c; d]));
  case (abs_expr (union [a; b]) (inter [c; d])) (inter [abs_expr a c; abs_expr b d]);

  (* ambiguous names *)
  case_var "A" top (fun a1 -> case_var "A" top (fun a2 -> case a1 a2));
  case (abs_expr_type ("A", top) (inline a)) (abs_expr_type ("A", top) id);
  case (abs_expr_type ("A", top) id) (abs_expr_type ("A", top) (inline a));

  (* type abstractions and variables *)
  case_var "T" (abs "X" top id) (fun t1 -> case_var "T" (abs "X" top id) (fun t2 -> case (app t1 top) (app t2 top)));
]
|> List.map (fun case -> case false ctx)
