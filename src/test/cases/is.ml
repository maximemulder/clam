open Type.Build
open Type.Build.Default
open Util.Func

let test ctx left right (_: unit) =
  Type.System.is left right ctx |> fst

let name left right expect =
  let left  = Abt.Display.display left in
  let right = Abt.Display.display right in
  let suffix = if expect then "" else "!" in
  "is" ^ suffix ^ " `" ^ left ^ "` `" ^ right ^ "`"

let case left right expect ctx =
  Case.make_case Case.bool (name left right expect) (test ctx left right) expect

let case_var name bound case expect ctx =
  let bind = { Abt.Type.name } in
  let ctx = { ctx with Type.Context.rigids = { bind; lower = bot; upper = bound } :: ctx.Type.Context.rigids } in
  let var = var bind in
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
  case a (union a a);
  case (union a a) a;
  case (union a a) (union a a);
  case (union a b) (union a b);
  case (union a b) (union b a);
  case (union a (union b c)) (union a (union b c));
  case (union a (union b c)) (union (union a b) c);
  case (union (union a b) c) (union a (union b c));
  case (union top a) top;
  case top (union top a);
  case (union a (union b a1)) (union a b);
  case (union a b) (union a (union b a1));

  (* intersections *)
  case a (inter a a);
  case (inter a a) a;
  case (inter a a) (inter a a);
  case (inter a b) (inter a b);
  case (inter a b) (inter b a);
  case (inter top a) a;
  case a (inter top a);
  case (inter a (inter b a1)) (inter a1 b);
  case (inter a1 b) (inter a (inter b a1));

  (* distributivity *)
  case (union (inter a b) (inter a c)) (inter a (union b c));
  case (inter a (union b c)) (union (inter a b) (inter a c));

  (* meets *)
  case (inter (lam a c) (lam b c)) (lam (union a b) c);
  case (lam (union a b) c) (inter (lam a c) (lam b c));
  case (inter (lam a b) (lam a c)) (lam a (inter b c));
  case (lam a (inter b c)) (inter (lam a b) (lam a c));

  (* universal abstractions *)
  case (univ_0 "A" (const a)) (univ_0 "A" (const a));
  case (univ_0 "A" id) (univ_0 "A" id);
  case (univ_0 "A" id) (univ_0 "B" id);

  case (univ_0 "A" (fun a -> (univ "B" bot (inter a int) (fun b -> (lam b (tuple [a; b])))))) (univ "A" bot int (fun a -> (lam a (tuple [a; a]))));
  case (univ_0 "A" (fun a -> (univ "B" bot a (fun b -> tuple [a; b])))) (univ_0 "A" (fun a -> (tuple [a; a])));
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
  case a1 a;
  case a a1;
  case a1 a2;

  (* unions *)
  case a (union a b);
  case (union a b) a;
  case (union top a) a;
  case a (union top a);
  case (union a (union b a1)) (union a1 b);
  case (union a1 b) (union a (union b a1));

  (* interesections *)
  case a (inter a b);
  case (inter a b) a;
  case (inter top a) top;
  case top (inter top a);
  case (inter a (inter b a1)) (inter a b);
  case (inter a b) (inter a (inter b a1));

  (* meets *)
  case (inter (lam a c) (lam b d)) (lam (union a b) (inter c d));
  case (lam (union a b) (inter c d)) (inter (lam a c) (lam b d));

  (* ambiguous names *)
  case_var "A" top (fun a1 -> case_var "A" top (fun a2 -> case a1 a2));
  case (univ_0 "A" (const a)) (univ_0 "A" id);
  case (univ_0 "A" id) (univ_0 "A" (const a));

  (* type abstractions and variables *)
  case_var "T" (abs_0 "X" id) (fun t1 -> case_var "T" (abs_0 "X" id) (fun t2 -> case (app t1 top) (app t2 top)));
]
|> List.map (fun case -> case false ctx)
