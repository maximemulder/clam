(* open Vars *)
open Type2
open Type2.Display
open Type2.Build
open Type2.Build.Default
open Util.Func

let test ctx sub sup (_: unit) =
  System.isa sub sup ctx |> fst

let name sub sup expect =
  let sub = display sub in
  let sup = display sup in
  let suffix = if expect then "" else "!" in
  "isa" ^ suffix ^ " `" ^ sub ^ "` `" ^ sup ^ "`"

let case sub sup expect ctx =
  Case.make_case Case.bool (name sub sup expect) (test ctx sub sup) expect

let case_var name bound case expect ctx =
  let bind = { Abt.name } in
  let ctx = { ctx with Context.rigids = { bind; lower = bot; upper = bound } :: ctx.Context.rigids } in
  let var = var bind in
  case var expect ctx

let tests = [
  (* bottom *)
  case bot top;
  case bot bot;
  case bot unit;

  case_var "A" bot (fun a -> case a top);
  case_var "A" bot (fun a -> case a bot);
  case_var "A" bot (fun a -> case a unit);
  case_var "A" bot (fun a -> case_var "B" bot (fun b -> case a b));
  case_var "A" bot (fun a -> case_var "B" bot (fun b -> case a b));
  case_var "A" bot (fun a -> case_var "B" a (fun b -> case b bot));

  (* variables *)
  case a a;
  case a1 a;
  case_var "A" top (fun a -> case a top);
  case_var "A" unit (fun a -> case a top);
  case_var "A" unit (fun a -> case a unit);

  (* unions *)
  case a (union a b);
  case a (union b a);
  case (union a a) (union a a);
  case (union a b) (union a b);
  case (union a b) (union b a);

  (* intersections *)
  case (inter a b) a;
  case (inter b a) b;
  case (inter a a) (inter a a);
  case (inter a b) (inter a b);
  case (inter a b) (inter b a);

  (* distributivity *)
  case (union (inter a b) (inter a c)) (inter a (union b c));
  case (inter a (union b c)) (union (inter a b) (inter a c));

  (* meets *)
  case (inter (lam a b) (lam a c)) (lam a (inter b c));
  case (lam (union a b) c) (inter (lam a c) (lam b c));
  case (inter (lam a b) (lam a c)) (lam a (inter b c));
  case (lam a (inter b c)) (inter (lam a b) (lam a c));
  case (lam (union a b) (inter c d)) (inter (lam a c) (lam b d));

  (* universal abstractions *)
  case (univ_0 "T" id) (univ_0 "T" id);
  case (univ_0 "T" id) (univ_0 "X" id);
  case (univ_0 "T" (const a)) (univ_0 "T" (const a));
  case (univ_0 "T" (const a)) (univ_0 "X" (const a));

  (* type abstractions *)
  case (abs_0 "T" id) (abs_0 "T" id);
  case (abs_0 "T" id) (abs_0 "T" (const top));
  case (abs_0 "T" (const a)) (abs_0 "T" (const a));
  case (abs_0 "T" (const a)) (abs_0 "T" (const top));

  (* type applications *)
  (* case_var "T" (abs "T" top id) (fun t -> case (app t top) top);
  case_var "T" (abs "T" top (const top)) (fun t -> case (app t top) top); *)
]
|> List.map (fun case -> case true ctx)

let tests_not = [
  (* bottom type *)
  case top bot;
  case unit bot;
  case bool bot;
  case int bot;
  case string bot;

  (* variables *)
  case a b;
  case_var "T" top (fun t1 -> case_var "T" top (fun t2 -> case t1 t2));

  (* tuples *)
  case (tuple [a]) (tuple []);
  case (tuple []) (tuple [a]);
  case (tuple [top]) (tuple [a]);

  (* records *)
  case (record []) (record ["foo", a]);
  case (record ["foo", top]) (record ["foo", a]);

  (* meets *)
  case (inter (lam a c) (lam b d)) (lam (union a b) (inter c d));

  (* type abstractions*)
  case (abs_0 "T" (const a)) (abs_0 "T" (const b));
  case (abs_0 "T" (const top)) (abs_0 "T" (const b));
  case (abs "T" bot a (const c)) (abs "T" bot b (const c));
  case (abs_0 "T" (const b)) (abs "T" bot a (const b));
  case (abs "T" bot a (const b)) (abs_0 "T" (const b));

  (* type abstractions and top *)
  case top (abs_0 "T" id);
  case top (abs_0 "T" (const top));
  case (abs_0 "T" id) top;
  case (abs_0 "T" (const top)) top;

  (* type abstractions and variables*)

  case_var "T" (abs_0 "X" id) (fun t1 -> case_var "T" (abs_0 "X" id) (fun t2 -> case (app t1 top) (app t2 top)));

  (* type applications *)
  (* case_var "T" (abs "T" top id) (fun t -> case top (app t top));
  case_var "T" (abs "T" top (const top)) (fun t -> case top (app t top)); *)
]
|> List.map (fun case -> case false ctx)
