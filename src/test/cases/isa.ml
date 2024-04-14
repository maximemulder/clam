open Vars

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
  let ctx = { ctx with Context.rigids = { bind; lower = Type.bot; upper = bound } :: ctx.Context.rigids } in
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
  case ea a;
  case_var "A" top (fun a -> case a top);
  case_var "A" unit (fun a -> case a top);
  case_var "A" unit (fun a -> case a unit);

  (* unions *)
  case a (union [a; b]);
  case a (union [b; a]);
  case (union [a; a]) (union [a; a]);
  case (union [a; b]) (union [a; b]);
  case (union [a; b]) (union [b; a]);

  (* intersections *)
  case (inter [a; b]) a;
  case (inter [b; a]) b;
  case (inter [a; a]) (inter [a; a]);
  case (inter [a; b]) (inter [a; b]);
  case (inter [a; b]) (inter [b; a]);

  (* distributivity *)
  case (union [inter [a; b]; inter [a; c]]) (inter [a; union [b; c]]);
  case (inter [a; union [b; c]]) (union [inter [a; b]; inter [a; c]]);

  (* meets *)
  case (inter [lam a b; lam a c]) (lam a (inter [b; c]));
  case (lam (union [a; b]) c) (inter [lam a c; lam b c]);
  case (inter [lam a b; lam a c]) (lam a (inter [b; c]));
  case (lam a (inter [b; c])) (inter [lam a b; lam a c]);
  case (lam (union [a; b]) (inter [c; d])) (inter [lam a c; lam b d]);

  (* universal abstractions *)
  case (univ "T" top id) (univ "T" top id);
  case (univ "T" top id) (univ "X" top id);
  case (univ "T" top (inline a)) (univ "T" top (inline a));
  case (univ "T" top (inline a)) (univ "X" top (inline a));

  (* type abstractions *)
  case (abs "T" top id) (abs "T" top id);
  case (abs "T" top id) (abs "T" top (inline top));
  case (abs "T" top (inline a)) (abs "T" top (inline a));
  case (abs "T" top (inline a)) (abs "T" top (inline top));

  (* type applications *)
  (* case_var "T" (abs "T" top id) (fun t -> case (app t top) top);
  case_var "T" (abs "T" top (inline top)) (fun t -> case (app t top) top); *)
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
  case (inter [lam a c; lam b d]) (lam (union [a; b]) (inter [c; d]));

  (* type abstractions*)
  case (abs "T" top (inline a)) (abs "T" top (inline b));
  case (abs "T" top (inline top)) (abs "T" top (inline b));
  case (abs "T" a (inline c)) (abs "T" b (inline c));
  case (abs "T" top (inline b)) (abs "T" a (inline b));
  case (abs "T" a (inline b)) (abs "T" top (inline b));

  (* type abstractions and top *)
  case top (abs "T" top id);
  case top (abs "T" top (inline top));
  case (abs "T" top id) top;
  case (abs "T" top (inline top)) top;

  (* type abstractions and variables*)

  case_var "T" (abs "X" top id) (fun t1 -> case_var "T" (abs "X" top id) (fun t2 -> case (app t1 top) (app t2 top)));

  (* type applications *)
  (* case_var "T" (abs "T" top id) (fun t -> case top (app t top));
  case_var "T" (abs "T" top (inline top)) (fun t -> case top (app t top)); *)
]
|> List.map (fun case -> case false ctx)
