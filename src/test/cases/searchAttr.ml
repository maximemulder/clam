open Clam
open Vars2

let test ctx type' name' (_: unit) =
  TypeSearch.search_proj ctx (TypeCheck.infer_attr_base name') type'

let name type' name' expect =
  let type' = TypeDisplay.display type' in
  let expect = match expect with
  | Some expect -> TypeDisplay.display expect
  | None -> "" in
  "attr `" ^ type' ^ "` `" ^ name' ^ "` `" ^ expect ^ "`"

let case type' name' expect ctx =
  Case.make_case Case.type_option (name type' name' expect) (test ctx type' name') expect

let case_var name bound case ctx =
  let bind = { Abt.name } in
  let ctx = TypeContext.add_bind_type ctx bind bound in
  let var = Type.var bind in
  case var ctx

let tests = [
  (* primitives *)
  case top "foo" None;
  case unit "foo" None;
  case bool "foo" None;
  case int "foo" None;
  case string "foo" None;

  (* records *)
  case (record ["foo", a]) "foo" (Some a);
  case (record ["foo", a; "bar", b]) "bar" (Some b);
  case (record []) "foo" None;
  case (record ["bar", a]) "foo" None;

  (* bottom *)
  case bot "foo" (Some bot);
  case bot "bar" (Some bot);

  (* vars *)
  case_var "A" (record []) (fun a -> case a "foo" None);
  case_var "A" (record ["foo", c]) (fun a -> case a "foo" (Some c));
  case_var "A" (record ["foo", c]) (fun a -> case_var "B" a (fun b -> case b "foo" (Some c)));

  (* vars & bottom *)
  case_var "A" bot (fun a -> case a "foo" (Some bot));
  case_var "A" bot (fun a -> case_var "B" a (fun b -> case b "foo" (Some bot)));

  (* unions *)
  case (union [top; top]) "foo" None;
  case (union [record ["foo", a]; top]) "foo" None;
  case (union [top; record ["foo", a]]) "foo" None;
  case (union [record []; record []]) "foo" None;
  case (union [record ["foo", a]; record []]) "foo" None;
  case (union [record []; record ["foo", a]]) "foo" None;
  case (union [record ["foo", a]; record ["foo", a]]) "foo" (Some a);
  case (union [record ["foo", a]; record ["foo", b]]) "foo" (Some (union [a; b]));

  (* unions & bottom *)
  case (union [bot; bot]) "foo" (Some bot);
  case (union [bot; record ["foo", a]]) "foo" (Some a);
  case (union [record ["foo", a]; bot]) "foo" (Some a);

  (* unions & intersections *)
  case (union [inter [record ["foo", a]; record ["foo", b]]; record ["foo", c]]) "foo" (Some (union [inter [a; b]; c]));

  (* intersections *)
  case (inter [top; top]) "foo" None;
  case (inter [record ["foo", a]; top]) "foo" (Some a);
  case (inter [top; record ["foo", a]]) "foo" (Some a);
  case (inter [record []; record []]) "foo" None;
  case (inter [record ["foo", a]; record []]) "foo" (Some a);
  case (inter [record []; record ["foo", a]]) "foo" (Some a);
  case (inter [record ["foo", a];record ["foo", a]]) "foo" (Some a);
  case (inter [record ["foo", a]; record ["foo", b]]) "foo" (Some (inter [a; b]));

  (* intersections & bottom *)
  case (inter [bot; bot]) "foo" (Some bot);
  case (inter [bot; top]) "foo" (Some bot);
  case (inter [top; bot]) "foo" (Some bot);
  case (inter [unit; bool]) "foo" (Some bot);
  case (inter [bool; unit]) "foo" (Some bot);

(* intersections & unions *)
  case (inter [union [record ["foo", a]; record ["foo", b]]; record ["foo", c]]) "foo" (Some (union [inter [a; c]; inter [b; c]]));

(* constructors *)
  case (abs "T" top (fun a -> (record ["foo", a]))) "foo" None;
  case (app (abs "T" top id) (record ["foo", a])) "foo" (Some a);
  case (app (abs "T" top (fun t -> (record ["foo", t]))) a) "foo" (Some a);

(* constructors & unions *)
  case (app (abs "T" top id) (union [record ["foo", a]; record []])) "foo" None;
  case (app (abs "T" top id) (union [record ["foo", a]; record ["foo", b]])) "foo" (Some (union [a; b]));

(* constructors & intersections *)
  (* case (app (abs "T" top (fun t -> inter [record ["foo", t]; record ["foo", unit]])) top) "foo" (Some unit);
  case (app (abs "T" top (fun t -> inter [record ["foo", unit]; record ["foo", t]])) top) "foo" (Some unit);

(* constructors & intersections & bottom *)
  case (app (abs "T" top id) (inter [unit; int])) "foo" (Some bot);
  case (app (abs "T" top (fun t -> inter [record ["foo", t]; record ["foo", int]])) unit) "foo" (Some bot);
  case (app (abs "T" top (fun t -> inter [record ["foo", int]; record ["foo", t]])) unit) "foo" (Some bot); *)
]
|> List.map (Utils.apply ctx)
