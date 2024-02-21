open Vars

let test ctx type' (_: unit) =
  System.promote ctx type'

let name type' expect =
  let type'  = display type'  in
  let expect = display expect in
  "prom `" ^ type' ^ "` `" ^ expect ^ "`"

let case type' expect ctx =
  Case.make_case Case.type' (name type' expect) (test ctx type') expect

let case_var name bound case ctx =
  let bind = { Abt.name } in
  let ctx = Context.add_bind_type ctx bind bound in
  let var = var bind in
  case var ctx

let tests = [
  (* primitives *)
  case unit unit;
  case top top;
  case bot bot;

  (* variables *)
  case_var "T" top (fun t -> case t top);
  case_var "T" int (fun t -> case t int);
  case_var "T" (tuple [bool; int]) (fun t -> case t (tuple [bool; int]));
  case_var "T" int (fun t -> case_var "U" t (fun u -> case u int));
  case_var "T" (record ["foo", int]) (fun t -> case_var "U" t (fun u -> case u (record ["foo", int])));
]
|> List.map (Util.apply ctx)
