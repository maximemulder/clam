open Vars

let test ctx left right (_: unit) =
  System2.join left right ctx |> fst

let name left right expect =
  let left   = display left   in
  let right  = display right  in
  let expect = display expect in
  "join `" ^ left ^ "` `" ^ right ^ "` `" ^ expect ^ "`"

let case left right expect ctx =
  Case.make_case Case.type' (name left right expect) (test ctx left right) expect

let tests = [
  (* top *)
  case top top top;
  case top a top;
  case a top top;

  (* bottom *)
  case bot bot bot;
  case bot a a;
  case a bot a;

  (* primitives *)
  case unit unit unit;
  case bool bool bool;
  case int int int;
  case string string string;

  (* variables *)
  case a a a;
  case a b (union [a; b]);
  case a ea a;
  case ea a a;
  case ea fa (union [ea; fa]);

  (* unions *)
  case a (union [b; c]) (union [a; union [b; c]]);
  case (union [a; b]) c (union [a; union [b; c]]);

  (* tuples *)
  case (tuple []) (tuple []) (tuple []);
  case (tuple [top]) (tuple [a]) (tuple [top]);
  case (tuple [a]) (tuple [top]) (tuple [top]);
  case (tuple [a]) (tuple [b]) (union [tuple [a]; tuple [b]]);
  case (tuple [a]) (tuple [a; b]) (union [tuple [a]; tuple [a; b]]);
  case (tuple [a; b]) (tuple [a]) (union [tuple [a; b]; tuple [a]]);
  case (tuple [a; b]) (tuple [c; d]) (union [tuple [a; b]; tuple [c; d]]);

  (* records *)
  case (record []) (record []) (record []);
  case (record ["foo", a]) (record []) (record []);
  case (record []) (record ["foo", a]) (record []);
  case (record ["foo", top]) (record ["foo", a]) (record ["foo", top]);
  case (record ["foo", a]) (record ["foo", top]) (record ["foo", top]);
  case (record ["foo", a]) (record ["foo", b]) (union [record ["foo", a]; record ["foo", b]]);
  case (record ["foo", a]) (record ["bar", b]) (union [record ["foo", a]; record ["bar", b]]);

  (* lambda abstractions *)
  case (lam a b) (lam a b) (lam a b);
  case (lam top b) (lam a b) (lam a b);
  case (lam a b) (lam top b) (lam a b);
  case (lam a top) (lam a b) (lam a top);
  case (lam a b) (lam a top) (lam a top);
  case (lam a c) (lam b c) (union [lam a c; lam b c]);
  case (lam a b) (lam a c) (union [lam a b; lam a c]);
  case (lam a b) (lam c d) (union [lam a b; lam c d]);
]
|> List.map (Util.apply ctx)
