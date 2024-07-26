open Ast

let todo = failwith "TODO"

let bool = todo (* TODO *)

let with_var_exis = todo (* TODO *)

let with_var_univ = todo (* TODO *)

let rec constrain sub sup =

  (* Type *)

  match sub, sup with
  | Type, Type ->
    true
  | sub, sup ->

  (* Group *)

  match sub with
  | Group sub ->
    constrain sub.body sup
  | sub ->
  match sup with
  | Group sup ->
    constrain sub sup.body
  | sup ->

  (* If *)

  match sub with
  | If sub ->
    constrain sub.cond bool &&
    false
    (* TODO: Eval ? *)
  | sub ->
  match sup with
  | If sup ->
    constrain sup.cond bool &&
    false
    (* TODO: Eval. *)
  | sup ->

  (* Type ascription *)

  match sub with
  | Ascr sub ->
    check sub.body sub.type' &&
    constrain sub.body sup
  | sub ->
  match sup with
  | Ascr sup ->
    check sup.body sup.type' &&
    constrain sub sup.body
  | sup ->

  (* Unions & Intersection *)

  match sub with
  | Union sub ->
    constrain sub.left  sup &&
    constrain sub.right sup
  | sub ->
  match sup with
  | Inter sup ->
    constrain sub sup.left &&
    constrain sub sup.right
  | sup ->
  (* TODO: Annoying cases *)

  (* Variable *)

  match sub, sup with
  | Var sub, Var sup when sub.name = sup.name ->
    true
  | sub, sup ->

  (* Bot & Top *)

  match sub with
  | Bot ->
    sup <> Type
  | sub ->
  match sup with
  | Top ->
    sub <> Type
  | _ ->

  (* TODO: Others *)
  false

and check term type' =
  match term with
  | Bot | Top ->
    constrain Type type'
  | Group group ->
    check group.body type'
  | If if' ->
    check if'.cond bool &&
    check if'.then' type' &&
    check if'.else' type'
  | Ascr ascr ->
    check ascr.body ascr.type' &&
    constrain ascr.type' type'
  | Union union ->
    constrain union.left  Top &&
    constrain union.right Top &&
    constrain Type type'
  | Inter inter ->
    constrain inter.left  Top &&
    constrain inter.right Top &&
    constrain Type type'
  | Type ->
    false
  | _ ->

    (* TODO: Others *)
    false

(*
let rec check term =
  match term with
  | Bot | Top | Type _ ->
    ()
  | Var _ ->
    ()
  | Row row ->
    check row.type'
  | Record record ->
    List.iter (fun attr -> check attr.value) record.attrs
  | Group group ->
    check group.body
  | If if' ->
    constrain if'.cond bool;
    check if'.then';
    check if'.else';
  | Ascr ascr ->
    check ascr.body;
    check ascr.type';
    constrain ascr.body ascr.type'
  | Abs abs ->
    check abs.body;
    check abs.body;
  | App app ->
    check app.arg
  | Univ univ ->
    check param.type'
*)