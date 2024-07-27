open Ast
open Ctx

let todo () = failwith "TODO"

let bool span =
  Var { span; ident = {name = "Bool"; index = 0} }

let true' span =
  Var { span; ident = {name = "True"; index = 0} }

let false' span =
  Var { span; ident = {name = "False"; index = 0} }

let rec constrain sub sup =

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
    check sub.cond (bool sub.span) &&
    check sub.then' Type &&
    check sub.else' Type &&
    constrain (eval (If sub)) sup
  | sub ->
  match sup with
  | If sup ->
    check sup.cond (bool sup.span) &&
    check sup.then' Type &&
    check sup.else' Type &&
    constrain sub (eval (If sup))
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
  | Var sub, Var sup when sub.ident.index = sup.ident.index ->
    true
  | sub, sup ->

  (* Type *)

  match sup with
  | Type ->
    true
  | sup ->
  match sub with
  | Type ->
    false
  | sub ->

  (* Bot & Top *)

  match sub with
  | Bot ->
    true
  | _ ->
  match sup with
  | Top ->
    true
  | _ ->

  (* TODO: Others *)
  false

and check term type' =
  match term with
  | Bot ->
    constrain Top type'
  | Top ->
    constrain Type type'
  | Row row ->
    check row.type' Type &&
    constrain Type type'
  | Group group ->
    check group.body type'
  | If if' ->
    check if'.cond (bool if'.span) &&
    check if'.then' type' &&
    check if'.else' type'
  | Ascr ascr ->
    check ascr.body ascr.type' &&
    constrain ascr.type' type'
  | App app ->
    with_exis app.span (fun param_type ->
      with_var app.span param_type (fun param_ident ->
        let param = { span = app.span; ident = param_ident; type' = param_type } in
        let abs = Abs { span = app.span; param; body = type' } in
        check app.abs abs &&
        check app.arg param_type
      )
    )
  | Union union ->
    check union.left  Type &&
    check union.right Type &&
    constrain Type type'
  | Inter inter ->
    check inter.left  Type &&
    check inter.right Type &&
    constrain Type type'
  | Type ->
    false
  | _ ->

    (* TODO: Others *)
    false

and eval term =
  match term with
  | Var var ->
    Var var
  | Group group ->
    eval group.body
  | _ ->
    todo ()

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