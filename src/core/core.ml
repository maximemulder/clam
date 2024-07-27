open Ast
open Ctx

open Ctx.M
open Ctx.Monad
open Display

let todo () = failwith "TODO"

let bool span =
  Var { span; ident = {name = "Bool"; index = 0} }

let true' span =
  Var { span; ident = {name = "True"; index = 0} }

let false' span =
  Var { span; ident = {name = "False"; index = 0} }

let rec constrain sub sup ctx =
  let res = constrain_inner sub sup ctx in
  match res with
  | Ok ((), ctx) ->
    Ok ((), ctx)
  | Error constraints ->
    Error ((SubType { sub; sup }) :: constraints)

and constrain_inner sub sup =
  let failure = lift (fail []) in

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
    all [
      check sub.cond (bool sub.span);
      check sub.then' Type;
      check sub.else' Type;
      constrain (step_if sub) sup;
    ]
  | sub ->
  match sup with
  | If sup ->
    all [
      check sup.cond (bool sup.span);
      check sup.then' Type;
      check sup.else' Type;
      constrain sub (step_if sup);
    ];
  | sup ->

  (* Type ascription *)

  match sub with
  | Ascr sub ->
    all [
      check sub.body sub.type';
      constrain sub.body sup;
    ];
  | sub ->
  match sup with
  | Ascr sup ->
    all [
      check sup.body sup.type';
      constrain sub sup.body;
    ];
  | sup ->

  (* Unions & Intersection *)

  match sub with
  | Union sub ->
    all [
      constrain sub.left  sup;
      constrain sub.right sup;
    ];
  | sub ->
  match sup with
  | Inter sup ->
    all [
      constrain sub sup.left;
      constrain sub sup.right;
    ];
  | sup ->
  (* TODO: Annoying cases *)

  (* Variable *)

  match sub, sup with
  | Var sub, Var sup when sub.ident.name = sup.ident.name ->
    return ()
  | sub, sup ->

  (* Type *)

  match sup with
  | Type ->
    return ()
  | sup ->
  match sub with
  | Type ->
    failure
  | sub ->

  (* Bot & Top *)

  match sub with
  | Bot ->
    return ()
  | _ ->
  match sup with
  | Top ->
    return ()
  | _ ->

  (* TODO: Others *)
  failure

and check term type' ctx =
  let res = check_inner term type' ctx in
  match res with
  | Ok ((), ctx) ->
    Ok ((), ctx)
  | Error constraints ->
    Error ((InType { term; type' }) :: constraints)

and check_inner term type' =
  let failure = lift (fail []) in
  match term with
  | Bot ->
    constrain Top type'
  | Top ->
    constrain Type type'
  | Var var -> (
    match step type' with
    | Some type' ->
      check term type'
    | None -> ( (* TMP HACK FOR POC *)
      match type' with
      | Var type' when var.ident.name = "True" && type'.ident.name = "Bool" ->
        return ()
      | Inter inter ->
        all [
          check (Var var) inter.left;
          check (Var var) inter.right;
        ]
      | Type ->
        return ()
      | _ ->
        failure
    ))
  | Row row ->
    all [
      check row.type' Type;
      constrain Type type';
    ]
  | Group group ->
    check group.body type'
  | If if' ->
    all [
      check if'.cond (bool if'.span);
      check if'.then' type';
      check if'.else' type';
    ]
  | Ascr ascr ->
    all [
      check ascr.body ascr.type';
      constrain ascr.type' type';
    ]
  | App app ->
    with_exis app.span (fun param_type ->
      with_var param_type (fun param_ident ->
        let param = { span = app.span; ident = Some param_ident; type' = Some param_type } in
        let abs = Abs { span = app.span; param; body = type' } in
        all [
          check app.abs abs;
          check app.arg param_type;
        ]
      )
    )
  | Union union ->
    all [
      check union.left  Type;
      check union.right Type;
      constrain Type type';
    ];
  | Inter inter ->
    all [
      check inter.left  Type;
      check inter.right Type;
      constrain Type type';
    ]
  | Type ->
    failure
  | _ ->

    (* TODO: Others *)
    failure

and step term =
  match term with
  | Group group ->
    Some group.body
  | If if' ->
    Some (step_if if')
  | _ -> None

and step_if if' =
  match if'.cond with
  | Var { ident = { name = "True"; _}; _ } ->
    if'.then'
  | Var { ident = { name = "False"; _}; _} ->
    if'.else'
  | cond ->
    If { if' with cond }

and eval term =
  match step term with
  | Some term ->
    eval term
  | None ->
    term
