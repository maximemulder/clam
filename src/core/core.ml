open Ast
open Ctx

open Ctx.M
open Ctx.Monad
open Display
open Prim

let todo () = failwith "TODO"

let tmp = Interval { span = Code.span_primitive; lower = Bot; upper = Top }

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
      check sub.cond (var_bool sub.span);
      check sub.then' tmp;
      check sub.else' tmp;
      constrain (step_if sub) sup;
    ]
  | sub ->
  match sup with
  | If sup ->
    all [
      check sup.cond (var_bool sup.span);
      check sup.then' tmp;
      check sup.else' tmp;
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

  match sup with
  | Inter sup ->
    all [
      constrain sub sup.left;
      constrain sub sup.right;
    ];
  | sup ->
  match sub with
  | Inter sub ->
    any [
      constrain sub.left  sup;
      constrain sub.right sup;
    ];
  | sub ->
  match sub with
  | Union sub ->
    all [
      constrain sub.left  sup;
      constrain sub.right sup;
    ];
  | sub ->
  match sup with
  | Union sup ->
    any [
      constrain sub  sup.left;
      constrain sub sup.right;
    ];
  | sup ->
  (* TODO: Annoying cases *)

  (* Variable *)

  match sub, sup with
  | Var sub, Var sup when sub.ident.name = sup.ident.name ->
    return ()
  | sub, sup ->

  (* Interval *)

  match sup with
  | Interval _ ->
    return ()
  | sup ->
  match sub with
  | Interval _ ->
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
    Error ((HasType { term; type' }) :: constraints)

and check_inner term type' =
  let failure = lift (fail []) in
  match term with
  | Bot ->
    constrain Top type'
  | Top ->
    constrain tmp type'
  | Var var -> (
    match step type' with
    | Some type' ->
      check term type'
    | None -> ( (* TMP HACK FOR POC *)
      match type' with
      | Var type' when (var.ident = ident_true || var.ident = ident_false) && type'.ident = ident_bool ->
        return ()
      | Inter inter ->
        all [
          check (Var var) inter.left;
          check (Var var) inter.right;
        ]
      | Interval interval ->
        all [
          constrain interval.lower (Var var);
          constrain (Var var) interval.upper;
        ]
      | _ ->
        failure
    ))
  | Row row ->
    all [
      check row.type' tmp;
      constrain tmp type';
    ]
  | Group group ->
    check group.body type'
  | If if' ->
    all [
      check if'.cond (var_bool if'.span);
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
      check union.left  tmp;
      check union.right tmp;
      constrain tmp type';
    ];
  | Inter inter ->
    all [
      check inter.left  tmp;
      check inter.right tmp;
      constrain tmp type';
    ]
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
  | Var var when var.ident = ident_true ->
    if'.then'
  | Var var when var.ident = ident_false ->
    if'.else'
  | cond ->
    If { if' with cond }

and eval term =
  match step term with
  | Some term ->
    eval term
  | None ->
    term
