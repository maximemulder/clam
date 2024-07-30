open Ast
open Ctx

open Ctx.ResultState
open Display
open Prim
open Subst

let todo () = failwith "TODO"

let tmp = Interval { span = Code.span_primitive; lower = Bot; upper = Top }

let rec constrain sub sup ctx =
  let res = constrain_inner sub sup ctx in
  let constrain = SubType { sub; sup } in
  match res with
  | Ok (subproofs, ctx) ->
    Ok ({ constrain; subproofs }, ctx)
  | Error constraints ->
    Error (constrain :: constraints)

and constrain_inner sub sup =
  (* Group *)

  match sub with
  | Group sub ->
    one (constrain sub.body sup)
  | sub ->
  match sup with
  | Group sup ->
    one (constrain sub sup.body)
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
    ]
  | sup ->

  (* Type ascription *)

  match sub with
  | Ascr sub ->
    all [
      check sub.body sub.type';
      constrain sub.body sup;
    ]
  | sub ->
  match sup with
  | Ascr sup ->
    all [
      check sup.body sup.type';
      constrain sub sup.body;
    ]
  | sup ->

  (* Unions & Intersection *)

  match sup with
  | Inter sup ->
    all [
      constrain sub sup.left;
      constrain sub sup.right;
    ]
  | sup ->
  match sub with
  | Inter sub ->
    any [
      constrain sub.left  sup;
      constrain sub.right sup;
    ]
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
      constrain sub sup.left;
      constrain sub sup.right;
    ];
  | sup ->
  (* TODO: Annoying cases *)

  (* Interval *)

  match sup with
  | Interval _ ->
    success
  | sup ->
  match sub with
  | Interval sub ->
    all [
      check sub.lower sup;
      check sub.upper sup;
    ]
  | sub ->

  (* Bot & Top *)

  match sub with
  | Bot ->
    success
  | sub ->
  match sup with
  | Top ->
    success
  | sup ->

  (* Row *)
  match sub, sup with
  | Row sub, Row sup when sub.tag = sup.tag ->
    one (constrain sub.type' sup.type')
  | sub, sup ->

  (* Application *)

  match sub with
  | App sub ->
    one (constrain (step_app sub) sup)
  | sub ->
  match sup with
  | App sup ->
    one (constrain sub (step_app sup))
  | _ ->

  (* TODO: Others *)
  fail

and check term type' ctx =
  let res = check_inner term type' ctx in
  let constrain = HasType { term; type' } in
  match res with
  | Ok (subproofs, ctx) ->
    Ok ({ constrain; subproofs }, ctx)
  | Error constraints ->
    Error (constrain :: constraints)

and check_inner term type' =
  match term with
  | Bot ->
    one (constrain Top type')
  | Top ->
    one (constrain tmp type')
  | Var var -> (
    match step type' with
    | Some type' ->
      one (check term type')
    | None -> ( (* TMP HACK FOR POC *)
      match type' with
      | Var type' ->
        let* val' = find_val type'.ident in
        (match val' with
        | Some val' ->
          one (check (Var var) (val'.value))
        | None ->
          failwith "Variable not in context")
      | Union union ->
        any [
          check (Var var) union.left;
          check (Var var) union.right;
        ]
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
        fail
    ))
  | Row row ->
    all [
      check row.type' tmp;
      constrain tmp type';
    ]
  | Record record ->
    let record_type = List.map (fun attr -> Row { span = record.span; tag = attr.tag; type' = (Interval { span = record.span; lower = attr.term; upper = attr.term }) }) record.attrs
      |> List.fold_left (fun row record_type' -> Inter { span = record.span; left = row; right = record_type' }) Bot in
    one (constrain record_type type')
  | Group group ->
    one (check group.body type')
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
    fail

and step term =
  match term with
  | Group group ->
    Some (step_group group)
  | If if' ->
    Some (step_if if')
  | App app ->
    Some (step_app app)
  | _ ->
    None

and step_group group =
  group.body

and step_if if' =
  match if'.cond with
  | Var var when var.ident = ident_true ->
    if'.then'
  | Var var when var.ident = ident_false ->
    if'.else'
  | cond ->
    let cond = Option.get (step cond) in
    If { if' with cond }

and step_app app =
  match app.abs with
  | Abs abs ->
    step_app_abs abs app.arg
  | abs ->
    let abs = Option.get (step abs) in
    App { app with abs }

and step_app_abs abs arg =
  match abs.param.ident with
  | Some ident ->
    subst ident arg abs.body
  | None ->
    failwith "Ill-formed type abstraction in `step_app_abs`"

and eval term =
  match step term with
  | Some term ->
    eval term
  | None ->
    term
