open Ast
open Ctx
open Display

open Ctx.ResultState
open Prim
open Subst
open Map_ctx

let todo () = failwith "TODO"

let tmp = Range { span = Code.span_primitive; lower = Bot; upper = Top }

let rec sub_type sub sup ctx =
  let res = sub_type_inner sub sup ctx in
  let constrain = SubType { sub; sup } in
  match res with
  | Ok (subproofs, ctx) ->
    Ok ({ constrain; subproofs }, ctx)
  | Error constraints ->
    Error (constrain :: constraints)

and sub_type_inner sub sup =
  (* Variable *)
  match sub, sup with
  | Var sub, Var sup when sub.ident = sup.ident ->
    success
  | sub, sup ->

  (* Group *)

  match sub with
  | Group sub ->
    one (sub_type sub.term sup)
  | sub ->
  match sup with
  | Group sup ->
    one (sub_type sub sup.term)
  | sup ->

  (* If *)

  match sub with
  | If sub ->
    all [
      has_type sub.cond (var_bool sub.span);
      has_type sub.then' tmp;
      has_type sub.else' tmp;
      sub_type (step_if sub) sup;
    ]
  | sub ->
  match sup with
  | If sup ->
    all [
      has_type sup.cond (var_bool sup.span);
      has_type sup.then' tmp;
      has_type sup.else' tmp;
      sub_type sub (step_if sup);
    ]
  | sup ->

  (* Type ascription *)

  match sub with
  | Ascr sub ->
    all [
      has_type sub.term sub.type';
      sub_type sub.term sup;
    ]
  | sub ->
  match sup with
  | Ascr sup ->
    all [
      has_type sup.term sup.type';
      sub_type sub sup.term;
    ]
  | sup ->

  (* Unions & Intersection *)

  match sup with
  | Inter sup ->
    all [
      sub_type sub sup.left;
      sub_type sub sup.right;
    ]
  | sup ->
  match sub with
  | Inter sub ->
    any [
      sub_type sub.left  sup;
      sub_type sub.right sup;
    ]
  | sub ->
  match sub with
  | Union sub ->
    all [
      sub_type sub.left  sup;
      sub_type sub.right sup;
    ];
  | sub ->
  match sup with
  | Union sup ->
    any [
      sub_type sub sup.left;
      sub_type sub sup.right;
    ];
  | sup ->
  (* TODO: Annoying cases *)

  (* Interval *)

  match sub, sup with
  | Range sub, Range sup ->
    all [
      sub_type sup.lower sub.lower;
      sub_type sub.upper sup.upper;
    ]
  | sub, sup ->

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
  | Record sub, Record sup ->
    all (List.map (fun (sup: record_attr) ->
      match List.find_opt (fun (sub: record_attr) -> sub.tag == sup.tag) sub.attrs with
      | Some sub ->
        sub_type sub.term sup.term
      | None ->
        fail
    ) sup.attrs)
  | sub, sup ->

  (* Application *)

  match sub with
  | App sub ->
    one (sub_type (step_app sub) sup)
  | sub ->
  match sup with
  | App sup ->
    one (sub_type sub (step_app sup))
  | _ ->

  (* TODO: Others *)
  fail

and has_type term type' ctx =
  let res = has_type_inner term type' ctx in
  let constrain = HasType { term; type' } in
  match res with
  | Ok (subproofs, ctx) ->
    Ok ({ constrain; subproofs }, ctx)
  | Error constraints ->
    Error (constrain :: constraints)

and has_type_inner term type' =
  match term with
  | Bot ->
    has_type_bot Bot type'
  | Top ->
    has_type_top Top type'
  | Var var -> (
    match step type' with
    | Some type' ->
      one (has_type term type')
    | None -> ( (* TMP HACK FOR POC *)
      match type' with
      | Var type' ->
        let* val' = find_val type'.ident in
        (match val' with
        | Some val' ->
          one (has_type (Var var) (val'.value))
        | None ->
          failwith ("Variable not in context " ^ var.ident.name ^" " ^ type'.ident.name))
      | Union union ->
        any [
          has_type (Var var) union.left;
          has_type (Var var) union.right;
        ]
      | Inter inter ->
        all [
          has_type (Var var) inter.left;
          has_type (Var var) inter.right;
        ]
      | Range range ->
        all [
          sub_type range.lower (Var var);
          sub_type (Var var) range.upper;
        ]
      | _ ->
        fail
    ))
  | Record record ->
    has_type_record record type'
  | Group group ->
    one (has_type group.term type')
  | If if' ->
    all [
      has_type if'.cond (var_bool if'.span);
      has_type if'.then' type';
      has_type if'.else' type';
    ]
  | Ascr ascr ->
    all [
      has_type ascr.term ascr.type';
      sub_type ascr.type' type';
    ]
  | App app ->
    with_exis app.span (fun param_type ->
      with_var param_type (fun param_ident ->
        let param: param = { ident = Some param_ident; type' = Some param_type } in
        let abs = Abs { span = app.span; param; body = type' } in
        all [
          has_type app.abs abs;
          has_type app.arg param_type;
        ]
      )
    )
  | Union union ->
    all [
      has_type union.left  tmp;
      has_type union.right tmp;
      sub_type tmp type';
    ];
  | Inter inter ->
    all [
      has_type inter.left  tmp;
      has_type inter.right tmp;
      sub_type tmp type';
    ]
  | _ ->

    (* TODO: Others *)
    fail

and has_type_bot bot type' =
  match type' with
  | Range range ->
    all [
      sub_type range.lower bot;
      sub_type bot range.upper;
    ]
  | type' ->
  match step type' with
  | Some type' ->
    one (has_type bot type')
  | None ->
    fail

and has_type_top top type' =
  match type' with
  | Range range ->
    all [
      sub_type range.lower top;
      sub_type top range.upper;
    ]
  | type' ->
  match step type' with
  | Some type' ->
    one (has_type top type')
  | None ->
    fail

and has_type_record record type' =
  match type' with
  | Record record_type ->
    all (List.map (fun attr ->
      has_type_record_attr attr record_type
    ) record.attrs)
  | type' ->
  match step type' with
  | Some type' ->
    one (has_type (Record record) type')
  | _ ->
    fail

  (* let* record_type = infer_record record in
  list_all (sub_type record_type type') *)

and has_type_record_attr attr record_type =
  match List.find_opt (fun (attr_type: record_attr) -> attr.tag = attr_type.tag) record_type.attrs with
  | Some attr_type ->
    has_type attr.term attr_type.term
  | None ->
    success_proof {
      constrain = HasType { term = (Record { span = attr.span; attrs = [attr] }); type' = (Record record_type) };
      subproofs = [];
    }

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
  group.term

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
