type config = {
  show_ast    : Util.writer option;
  show_kinds  : Util.writer option;
  show_types  : Util.writer option;
  show_values : Util.writer option;
  print_out   : Util.writer;
  print_err   : Util.writer;
}

let parse code config =
  let ast = Parser.parse code in
  (match config.show_ast with
  | Some show_ast -> show_ast(Ast.display_program ast);
  | None -> ());
  ast

let desugar ast =
  Sugar.desugar ast Prim.binds

let type_check abt config =
  let kinds, types = Infer.check abt Prim.types in
  (match config.show_kinds with
  | Some show_kinds ->
    List.iter (fun (name, kind) ->
      show_kinds(name ^ " :: " ^ Type.Kind.display kind)
    ) kinds
  | None -> ());
  match config.show_types with
  | Some show_types ->
    List.iter (fun (def, type') ->
      show_types((def: Abt.bind_expr).name ^ ": " ^ Type.display type')
    ) types
  | None -> ()

let eval abt config =
  let main = (match List.find_opt (fun (def: Abt.def_expr) -> def.bind.name = "main") abt.Abt.exprs with
  | Some main -> main
  | None -> Error.handle_main () config.print_err
  ) in
  Eval.eval main abt.exprs Prim.values config.print_out

let run code config =
  try
    let ast = parse code config in
    let abt = desugar ast in
    type_check abt config;
    eval abt config
  with
  | Parser.Error error ->
    Error.handle_parser error config.print_err
  | Sugar.Error  error ->
    Error.handle_sugar  error config.print_err
  | Type.Error   error ->
    Error.handle_type   error config.print_err
  | Infer.Error  error ->
    Error.handle_infer  error config.print_err
  | Eval.Error   error ->
    Error.handle_eval   error config.print_err

  ;;
  open Type.Context

  let inline v _ = v

  let id v = v

  (* Types *)

  include Type

  (* TODO: Adopt new bind system once typing is refactored *)

  let bind name =
    { Abt.name }

  let record attrs =
    let attrs = attrs
      |> List.map (fun (label, type') -> (label, { label; type' }))
      |> List.to_seq
      |> Util.NameMap.of_seq in
    base (Record { attrs })

  let univ name bound ret =
    let bind = bind name in
    let param: Type.param = { bind; lower = bot; upper = bound } in
    let ret = ret (var bind) in
    base (Univ { param; ret })

  let abs name bound body =
    let bind = bind name in
    let param: Type.param = { bind; lower = bot; upper = bound } in
    let body = body (var bind) in
    base (Abs { param; body })

  let a = bind "A"
  let b = bind "B"
  let c = bind "C"
  let d = bind "D"
  let e = bind "E"
  let f = bind "F"
  let z = bind "Z"

  let ea = bind "EA"
  let fa = bind "FA"

  let ctx = {
    id = 0;
    level = 0;
    freshs = [];
    rigids = [
      { bind = a; lower = bot; upper = top };
      { bind = b; lower = bot; upper = top };
      { bind = c; lower = bot; upper = top };
      { bind = d; lower = bot; upper = top };
      { bind = e; lower = bot; upper = top };
      { bind = f; lower = bot; upper = top };
      { bind = z; lower = bot; upper = top };
      { bind = ea; lower = bot; upper = var a };
      { bind = fa; lower = bot; upper = var a };
    ]
  }

  let union types =
    let types = List.map (fun type' -> type'.dnf) types in
    let types = List.flatten types in
    { dnf = types }

  let inter types =
    Util.list_reduce (fun left right -> { dnf = Util.list_product (fun left right -> left @ right) left.dnf right.dnf }) types

  let a = var a
  let b = var b
  let c = var c
  let d = var d
  let e = var e
  let f = var f
  let z = var z
  let ea = var ea
  let fa = var fa

;;

let _ =
  System.isa (univ "T" top (inline a)) (univ "U" top (inline a)) ctx
