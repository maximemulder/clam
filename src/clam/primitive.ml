open Model
open RuntimeValue

let pos = {
  Lexing.pos_fname = "primitives.clam";
  Lexing.pos_lnum = 0;
  Lexing.pos_bol = 0;
  Lexing.pos_cnum = 0;
}

let top    = TypeTop    { pos }
let bot    = TypeBot    { pos }
let unit   = TypeUnit   { pos }
let bool   = TypeBool   { pos }
let int    = TypeInt    { pos }
let char   = TypeChar   { pos }
let string = TypeString { pos }

type primitive = {
  bind: bind_expr;
  name: string;
  type': type';
  value: value;
}

let make_primitives primitives =
  let (_, primitives) = List.fold_left (fun (id, res) (name, type', prim) ->
    id - 1, { bind = BindExprPrim id; name; type'; value = VExprAbs (VPrim prim) } :: res
  ) (-1, []) primitives in
  primitives

let primitives = make_primitives [
  ("print", TypeAbsExpr { pos; param = top; body = unit }, fun { value; out } ->
    let string = RuntimeDisplay.display value in
    let _ = out string in
    VUnit);
  ("__pos__", TypeAbsExpr { pos; param = int; body = int }, fun { value; _ } ->
    let int = value_int value in
    (VInt int));
  ("__neg__", TypeAbsExpr { pos; param = int; body = int }, fun { value; _ } ->
    let int = value_int value in
    (VInt (-int)));
  ("__not__", TypeAbsExpr { pos; param = bool; body = bool }, fun { value; _ } ->
    let bool = value_bool value in
    (VBool (not bool)));
]

let binds = List.map (fun primitive -> primitive.name, primitive.bind) primitives

let types = List.map (fun primitive -> primitive.bind, primitive.type') primitives

let values = List.map (fun primitive -> primitive.bind, primitive.value) primitives
