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
  "print", TypeAbsExpr { pos; param = top; body = unit }, fun { value; out } ->
    let string = RuntimeDisplay.display value in
    let _ = out string in
  VUnit;
]

let binds = List.map (fun primitive -> primitive.name, primitive.bind) primitives

let types = List.map (fun primitive -> primitive.bind, primitive.type') primitives

let values = List.map (fun primitive -> primitive.bind, primitive.value) primitives
