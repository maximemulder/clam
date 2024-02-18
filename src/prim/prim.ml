open Eval.Value
open Clam

type primitive = {
  bind: Abt.bind_expr;
  name: string;
  type': Type.type';
  value: value;
}

let make_primitives primitives =
  let (_, primitives) = List.fold_left (fun (id, res) (name, type', value) ->
    id - 1, { bind = { id; name }; name; type'; value } :: res
  ) (-1, []) primitives in
  primitives

let unary name param ret prim =
  (name,
  Type.lam param ret,
  VLam (VPrim (fun { value; out } -> prim value out)))

let binary name left right ret prim =
  (
    name,
    Type.lam left (Type.lam right ret),
    VLam (VPrim (fun { value; _ } ->
      let left = value in
      VLam (VPrim (fun { value; _ } ->
        let right = value in
        prim left right
      ))
    ))
  )

let primitives = make_primitives [
  unary "print" Type.top Type.unit (fun value out ->
    let string = Eval.Display.display value in
    let _ = out string in
    VUnit);
  unary "__pos__" Type.int Type.int (fun value _ ->
    let int = value_int value in
    (VInt int));
  unary "__neg__" Type.int Type.int (fun value _ ->
    let int = value_int value in
    (VInt (-int)));
  unary "__not__" Type.bool Type.bool (fun value _ ->
    let bool = value_bool value in
    (VBool (not bool)));
  binary "__add__" Type.int Type.int Type.int (fun left right ->
    let left  = value_int left  in
    let right = value_int right in
    VInt (left + right));
  binary "__sub__" Type.int Type.int Type.int (fun left right ->
    let left  = value_int left  in
    let right = value_int right in
    VInt (left - right));
  binary "__mul__" Type.int Type.int Type.int (fun left right ->
    let left  = value_int left  in
    let right = value_int right in
    VInt (left * right));
  binary "__div__" Type.int Type.int Type.int (fun left right ->
    let left  = value_int left  in
    let right = value_int right in
    VInt (left / right));
  binary "__mod__" Type.int Type.int Type.int (fun left right ->
    let left  = value_int left  in
    let right = value_int right in
    VInt (left mod right));
  binary "__concat__" Type.string Type.string Type.string (fun left right ->
    let left  = value_string left  in
    let right = value_string right in
    VString (left ^ right));
  binary "__eq__" Type.top Type.top Type.bool (fun left right ->
    VBool (compare left right));
  binary "__ne__" Type.top Type.top Type.bool (fun left right ->
    VBool (not (compare left right)));
  binary "__lt__" Type.int Type.int Type.bool (fun left right ->
    let left  = value_int left  in
    let right = value_int right in
    VBool (left < right));
  binary "__gt__" Type.int Type.int Type.bool (fun left right ->
    let left  = value_int left  in
    let right = value_int right in
    VBool (left > right));
  binary "__le__" Type.int Type.int Type.bool (fun left right ->
    let left  = value_int left  in
    let right = value_int right in
    VBool (left <= right));
  binary "__ge__" Type.int Type.int Type.bool (fun left right ->
    let left  = value_int left  in
    let right = value_int right in
    VBool (left >= right));
  binary "__and__" Type.bool Type.bool Type.bool (fun left right ->
    let left  = value_bool left  in
    let right = value_bool right in
    VBool (left && right));
  binary "__or__" Type.bool Type.bool Type.bool (fun left right ->
    let left  = value_bool left  in
    let right = value_bool right in
    VBool (left || right));
]

let binds = List.map (fun primitive -> primitive.name, primitive.bind) primitives

let types = List.map (fun primitive -> primitive.bind, primitive.type') primitives

let values = List.map (fun primitive -> primitive.bind, primitive.value) primitives
