open Type
open Eval.Value

type primitive = {
  bind: Abt.bind_expr;
  name: string;
  type': type';
  value: value;
}

let make_primitives primitives =
  let (_, primitives) = List.fold_left (fun (id, res) (name, type', value) ->
    id - 1, { bind = { id; name }; name; type'; value } :: res
  ) (-1, []) primitives in
  primitives

let unary name param ret prim =
  (
    name,
    Lam { param; ret },
    VLam (VPrim (fun { value; out } -> prim value out))
  )

let binary name left right ret prim =
  (
    name,
    Lam { param = left; ret = Lam { param = right; ret }},
    VLam (VPrim (fun { value; _ } ->
      let left = value in
      VLam (VPrim (fun { value; _ } ->
        let right = value in
        prim left right
      ))
    ))
  )

let primitives = make_primitives [
  unary "print" Top Unit (fun value out ->
    let string = Eval.Display.display value in
    let _ = out string in
    VUnit);
  unary "__pos__" Int Int (fun value _ ->
    let int = value_int value in
    (VInt int));
  unary "__neg__" Int Int (fun value _ ->
    let int = value_int value in
    (VInt (-int)));
  unary "__not__" Bool Bool (fun value _ ->
    let bool = value_bool value in
    (VBool (not bool)));
  binary "__add__" Int Int Int (fun left right ->
    let left  = value_int left  in
    let right = value_int right in
    VInt (left + right));
  binary "__sub__" Int Int Int (fun left right ->
    let left  = value_int left  in
    let right = value_int right in
    VInt (left - right));
  binary "__mul__" Int Int Int (fun left right ->
    let left  = value_int left  in
    let right = value_int right in
    VInt (left * right));
  binary "__div__" Int Int Int (fun left right ->
    let left  = value_int left  in
    let right = value_int right in
    VInt (left / right));
  binary "__mod__" Int Int Int (fun left right ->
    let left  = value_int left  in
    let right = value_int right in
    VInt (left mod right));
  binary "__concat__" String String String (fun left right ->
    let left  = value_string left  in
    let right = value_string right in
    VString (left ^ right));
  binary "__eq__" Top Top Bool (fun left right ->
    VBool (compare left right));
  binary "__ne__" Top Top Bool (fun left right ->
    VBool (not (compare left right)));
  binary "__lt__" Int Int Bool (fun left right ->
    let left  = value_int left  in
    let right = value_int right in
    VBool (left < right));
  binary "__gt__" Int Int Bool (fun left right ->
    let left  = value_int left  in
    let right = value_int right in
    VBool (left > right));
  binary "__le__" Int Int Bool (fun left right ->
    let left  = value_int left  in
    let right = value_int right in
    VBool (left <= right));
  binary "__ge__" Int Int Bool (fun left right ->
    let left  = value_int left  in
    let right = value_int right in
    VBool (left >= right));
  binary "__and__" Bool Bool Bool (fun left right ->
    let left  = value_bool left  in
    let right = value_bool right in
    VBool (left && right));
  binary "__or__" Bool Bool Bool (fun left right ->
    let left  = value_bool left  in
    let right = value_bool right in
    VBool (left || right));
]

let binds = List.map (fun primitive -> primitive.name, primitive.bind) primitives

let types = List.map (fun primitive -> primitive.bind, primitive.type') primitives

let values = List.map (fun primitive -> primitive.bind, primitive.value) primitives
