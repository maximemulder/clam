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
    VLam (VPrim (fun { expr; value; out } -> prim expr value out))
  )

let binary name left right ret prim =
  (
    name,
    Lam { param = left; ret = Lam { param = right; ret }},
    VLam (VPrim (fun { value; _ } ->
      let left = value in
      VLam (VPrim (fun { expr; value; _ } ->
        let right = value in
        prim expr left right
      ))
    ))
  )

let primitives = make_primitives [
  unary "print" Top Unit (fun _ value out ->
    let string = Eval.Display.display value in
    let _ = out string in
    VUnit);
  unary "__pos__" Int Int (fun e value _ ->
    let int = value_int e value in
    (VInt int));
  unary "__neg__" Int Int (fun e value _ ->
    let int = value_int e value in
    (VInt (-int)));
  unary "__not__" Bool Bool (fun e value _ ->
    let bool = value_bool e value in
    (VBool (not bool)));
  binary "__add__" Int Int Int (fun e left right ->
    let left  = value_int e left  in
    let right = value_int e right in
    VInt (left + right));
  binary "__sub__" Int Int Int (fun e left right ->
    let left  = value_int e left  in
    let right = value_int e right in
    VInt (left - right));
  binary "__mul__" Int Int Int (fun e left right ->
    let left  = value_int e left  in
    let right = value_int e right in
    VInt (left * right));
  binary "__div__" Int Int Int (fun e left right ->
    let left  = value_int e left  in
    let right = value_int e right in
    VInt (left / right));
  binary "__mod__" Int Int Int (fun e left right ->
    let left  = value_int e left  in
    let right = value_int e right in
    VInt (left mod right));
  binary "__concat__" String String String (fun e left right ->
    let left  = value_string e left  in
    let right = value_string e right in
    VString (left ^ right));
  binary "__eq__" Top Top Bool (fun _ left right ->
    VBool (compare left right));
  binary "__ne__" Top Top Bool (fun _ left right ->
    VBool (not (compare left right)));
  binary "__lt__" Int Int Bool (fun e left right ->
    let left  = value_int e left  in
    let right = value_int e right in
    VBool (left < right));
  binary "__gt__" Int Int Bool (fun e left right ->
    let left  = value_int e left  in
    let right = value_int e right in
    VBool (left > right));
  binary "__le__" Int Int Bool (fun e left right ->
    let left  = value_int e left  in
    let right = value_int e right in
    VBool (left <= right));
  binary "__ge__" Int Int Bool (fun e left right ->
    let left  = value_int e left  in
    let right = value_int e right in
    VBool (left >= right));
  binary "__and__" Bool Bool Bool (fun e left right ->
    let left  = value_bool e left  in
    let right = value_bool e right in
    VBool (left && right));
  binary "__or__" Bool Bool Bool (fun e left right ->
    let left  = value_bool e left  in
    let right = value_bool e right in
    VBool (left || right));
]

let binds = List.map (fun primitive -> primitive.name, primitive.bind) primitives

let types = List.map (fun primitive -> primitive.bind, primitive.type') primitives

let values = List.map (fun primitive -> primitive.bind, primitive.value) primitives
