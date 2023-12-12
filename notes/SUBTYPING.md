# Subtyping

## Unions and intersections

The order of the subtyping pattern matching is important. Notably, the left union should appear before the right union and intersection, which should appear before the left intersection.

## Debugging subtyping

The following code can be used to debug subtyping by printing the subtyping steps. The `isa` function must be renamed to `isa2`.

```
and indent_n = ref 0

and indent n =
  if n = 0 then "" else "  " ^ indent (n - 1)

and indent_with f =
  indent_n := indent_n.contents + 1;
  let res = f () in
  indent_n := indent_n.contents - 1;
  res

and isa l r =
  let res = indent_with (fun _ -> isa2 l r) in
  let tab = indent indent_n.contents in
  print_endline(tab ^ (TypingDisplay.display (normalize l)) ^ " : " ^ (TypingDisplay.display (normalize r)));
  print_endline(tab ^ (TypingDisplay.display l) ^ " : " ^ (TypingDisplay.display r) ^ "   " ^ (string_of_bool res));
  res

and is l r =
  let res = indent_with (fun _ -> is2 l r) in
  let tab = indent indent_n.contents in
  print_endline(tab ^ (TypingDisplay.display (normalize l)) ^ " = " ^ (TypingDisplay.display (normalize r)));
  print_endline(tab ^ (TypingDisplay.display l) ^ " = " ^ (TypingDisplay.display r) ^ "   " ^ (string_of_bool res));
  res
```

## Type representation

It may be a good idea to create a (OCaml) type for normalized (Clam) types.

All types would always be in their DNF form. And applications would always be applied when possible.

Example:
```
type type' =
  type_literal list list

type_literal =
  | Int type_int
  | Tuple type_tuple
  ...

type_int {
  pos: pos;
}

type_tuple {
  pos: pos;
  elems: type'
}
```

## TODO

- Finish higher order subtyping
- Add more tests
