# Subtyping

## Unions and intersections

Subtyping for unions and intersections is inspired by the following article:
https://dl.acm.org/doi/abs/10.1145/3276482

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

and isa l r c =
  let res = indent_with (fun _ -> isa2 l r c) in
  let tab = indent indent_n.contents in
  print_endline(tab ^ (TypingDisplay.display (normalize l)) ^ " : " ^ (TypingDisplay.display (normalize r)));
  print_endline(tab ^ (TypingDisplay.display l) ^ " : " ^ (TypingDisplay.display r) ^ "   " ^ (string_of_bool res));
  res
```

## TODO

THIS LIST IS NOT UP-TO-DATE
- Finish `meet` function, check if `join` is complete
- Type variables with `Bot` as a bound should act similar to `Bot`.
- More testing (calls and above improvements)
- Subtyping uses substitution for type applications, but what if it simply added variables to the context ?
- Check the interactions of a type variable bound to a parameter whose name is overriden by another parameter.
- Check this case `case (abs_expr_type_1 ("A", top) (fun a -> (inter a int))) (abs_expr_type_1 ("B", top) (fun b -> b)) (abs_expr_type_1 ("A", top) (fun a -> (inter a int)));`
