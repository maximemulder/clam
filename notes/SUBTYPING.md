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
  if n = 0 then "" else "  " ^ repeat (n - 1)

and indent_with f
  indent_n := indent.contents + 1;
  let res = isa2 l r c in
  indent_n := indent.contents - 1;
  res

and isa l r c =
  indent_with isa2 l r c in
  let tab = indent indent_n.contents in
  print_endline(tab ^ (TypingDisplay.display (Typing.normalize l)) ^ " : " ^ (TypingDisplay.display (Typing.normalize r)));
  print_endline(tab ^ (TypingDisplay.display l) ^ " : " ^ (TypingDisplay.display r) ^ "   " ^ (string_of_bool res));
  res
```

## TODO

- Finish `meet` function, check if `join` is complete
- Type variables with `Bot` as a bound should act similar to `Bot`.
- More testing (calls and above improvements)
- Subtyping uses substitution for type applications, but what if it simply added variables to the context ?
