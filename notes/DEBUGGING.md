# Subtyping

## Print stack trace
```
export OCAMLRUNPARAM=b;
```

## Print subtyping

```
and indent_n = ref 0

and indent n =
  if n = 0 then "" else "  " ^ indent (n - 1)

and indent_with f =
  indent_n := indent_n.contents + 1;
  let res = f () in
  indent_n := indent_n.contents - 1;
  res

and isa c l r =
  let res = indent_with (fun _ -> isa2 c l r) in
  let tab = indent indent_n.contents in
  print_endline(tab
    ^ (TypeDisplay.display l) ^ " < " ^ (TypeDisplay.display r)
    ^ " |- " ^ (TypeDisplay.display_context c)
    ^ "   " ^ (string_of_bool res));
  res

and is c l r =
  let res = indent_with (fun _ -> is2 c l r) in
  let tab = indent indent_n.contents in
  print_endline(tab
    ^ (TypeDisplay.display l) ^ " = " ^ (TypeDisplay.display r)
    ^ " |- " ^ (TypeDisplay.display_context c)
    ^ "   " ^ (string_of_bool res));
  res

and meet c l r =
  let res = indent_with (fun _ -> meet_2 c l r) in
  let tab = indent indent_n.contents in
  print_endline(tab
    ^ (TypeDisplay.display l) ^ " ^ " ^ (TypeDisplay.display r)
    ^ " |- " ^ (TypeDisplay.display_context c)
    ^ "   " ^ (string_of_bool res));
```

## Print inference

```
if not (sub = TypePrimitive.bot) && not (sup = TypePrimitive.top) then
  print_endline("constrain " ^ TypeDisplay.display sub ^ "  <  " ^ TypeDisplay.display sup ^ "")
else
  ();

let* lower = get_lower_bound param_bind in
let* upper = get_upper_bound param_bind in
print_endline("param `" ^ TypeDisplay.display lower ^ "` < `" ^ TypeDisplay.display upper ^ "`");
let* lower = get_lower_bound ret_bind in
let* upper = get_upper_bound ret_bind in
print_endline("ret `" ^ TypeDisplay.display lower ^ "` < `" ^ TypeDisplay.display upper ^ "`");

print_endline("");
print_endline("infer def " ^ def.bind.name);
```