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

## Print inference state

```
let print_state state =
  List.sort (fun a b -> compare a.level b.level) state.vars
  |> List.map (fun a -> string_of_int a.level
    ^ " " ^ a.bind.name
    ^ ": " ^ TypeDisplay.display a.lower
    ^ " < " ^ TypeDisplay.display a.upper)
  |> String.concat ", "
  |> print_endline, state
```

## Print inference constraints

```
if sub <> Type.bot && sup <> Type.top then
  print_endline("constrain " ^ TypeDisplay.display sub ^ " < " ^ TypeDisplay.display sup);
```

## Print inference definitions

```
  let state = check_defs state in
  let types = List.filter (fun (e: entry_expr) -> not(List.exists (fun (p: entry_expr) -> p.bind.name = e.bind.name) primitives)) state.exprs in
  List.iter (fun (e: entry_expr) -> print_endline(e.bind.name ^ ": " ^ TypeDisplay.display e.type')) types
```
