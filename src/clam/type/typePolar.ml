open TypeState

type pol = Pos | Neg

let inv pol =
  match pol with
  | Pos -> Neg
  | Neg -> Pos

let rec occurs (type': Type.type') bind =
  list_any (fun type' -> occurs_inter type' bind) type'.union

and occurs_inter (inter: Type.inter) bind =
  list_any (fun type' -> occurs_base type' bind) inter.inter

and occurs_base (type': Type.base) bind =
  match type' with
  | Var var ->
    occurs_bind var bind
  | AbsExpr abs ->
    let* param = occurs abs.param bind in
    let* ret = occurs abs.ret bind in
    return (param || ret)
  | _ ->
    return false

and occurs_bind var bind =
  if var.bind == bind then
    return true
  else
  let* lower = get_lower_bound var.bind in
  let* upper = get_upper_bound var.bind in
  let* lower = occurs lower bind in
  let* upper = occurs upper bind in
  return (lower || upper)

let occurs_in_lower_vars bind state =
  let level, _ = get_level bind state in
  List.filter (fun (entry: entry_bounds) -> entry.level < level) state.bounds
  |> List.map (fun entry -> entry.bind)
  |> List.exists (fun var -> fst (occurs_bind { bind = var } bind state)), state

(* TEST *)

let rec inline (type': Type.type') var pol =
  inline_union type' var pol

and inline_union union var pol =
  let* ctx = get_context in
  let* types = map_list (fun type' -> inline_inter type' var pol) union.union in
  return (Utils.list_reduce (TypeSystem.join ctx) types)

and inline_inter inter var pol =
  let* ctx = get_context in
  let* types = map_list (fun type' -> inline_base type' var pol) inter.inter in
  return (Utils.list_reduce (TypeSystem.meet ctx) types)

and inline_base type' var' pol =
  match type' with
  | Type.Var var when var.bind == var' -> (
    match pol with
    | Pos ->
      let* bound = get_upper_bound var.bind in
      let* cond = occurs_in_lower_vars var.bind in
      if not cond then
        inline bound var' Pos
      else
        return bound
    | Neg ->
      let* bound = get_lower_bound var.bind in
      let* cond = occurs_in_lower_vars var.bind in
      if not cond then
        inline bound var' Neg
      else
        return bound
  )
  | AbsExpr abs ->
    let* param = inline abs.param var' (inv pol) in
    let* ret = inline abs.ret var' pol in
    return (Type.base (Type.AbsExpr { param; ret }))
  | _ ->
    return (Type.base type')

let rec appears (type': Type.type') var pol =
  appears_union type' var pol

and appears_union union var pol =
  let types = List.map (fun type' -> appears_inter type' var pol) union.union in
  Utils.list_reduce (List.append) types

and appears_inter inter var pol =
  let types = List.map (fun type' -> appears_base type' var pol) inter.inter in
  Utils.list_reduce (List.append) types

and appears_base type' var pol =
  match type' with
  | Type.Var var' when var'.bind == var ->
    [pol]
  | AbsExpr abs ->
    let param = appears abs.param var (inv pol) in
    let ret = appears abs.ret var pol in
    List.append param ret
  | _ ->
    []

let inline_state bind state =
  let bounds = List.map (fun entry -> { entry with
    upper = fst(inline entry.upper bind Neg state);
    lower = fst(inline entry.lower bind Pos state);
  }) state.bounds in
  (), { state with bounds }

(* Returns variables that appear in this type and are equal or higher to the current state level *)
let get_variables type' state =
  List.filter (fun (entry: entry_bounds) -> entry.level >= state.level) state.bounds
  |> List.map (fun entry -> entry.bind)
  |> List.filter (fun bind -> TypeUtils.contains type' bind), state

(* Returns variables that do not appear in lower variables *)
let filter_variables vars state =
  List.filter (fun bind -> not(fst (occurs_in_lower_vars bind state))) vars, state

let should_quantify pols =
  List.exists (fun pol -> pol = Pos) pols && List.exists (fun pol -> pol = Neg) pols

let exists bind state =
  List.exists (fun entry -> entry.bind == bind) state.bounds, state

let rec treat type' =
  let* vars = get_variables type' in
  let* vars = filter_variables vars in
  fold_list (fun type' bind ->
    let* exists = exists bind in
    if not exists then
      return type'
    else if should_quantify (appears type' bind Neg) then
      let* bound = get_upper_bound bind in
      print_endline("quantify " ^ bind.name);
      return (Type.base (Type.AbsTypeExpr { param = { bind; bound }; ret = type' }))
    else
      let* t = (inline type' bind Neg) in
      let* () = inline_state bind in
      print_endline("inline " ^ bind.name ^ " " ^ TypeDisplay.display type' ^ " " ^ TypeDisplay.display t);
      let* () = remove_var bind in
      treat t
  ) type' vars

let with_var f =
  let* bind, type' = make_var in
  print_endline("var_start " ^ bind.name);
  let* type' = with_level_inc (f type') in
  print_endline("var_end " ^ bind.name);
  treat type'
