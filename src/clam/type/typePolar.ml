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
  | AbsTypeExpr abs ->
    let* param = occurs_param abs.param bind in
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

and occurs_param param bind =
  occurs param.bound bind

let occurs_in_lower_vars bind state =
  let level, _ = get_level bind state in
  List.filter (fun (entry: entry_bounds) -> entry.level < level) state.bounds
  |> List.map (fun entry -> entry.bind)
  |> List.exists (fun var -> fst (occurs_bind { bind = var } bind state)), state

(* TEST *)

let rec inline (type': Type.type') bind pol =
  inline_union type' bind pol

and inline_union union bind pol =
  let* ctx = get_context in
  let* types = map_list (fun type' -> inline_inter type' bind pol) union.union in
  return (Utils.list_reduce (TypeSystem.join ctx) types)

and inline_inter inter bind pol =
  let* ctx = get_context in
  let* types = map_list (fun type' -> inline_base type' bind pol) inter.inter in
  return (Utils.list_reduce (TypeSystem.meet ctx) types)

and inline_base type' bind pol =
  match type' with
  | Type.Var var when var.bind == bind -> (
    match pol with
    | Pos ->
      let* bound = get_upper_bound var.bind in
      let* cond = occurs_in_lower_vars var.bind in
      if not cond then
        inline bound bind Pos
      else
        return bound
    | Neg ->
      let* bound = get_lower_bound var.bind in
      let* cond = occurs_in_lower_vars var.bind in
      if not cond then
        inline bound bind Neg
      else
        return bound
  )
  | Tuple tuple ->
    let* elems = map_list (fun elem -> inline elem bind pol) tuple.elems in
    return (Type.tuple elems)
  | Record record ->
    let* attrs = map_map (fun attr -> inline_attr attr bind pol) record.attrs in
    return (Type.record attrs)
  | AbsExpr abs ->
    let* param = inline abs.param bind (inv pol) in
    let* ret = inline abs.ret bind pol in
    return (Type.abs_expr param ret)
  | AbsTypeExpr abs ->
    let* param = inline_param abs.param bind pol in
    let* ret = inline abs.ret bind pol in
    return (Type.abs_type_expr param ret)
  | _ ->
    return (Type.base type')

and inline_attr attr bind pol =
  let* type' = inline attr.type' bind pol in
  return { attr with type' }

and inline_param param bind _pol =
  (* TODO: Polarity ??? *)
  let* bound = inline param.bound bind Neg in
  return { param with bound }

(* POLARITY *)

(* Returns the polarities in which a type variable occurs in a type, used to know whether to
  inline or quantify this variable *)

type occurence = {
  pos: bool;
  neg: bool;
}

let none = {
  pos = false;
  neg = false;
}

let occur pol =
  match pol with
  | Pos ->
    {
      pos = true;
      neg = false;
    }
  | Neg ->
    {
      pos = false;
      neg = true;
    }

let combine left right =
  {
    pos = left.pos || right.pos;
    neg = left.neg || right.neg;
  }

let rec appears (type': Type.type') bind pol =
  appears_union type' bind pol

and appears_union union bind pol =
  let types = List.map (fun type' -> appears_inter type' bind pol) union.union in
  Utils.list_reduce combine types

and appears_inter inter bind pol =
  let types = List.map (fun type' -> appears_base type' bind pol) inter.inter in
  Utils.list_reduce combine types

and appears_base type' bind pol =
  match type' with
  | Type.Var var when var.bind == bind ->
    occur pol
  | Tuple tuple ->
    List.map (fun elem -> appears elem bind pol) tuple.elems
    |> List.fold_left combine none
  | Record record ->
    Utils.NameMap.to_list record.attrs
    |> List.map snd
    |> List.map (fun (attr: Type.attr) -> appears_attr attr bind pol)
    |> List.fold_left combine none
  | AbsExpr abs ->
    combine
      (appears abs.param bind (inv pol))
      (appears abs.ret bind pol)
  | AbsTypeExpr abs ->
    combine
      (appears_param abs.param bind pol)
      (appears abs.ret bind pol)
  | _ ->
    none

and appears_attr attr bind pol =
  appears attr.type' bind pol

and appears_param param bind pol =
  appears param.bound bind pol

let inline_state bind state =
  let bounds = List.map (fun entry -> { entry with
    upper = fst(inline entry.upper bind Pos state);
    lower = fst(inline entry.lower bind Neg state);
  }) state.bounds in
  (), { state with bounds }

(* Returns variables that are equal or higher to the current state level and that do not appear in lower variables *)
let get_variables state =
  List.filter (fun (entry: entry_bounds) -> entry.level >= state.level) state.bounds
  |> List.map (fun entry -> entry.bind)
  |> List.filter (fun bind -> not(fst (occurs_in_lower_vars bind state))), state

let should_quantify occurence =
  occurence.pos && occurence.neg

let rec treat type' =
  let* vars = get_variables in
  match vars with
  | [] ->
    return type'
  | bind :: _ ->
    let* type' = if should_quantify (appears type' bind Neg) then
      let* bound = get_upper_bound bind in
      return (Type.abs_type_expr { bind; bound } type')
    else
      let* type' = (inline type' bind Neg) in
      let* () = inline_state bind in
      return type'
    in
    let* () = remove_var bind in
    treat type'

let with_level f state =
  let state = { state with level = state.level + 1 } in
  let x, state = f state in
  let x, state = treat x state in
  let state = { state with level = state.level - 1 } in
  x, state

let with_var f =
  let* type' = make_var in
  with_level (f type')
