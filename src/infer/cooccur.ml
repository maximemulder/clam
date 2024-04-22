(** The algorithm to remove cooccurrences in a type. It is honestly not bery good *)

open Polar


(** The polarities at which a type variable occurs or cooccurs. *)
type cooccs = {
  neg: bool option;
  pos: bool option;
}

(** The empty cooccurrence, when a variable does not occur a type. *)
let cooccs_none = { neg = None; pos = None }

(** The polar cooccurrence, when a variable occurs or cooccurs at a given polarity in a type. *)
let cooccs_pol pol coocc =
  match pol with
  | Neg ->
    { neg = Some coocc; pos = None }
  | Pos ->
    { neg = None; pos = Some coocc }

let cooccs_cmp left right =
  match left, right with
  | Some true, Some true | Some true, None | None, Some true ->
    true
  | _, _ ->
    false

let cooccs_display cooccs =
  let f x = match x with
  | Some true ->
    "yes"
  | Some false ->
    "no"
  | None ->
    "none" in
  "neg:" ^ f cooccs.neg ^ " pos: " ^ f cooccs.pos

let merge_cooccs left right =
  let neg = Util.option_join left.neg right.neg (&&) in
  let pos = Util.option_join left.pos right.pos (&&) in
  { neg; pos }

let occurs bind type' =
  List.exists (List.exists ((=) (Type.Var { bind }))) type'.Type.dnf

let occurs_neg bind type' =
  List.for_all (List.exists ((=) (Type.Var { bind }))) type'.Type.dnf

let occurs_pos bind type' =
  List.exists (List.for_all ((=) (Type.Var { bind }))) type'.Type.dnf

let rec cooccurs bind other pol type' =
  if occurs bind type' then
    match pol with
    | Neg ->
      cooccs_pol Neg (occurs_neg bind type' && occurs_neg other type')
    | Pos ->
      cooccs_pol Pos (occurs_pos bind type' && occurs_pos other type')
  else
    List.map (List.map (cooccurs_base bind other pol)) type'.Type.dnf
    |> List.flatten
    |> List.fold_left merge_cooccs cooccs_none

and cooccurs_base bind other pol type' =
  match type' with
  | Top | Bot | Unit | Bool | Int | String | Var _ ->
    cooccs_none
  | Tuple tuple ->
    List.map (cooccurs bind other pol) tuple.elems
    |> List.fold_left merge_cooccs cooccs_none
  | Record record ->
    Util.NameMap.to_list record.attrs
    |> List.map snd
    |> List.map (cooccurs_attr bind other pol)
    |> List.fold_left merge_cooccs cooccs_none
  | Lam lam ->
    let param = cooccurs bind other (inv pol) lam.param in
    let ret   = cooccurs bind other pol lam.ret in
    merge_cooccs param ret
  | Univ univ ->
    let param = cooccurs_param bind other univ.param in
    let ret   = cooccurs bind other pol univ.ret in
    merge_cooccs param ret
  | Abs abs ->
    let param = cooccurs_param bind other abs.param in
    let body  = cooccurs bind other pol abs.body in
    merge_cooccs param body
  | App app ->
    let abs = cooccurs bind other pol app.abs in
    let arg = cooccurs bind other pol app.arg in
    merge_cooccs abs arg

and cooccurs_attr bind other pol attr =
  cooccurs bind other pol attr.type'

and cooccurs_param bind other param =
  if param.bind == other then
    cooccs_none
  else
  let lower = cooccurs bind other Pos param.lower in
  let upper = cooccurs bind other Neg param.upper in
  merge_cooccs lower upper

let cooccurs bind other pol type' =
  let res = cooccurs bind other pol type' in
  (*print_endline("coocurs " ^ bind.name ^ " with " ^ other.name ^ " in " ^ Type.display type' ^
    " pol " ^ (match pol with Pos -> "pos" | Neg -> "neg") ^
    " neg: " ^ (match res.neg with Some true -> "true" | Some false -> "false" | None -> "none") ^
    " pos: " ^ (match res.pos with Some true -> "true" | Some false -> "false" | None -> "none"));
  *)res

let join_coocc a b = Util.option_join a b (fun a _ -> a)

let rec get_coocc bind orig pol type' =
  List.map (List.map (get_coocc_base bind orig pol)) type'.Type.dnf
  |> List.flatten
  |> List.fold_left join_coocc None

and get_coocc_base bind orig pol type' =
  match type' with
  | Top | Bot | Unit | Bool | Int | String | Var _ ->
    None
  | Tuple tuple ->
    List.map (get_coocc bind orig pol) tuple.elems
    |> List.fold_left join_coocc None
  | Record record ->
    Util.NameMap.to_list record.attrs
    |> List.map snd
    |> List.map (get_coocc_attr bind orig pol)
    |> List.fold_left join_coocc None
  | Lam lam ->
    let param = get_coocc bind orig (inv pol) lam.param in
    let ret   = get_coocc bind orig pol lam.ret in
    join_coocc param ret
  | Univ univ ->
    let a = cooccurs bind univ.param.bind Pos orig in
    let b = cooccurs univ.param.bind bind Pos orig in
    if (cooccs_cmp a.neg b.neg) || (cooccs_cmp a.pos b.pos) then
      Some univ.param.bind
    else
    let param = get_coocc_param bind orig univ.param in
    let ret   = get_coocc bind orig pol univ.ret in
    join_coocc param ret
  | Abs abs ->
    let param = get_coocc_param bind orig abs.param in
    let body  = get_coocc bind orig pol abs.body in
    join_coocc param body
  | App app ->
    let abs = get_coocc bind orig pol app.abs in
    let arg = get_coocc bind orig pol app.arg in
    join_coocc abs arg

and get_coocc_attr bind orig pol attr =
  get_coocc bind orig pol attr.type'

and get_coocc_param bind orig param =
  let lower = get_coocc bind orig Pos param.lower in
  let upper = get_coocc bind orig Neg param.upper in
  join_coocc lower upper

let get_coocc bind pol type' =
  get_coocc bind type' pol type'

open Type.Context
open Type.Context.Monad
open Type.System

let rec simplify_2 (fresh: fresh) orig pol type' =
  Type.System.map_type (simplify_base fresh orig pol) type'

and simplify_base fresh orig pol type' =
  match type' with
  | Top | Bot | Unit | Bool | Int | String | Var _ ->
    return (Type.base type')
  | Tuple tuple ->
    let* elems = list_map (simplify_2 fresh orig pol) tuple.elems in
    return (Type.tuple elems)
  | Record record ->
    let* attrs = map_map (simplify_attr fresh orig pol) record.attrs in
    return (Type.record attrs)
  | Lam lam ->
    let* param = simplify_2 fresh orig (inv pol) lam.param in
    let* ret   = simplify_2 fresh orig pol lam.ret in
    return (Type.lam param ret)
  | Univ univ ->
    let* param = simplify_param fresh orig univ.param in
    with_param_rigid param (
      let* ret = simplify_2 fresh orig pol univ.ret in
      let* cond = simplify_univ_cond fresh orig univ in
      if cond then
        substitute param.bind (Type.var fresh.bind) ret
      else
        return (Type.univ param ret)
    )
  | Abs abs ->
    let* param = simplify_param fresh orig abs.param in
    let* body  = simplify_2 fresh orig pol abs.body in
    return (Type.abs param body)
  | App app ->
    let* abs = simplify_2 fresh orig pol app.abs in
    let* arg = simplify_2 fresh orig pol app.arg in
    return (Type.app abs arg)

and simplify_attr fresh orig pol attr =
  let* type' = simplify_2 fresh orig pol attr.type' in
  return { attr with type' }

and simplify_param fresh orig param =
  let* lower = simplify_2 fresh orig Pos param.lower in
  let* upper = simplify_2 fresh orig Neg param.upper in
  return { param with lower; upper }

and simplify_univ_cond fresh orig univ =
  let* lower = with_freeze (is univ.param.lower fresh.lower) in
  let* upper = with_freeze (is univ.param.upper fresh.upper) in
  let* sub = with_freeze (is (Type.var fresh.bind) univ.param.lower) in
  let* sup = with_freeze (is univ.param.upper (Type.var fresh.bind)) in
  (*print_endline ("lower" ^ fresh.bind.name ^ " " ^ univ.param.bind.name ^ " " ^ string_of_bool lower);
  print_endline ("upper" ^ fresh.bind.name ^ " " ^ univ.param.bind.name ^ " " ^ string_of_bool upper);
  print_endline ("sub" ^ fresh.bind.name ^ " " ^ univ.param.bind.name ^ " " ^ string_of_bool sub);
  print_endline ("sup" ^ fresh.bind.name ^ " " ^ univ.param.bind.name ^ " " ^ string_of_bool sup);
  *)if not (sub || lower) || not (sup || upper) then
    return false
  else
  let fresh_param = cooccurs fresh.bind univ.param.bind Pos orig in
  let param_fresh = cooccurs univ.param.bind fresh.bind Pos orig in
  (*print_endline ("coocc" ^ fresh.bind.name ^ " " ^ univ.param.bind.name ^ " " ^ cooccs_display fresh_param);
  print_endline ("coocc" ^ fresh.bind.name ^ " " ^ univ.param.bind.name ^ " " ^ cooccs_display param_fresh);
  *)let param_fresh = {
    neg = if sup then Some true else param_fresh.neg;
    pos = if sub then Some true else param_fresh.pos;
  } in
  return ((cooccs_cmp fresh_param.neg param_fresh.neg) || (cooccs_cmp fresh_param.pos param_fresh.pos))

(* TODO: Better logging and clean recursion. *)
let rec simplify fresh pol type' =
  let* res = simplify_2 fresh type' pol type' in
  (* if type' <> res then
    print_endline ("simplify " ^ Type.display type' ^ " to " ^ Type.display res);*)
  if res <> type' then (
    (*print_endline ("try " ^ Type.display res);*)
    simplify fresh pol res)
  else
    return res
