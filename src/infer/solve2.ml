(*
  This file contains the algorithm that decides how to inline or quantify type variables.
  It is currently a little ugly and almost certainly buggy. The biggest challenge is to figure
  out in which order the type variables should be treated.
*)

open Polar
open State

let prioritize f vars =
  let news = List.filter f vars in
  if List.is_empty news then
    vars
  else
    news

(* Prioritize variables that do not occur in other bounds *)
(* Ideally this should be determined when variable levels are being lowered *)
let prioritize_bound vars state =
  prioritize (fun var ->
    List.for_all (fun (entry: entry_var) ->
      if entry.bind == var then
        true
      else
        let a, _ = occurs entry.lower var state in
        let b, _ = occurs entry.upper var state in
        (not a && not b)
    ) state.vars
  ) vars

let substitute_state bind arg state =
  let ctx, _ = get_context state in
  let vars = List.map (fun entry -> { entry with
    lower = Type.System.substitute_arg ctx bind arg entry.lower;
    upper = Type.System.substitute_arg ctx bind arg entry.upper;
  }) state.vars in
  let exprs =  List.map (fun entry -> {
    entry with type' = Type.System.substitute_arg ctx bind arg entry.type'
  }) state.exprs in
  (), { state with vars; exprs }

open Inline

let inline_state bind state =
  let exprs =  List.map (fun entry -> {
    entry with type' = fst(inline entry.type' bind Neg state)
  })  state.exprs in
  (), { state with exprs }

let rec solve type' level =
  let* pols = Extrude.extrude type' level in
  (* print_endline ("in " ^ Type.display type');
  List.iter
    (fun bind -> print_endline("  " ^ (bind: Abt.bind_type).name)) pols; *)
  let* state = get_state in
  print_endline("solve " ^ Type.display type' ^ " " ^ string_of_int (List.length pols));
  match pols with
  | [] ->
    (* print_endline("to " ^ Type.display type'); *)
    return type'
  | bind :: _ -> (
    let* entry = get_var_entry bind in
    (* let* () = print_state in *)
    let pols = get_pols type' bind Neg in
    let* lower = get_var_lower bind in
    let* upper = get_var_upper bind in
    let* type' = (match pols.neg, pols.pos with
    | Some ((_ :: _) as neg), _ ->
      print_endline("  co-neg " ^ bind.name);
      let neg = List.map Type.var neg in
      let* neg = fold_list join Type.bot neg in
      (* let* _ = substitute_state bind neg in *)
      substitute bind neg type'
    | _, Some ((_ :: _) as pos) ->
      print_endline("  co-pos " ^ bind.name);
      let pos = List.map Type.var pos in
      let* pos = fold_list meet Type.top pos in
      (* let* _ = substitute_state bind pos in *)
      substitute bind pos type'
    | Some [], Some [] ->
      let* entry = get_var_entry bind in
      print_endline("  quantify " ^ bind.name ^ " " ^ Type.display lower ^ " < " ^ Type.display upper ^ " " ^ string_of_int entry.level_orig ^ " " ^ string_of_int entry.level_low ^ " " ^ string_of_int state.level);
      let* upper = get_var_upper bind in
      let* _lower = get_var_lower bind in
      (* if lower <> Type.bot then
        (* This is a hack that probably does not generalize well *)
        inline type' bind Neg
      else *)
      let param = { Abt.name = bind.name } in
      let* () = add_type param upper in
      let* type' = substitute bind (Type.var param) type' in
      let type' = (Type.univ { bind = param; bound = upper } type') in
      let* () = add_type bind upper in
      return type'
    | Some [], None ->
      print_endline("  inline neg " ^ bind.name ^ " " ^ Type.display lower);
      (* let* () = inline_state bind in *)
      let* lower = get_var_lower bind in
      substitute bind lower type'
    | None, Some [] ->
      print_endline("  inline pos " ^ bind.name ^ " " ^ Type.display upper);
      (* let* () = inline_state bind in *)
      let* upper = get_var_upper bind in
      substitute bind upper type'
    | None, None ->
      (* let* () = inline_state bind in *)
      return type') in
    (* let* () = remove_var bind in *)
    (* print_endline("to " ^ Type.display type'); *)
    print_endline("res " ^ Type.display type');
    solve type' (min level entry.level_low))

let with_level f state =
  let state = { state with level = state.level + 1 } in
  let x, state = f state in
  let state = { state with level = state.level - 1 } in
  x, state

let with_var f =
  let* var = make_var in
  let* type' = with_level (f var) in
  let* state = get_state in
  solve type' state.level
