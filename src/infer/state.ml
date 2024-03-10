(*
  This file contains the environment used for type inference, which itself contains information
  about the current inference type variables, parameter type variables and expression types.
*)

let cmp_bind a b =
  a.Abt.id = b.Abt.id

type entry_def = {
  bind: Abt.bind_expr;
  def: Abt.def_expr;
}

type entry_expr = {
  bind: Abt.bind_expr;
  type': Type.type';
}

type entry_type = {
  bind: Abt.bind_type;
  lower: Type.type';
  upper: Type.type';
}

type entry_var = {
  bind: Abt.bind_type;
  lower: Type.type';
  upper: Type.type';
  id: int;
  level_orig: int;
  level_low: int;
  level: int;
}

type var =
  | Param of entry_type
  | Infer of entry_var

(* STATE *)

type state = {
  id: int;
  level: int;
  defs: entry_def list;
  exprs: entry_expr list;
  types: entry_type list;
  vars: entry_var list;
}

include Util.Monad.Monad(Util.Monad.StateMonad(struct
  type s = state
end))

let get_var bind state =
  match List.find_opt (fun (entry: entry_type) -> entry.bind == bind) state.types with
  | Some entry -> Param entry, state
  | None ->
  match List.find_opt (fun (entry: entry_var) -> entry.bind == bind) state.vars with
  | Some entry -> Infer entry, state
  | None ->
    raise Not_found

let make_state defs exprs =
  let defs = List.map (fun (def: Abt.def_expr) -> { bind = def.bind; def }) defs in
  { id = 0; level = 0; defs; exprs; types = []; vars = [] }

(*
  Adapter functions that allow to use the old type system in the new one. Eventually
  they should disappear once the old type system has been reworked to use the new
  data structures.
*)

let get_context state =
  let assumptions = List.append
    (List.map (fun (entry: entry_type) -> { Type.Context.bind = entry.bind; lower = entry.lower; upper = entry.upper }) state.types)
    (List.map (fun (entry: entry_var) -> { Type.Context.bind = entry.bind; lower = entry.lower; upper = entry.upper }) state.vars)
  in
  { Type.Context.assumptions }, state

let validate type' =
  let* ctx = get_context in
  return (Type.Validate.validate ctx type')

let validate_param param =
  let* ctx = get_context in
  return (Type.Validate.validate_param ctx param)

let validate_proper type' =
  let* ctx = get_context in
  return (Type.Validate.validate_proper ctx type')

let substitute bind arg type' =
  let* ctx = get_context in
  return (Type.System.substitute ctx type' bind arg)

let is left right =
  let* ctx = get_context in
  return (Type.System.is ctx left right)

let isa sub sup =
  let* ctx = get_context in
  return (Type.System.isa ctx sub sup)

let join left right =
  let* ctx = get_context in
  return (Type.System.join ctx left right)

let meet left right =
  let* ctx = get_context in
  return (Type.System.meet ctx left right)

(* STATE FUNCTION *)

let get_def_entry bind state =
  List.find (fun (entry: entry_def) -> cmp_bind entry.bind bind) state.defs, state

let get_expr_entry bind state =
  List.find_opt (fun (entry: entry_expr) -> cmp_bind entry.bind bind) state.exprs, state

let get_var_entry bind state =
  List.find (fun (entry: entry_var) -> entry.bind == bind) state.vars, state

let get_var_entry_opt bind state =
  List.find_opt (fun (entry: entry_var) -> entry.bind == bind) state.vars, state

let update_var_entry bind f state =
  let vars = List.map (fun (entry: entry_var) ->
    if entry.bind == bind then
      f entry
    else
      entry
    ) state.vars in
  (), { state with vars }

let get_def bind =
  let* entry = get_def_entry bind in
  return entry.def

let get_expr_type bind =
  let* entry = get_expr_entry bind in
  let type' = Option.map (fun entry -> entry.type') entry in
  return type'

let get_var_lower bind =
  let* entry = get_var_entry bind in
  return entry.lower

let get_var_upper bind =
  let* entry = get_var_entry bind in
  return entry.upper

let update_var_lower bind bound =
  let* ctx = get_context in
  update_var_entry bind (fun entry -> { entry with lower = Type.System.join ctx entry.lower bound })

let update_var_upper bind bound =
  let* ctx = get_context in
  update_var_entry bind (fun entry -> { entry with upper = Type.System.meet ctx entry.upper bound })

let add_expr bind type' state =
  let exprs = { bind; type' } :: state.exprs in
  (), { state with exprs }

let remove_expr bind state =
  let exprs = List.filter (fun (entry: entry_expr) -> not (cmp_bind entry.bind bind)) state.exprs in
  (), { state with exprs }

let with_expr bind type' f =
  let* () = add_expr bind type' in
  let* x = f in
  let* () = remove_expr bind in
  return x

let add_type bind lower upper state =
  (), { state with types = { bind; lower; upper } :: state.types }

let with_type bind lower upper f state =
  let (), state = add_type bind lower upper state in
  f state
  (* TODO: Remove type variables when they are no longer needed. Type variables are probably not
    scope so an "add" function may be better than a "with" function. The current leak is not
    critical but it is also not very elegant. *)

let make_var state =
  let bind = { Abt.name = "'" ^ string_of_int state.id } in
  let type' = Type.var bind in
  let var = { id = state.id; bind; level_orig = state.level; level_low = state.level; level = state.level; lower = Type.bot; upper = Type.top } in
  let state = { state with id = state.id + 1; vars = var :: state.vars } in
  type', state

let remove_var bind state =
  let vars = List.filter (fun entry -> entry.bind != bind) state.vars in
  (), { state with vars }

let get_state state =
  state, state

let is_infer bind state =
  List.exists (fun (entry: entry_var) -> entry.bind == bind) state.vars

(* DEBUG FUNCTIONS *)

let print string =
  let* state = get_state in
  print_endline (Util.string_indent state.level string);
  return ()

let print_vars state =
  let vars = Util.list_group (fun (var: entry_var) -> var.level_low) state.vars in
  List.iter (fun (id, vars) ->
    let vars = List.sort (fun (a: entry_var) b -> Int.compare a.id b.id) vars in
    let vars = String.concat ", " (List.map (fun var -> var.bind.name ^ ": " ^ Type.display var.lower ^  " .. " ^ Type.display var.upper) vars) in
    print_endline (Util.string_indent id vars)
  ) vars, state
