let cmp_bind a b =
  a.Abt.id = b.Abt.id

type entry_def = {
  bind: Abt.bind_expr;
  def: Abt.def_expr;
}

type entry_type = {
  bind: Abt.bind_expr;
  type': Type.type';
}

type entry_bounds = {
  bind: Abt.bind_type;
  level: int;
  lower: Type.type';
  upper: Type.type';
}

(* STATE *)

type state = {
  level: int;
  defs: entry_def list;
  types: entry_type list;
  bounds: entry_bounds list;
}

let make_state defs types =
  let defs = List.map (fun def -> { bind = def.Abt.bind; def }) defs in
  { level = 0; defs; types; bounds = [] }

let get_context state =
  (* TODO: bounds ??? *)
  let assumptions = List.map (fun entry -> { TypeContext.bind = entry.bind; bound = entry.upper }) state.bounds in
  { TypeContext.assumptions }, state

let get_bind_def bind state =
  let entry = List.find (fun (entry: entry_def) -> cmp_bind entry.bind bind) state.defs in
  entry.def, state

let get_bind_type bind state =
  let entry = List.find_opt (fun (entry: entry_type) -> cmp_bind entry.bind bind) state.types in
  let type' = Option.map (fun entry -> entry.type')  entry in
  type', state

let get_lower_bound bind state =
  let entry = List.find (fun (entry: entry_bounds) -> entry.bind == bind) state.bounds in
  entry.lower, state

let get_upper_bound bind state =
  let entry = List.find (fun (entry: entry_bounds) -> entry.bind == bind) state.bounds in
  entry.upper, state

let set_upper_bound bind bound state =
  let bounds = List.map (fun (entry: entry_bounds) ->
    if entry.bind == bind then
      let ctx, _ = get_context state in
      { entry with upper = TypeSystem.meet ctx entry.upper bound }
    else
      entry
    ) state.bounds in
  (), { state with bounds }

let set_lower_bound bind bound state =
  let bounds = List.map (fun (entry: entry_bounds) ->
    if entry.bind == bind then
      let ctx, _ = get_context state in
      { entry with lower = TypeSystem.join ctx entry.lower bound }
    else
      entry
    ) state.bounds in
  (), { state with bounds }

let get_level bind state =
  let entry = List.find (fun (entry: entry_bounds) -> entry.bind == bind) state.bounds in
  entry.level, state

let remove_def bind state =
  let defs = List.filter (fun (entry: entry_def) -> not (cmp_bind entry.bind bind)) state.defs in
  let state = { state with defs } in
  (), state

let add_bind bind type' state =
  let types = { bind; type' } :: state.types in
  let state = { state with types } in
  (), state

let with_bind bind type' f state =
  let types = { bind; type' } :: state.types in
  let state = { state with types } in
  let x, state = f state in
  let types = List.filter (fun (entry: entry_type) -> not (cmp_bind entry.bind bind)) state.types in
  let state = { state with types } in
  x, state

(* I use a global counter so that each variable has a distinct name, which is easier for debugging *)
let counter = ref 0

let make_var state =
  let bind = { Abt.name = "'" ^ string_of_int counter.contents } in
  counter := counter.contents + 1;
  let type' = Type.var bind in
  let bound = { bind; level = state.level; lower = Type.bot; upper = Type.top } in
  let state = { state with bounds = bound :: state.bounds } in
  type', state

let remove_var bind state =
  let bounds = List.filter (fun entry -> entry.bind != bind) state.bounds in
  (), { state with bounds }

include Monad.Monad(Monad.StateMonad(struct
  type s = state
end))
