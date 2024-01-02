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
  bound: Type.type';
}

type entry_var = {
  bind: Abt.bind_type;
  level: int;
  lower: Type.type';
  upper: Type.type';
}

(* STATE *)

type state = {
  level: int;
  defs: entry_def list;
  exprs: entry_expr list;
  types: entry_type list;
  vars: entry_var list;
}

let make_state defs exprs =
  let defs = List.map (fun def -> { bind = def.Abt.bind; def }) defs in
  { level = 0; defs; exprs; types = []; vars = [] }

let get_context state =
  let assumptions = List.append
    (List.map (fun (entry: entry_type) -> { TypeContext.bind = entry.bind; bound = entry.bound }) state.types)
    (List.map (fun (entry: entry_var) -> { TypeContext.bind = entry.bind; bound = entry.upper }) state.vars)
  in
  { TypeContext.assumptions }, state

let get_def_entry bind state =
  List.find (fun (entry: entry_def) -> cmp_bind entry.bind bind) state.defs, state

let get_expr_entry bind state =
  List.find_opt (fun (entry: entry_expr) -> cmp_bind entry.bind bind) state.exprs, state

let get_var_entry bind state =
  List.find (fun (entry: entry_var) -> entry.bind == bind) state.vars, state

let update_var_entry bind f state =
  let vars = List.map (fun (entry: entry_var) ->
    if entry.bind == bind then
      f entry
    else
      entry
    ) state.vars in
  (), { state with vars }

include Monad.Monad(Monad.StateMonad(struct
  type s = state
end))

let get_def bind =
  let* entry = get_def_entry bind in
  return entry.def

let get_expr_type bind =
  let* entry = get_expr_entry bind in
  let type' = Option.map (fun entry -> entry.type') entry in
  return type'

let get_level bind =
  let* entry = get_var_entry bind in
  return entry.level

let get_lower_bound bind =
  let* entry = get_var_entry bind in
  return entry.lower

let get_upper_bound bind =
  let* entry = get_var_entry bind in
  return entry.upper

let update_lower_bound bind bound =
  let* ctx = get_context in
  update_var_entry bind (fun entry -> { entry with lower = TypeSystem.join ctx entry.lower bound })

let update_upper_bound bind bound =
  let* ctx = get_context in
  update_var_entry bind (fun entry -> { entry with upper = TypeSystem.meet ctx entry.upper bound })

let remove_def bind state =
  let defs = List.filter (fun (entry: entry_def) -> not (cmp_bind entry.bind bind)) state.defs in
  (), { state with defs }

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

let with_type bind bound f state =
  let state = { state with types = { bind; bound } :: state.types } in
  let x, state = f state in
  x, { state with types = List.tl state.types }

(* I use a global counter so that each variable has a distinct name, which is easier for debugging *)
let counter = ref 0

let make_var state =
  let bind = { Abt.name = "'" ^ string_of_int counter.contents } in
  counter := counter.contents + 1;
  let type' = Type.var bind in
  let var = { bind; level = state.level; lower = Type.bot; upper = Type.top } in
  let state = { state with vars = var :: state.vars } in
  type', state

let remove_var bind state =
  let vars = List.filter (fun entry -> entry.bind != bind) state.vars in
  (), { state with vars }

let get_state state =
  state, state
