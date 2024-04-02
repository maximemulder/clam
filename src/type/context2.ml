(**
  New super mega cool type context for type checking algorithm V2
*)

(** Flexibility of a type variable, which can be either fresh, if the variable can be constrained
  during typing, or rigid, if it cannot. *)
type flex =
  | Fresh
  | Rigid

(** Type variable, which has a lower and an upper bound. *)
type var = {
  bind: Abt.bind_type;
  flex: flex;
  lower: Node.type';
  upper: Node.type';
}

(** The type context, which contains the type variables used during typing. *)
type ctx = {
  vars: var list;
}

let empty = { vars = [] }

let add_var bind flex lower upper ctx =
  { vars = { bind; flex; lower; upper } :: ctx.vars }

let update_var var ctx =
  let vars = List.map (fun old -> if old.bind == var.bind then var else old) ctx.vars in
  (), { vars }

let with_param_fresh (param: Node.param) f ctx =
  let var = { bind = param.bind; flex = Fresh; lower = param.lower; upper = param.upper } in
  let ctx = { vars = var :: ctx.vars } in
  let x, ctx = f ctx in
  let ctx = { vars = List.tl ctx.vars } in
  x, ctx

let with_param_rigid (param: Node.param) f ctx =
  let var = { bind = param.bind; flex = Rigid; lower = param.lower; upper = param.upper } in
  let ctx = { vars = var :: ctx.vars } in
  let x, ctx = f ctx in
  let ctx = { vars = List.tl ctx.vars } in
  x, ctx

let get_var bind ctx =
  let var = List.find_opt (fun var -> var.bind == bind) ctx.vars in
  match var with
  | Some var ->
    var, ctx
  | None ->
    failwith ("Type variable `" ^ bind.name ^ "` not found in the type context.")

let display_var var =
  var.bind.name
  ^ (match var.flex with Fresh -> ": " | Rigid -> " = ")
  ^ Display.display var.lower
  ^ " .. "
  ^ Display.display var.upper

let display ctx =
  let vars = List.map display_var (List.rev ctx.vars) in
  String.concat ", " vars

module Monad = Util.Monad.Monad(Util.Monad.StateMonad(struct
  type s = ctx
end))
