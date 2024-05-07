(* CONTEXT TYPES *)

(** Fresh type variable, whose bounds can be tightened, and which can be reordered in the context. *)
type fresh = {
  bind: Abt.bind_type;
  level: int;
  lower: Node.type';
  upper: Node.type';
}

(** Rigid type variable, which have fixed bounds and order in the context. *)
type rigid = {
  bind: Abt.bind_type;
  lower: Node.type';
  upper: Node.type';
}

(** Typing context, which contains both fresh and rigid type variables. *)
type ctx = {
  id: int;
  level: int;
  freshs: fresh list;
  rigids: rigid list;
}

(* TODO: Factorize the enumeration inside the type context. *)
(** Type variable, either fresh or rigid. *)
type var =
  | Fresh of fresh
  | Rigid of rigid

(* CONTEXT MAKE *)

let empty = { id = 0; level = 0; freshs = []; rigids = [] }

(* CONTEXT MONAD *)

module Monad = Util.Monad.StateMonad(struct
  type s = ctx
end)

open Monad

let with_param_rigid (param: Node.param) f =
  let var = { bind = param.bind; lower = param.lower; upper = param.upper } in
  let* () = modify (fun ctx -> { ctx with rigids = var :: ctx.rigids }) in
  let* x = f in
  let* () = modify (fun ctx -> { ctx with rigids = List.tl ctx.rigids }) in
  return x

(** Returns the fresh or rigid variable corresponding to a given bind in the context. *)
let get_var bind =
  let* ctx = get in
  let fresh = List.find_opt (fun (var: fresh) -> var.bind == bind) ctx.freshs in
  let rigid = List.find_opt (fun (var: rigid) -> var.bind == bind) ctx.rigids in
  match fresh, rigid with
  | Some _, Some _ ->
    failwith ("Type variable `" ^ bind.name ^ "` found twice in the type context.")
  | Some var, None ->
    return (Fresh var)
  | None, Some var ->
    return (Rigid var)
  | None, None ->
    failwith ("Type variable `" ^ bind.name ^ "` not found in the type context.")
