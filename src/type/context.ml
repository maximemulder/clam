open Display

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

(* CONTEXT EMPTY *)

let empty = { id = 0; level = 0; freshs = []; rigids = [] }

(* CONTEXT MONAD *)

module Monad = Util.Monad.StateMonad(struct
  type s = ctx
end)

open Monad

(* CONTEXT FREEZE *)

let freeze_fresh (fresh: fresh) =
  { bind = fresh.bind; lower = fresh.lower; upper = fresh.upper }

let freeze ctx =
  let freshs = List.map freeze_fresh ctx.freshs in
  { id = 0; level = 0; freshs = []; rigids = ctx.rigids @ freshs }

let with_freeze f =
  let* ctx = get in
  let* () = modify (fun ctx -> freeze ctx) in
  let show_constrain = !Global.show_constrain in
  Global.show_constrain := false;
  let* res = f in
  Global.show_constrain := show_constrain;
  let* () = put ctx in
  return res

(* CONTEXT VARIABLES *)

let update_fresh (var: fresh) =
  modify (fun ctx -> { ctx with
    freshs = List.map (fun (old: fresh) -> if old.bind == var.bind then var else old) ctx.freshs
  })

let rec collect_freshs ctx =
  let fresh = List.nth_opt ctx.freshs 0 in
  match fresh with
  | Some fresh when fresh.level >= ctx.level ->
    let ctx = { ctx with freshs = List.tl ctx.freshs } in
    collect_freshs ctx
  | _ ->
    (), ctx

let with_param_fresh (param: Node.param) type' f =
  let* ctx = get in
  let bind = { Abt.name = "'" ^ string_of_int ctx.id } in
  let var = { bind = bind; level = ctx.level + 1; lower = param.lower; upper = param.upper } in
  let ctx = { ctx with id = ctx.id + 1; level = ctx.level + 1; freshs = var :: ctx.freshs } in
  let* () = put ctx in
  let type' = Rename.rename param.bind bind type' in
  let* x = f type' in
  let* () = collect_freshs in
  let* () = modify (fun ctx -> { ctx with level = ctx.level - 1 }) in
  return x

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

(* COMPARE VARIABLES *)

let rec cmp_level (left: Abt.bind_type) (right: Abt.bind_type) (vars: fresh list) =
  match vars with
  | [] ->
    failwith ("Fresh type variables `" ^ left.name ^ "` and `" ^ right.name ^ "` not found in the type context.")
  | var :: vars ->
    if var.bind == left then
      true
    else if var.bind == right then
      false
    else
      cmp_level left right vars

let cmp_level left right =
  let* ctx = get in
  return (cmp_level left right ctx.freshs)

(* REORDER VARIABLES *)

let rec insert bind (other: fresh) vars =
  match vars with
  | [] ->
    [other]
  | var :: vars ->
    var :: if var.bind == bind then
      other :: vars
    else
      insert bind other vars

let rec reorder bind other (vars: fresh list) =
  match vars with
  | [] ->
    []
  | var :: vars ->
    if var.bind == bind then
      var :: vars
    else if var.bind == other then
      insert bind var vars
    else
      var :: reorder bind other vars

let reorder bind other =
  let* ctx = get in
  let freshs = reorder bind other ctx.freshs in
  put { ctx with freshs }

(* CONTEXT SHOW *)

let show cond string =
  let* ctx = get in
  if cond then
    Util.string_indent (List.length ctx.rigids + List.length ctx.freshs + !Global.nesting) string
    |> print_endline;
  return ()

let show_ctx cond =
  let* ctx = get in
  if cond then (
    List.mapi (fun i (var: rigid) -> Util.string_indent i (var.bind.name) ^ ": " ^ display var.lower ^ " .. " ^ display var.upper ^ "\n") (List.rev ctx.rigids)
    |> String.concat ""
    |> print_string;
    let rigids_length = List.length ctx.rigids in
    List.mapi (fun i (var: fresh) -> Util.string_indent (rigids_length + i) (var.bind.name) ^ "~: " ^ display var.lower ^ " .. " ^ display var.upper ^ "\n") (List.rev ctx.freshs)
    |> String.concat ""
    |> print_string
  );
  return ()
