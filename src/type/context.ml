(**
  New super mega cool type context for type checking algorithm V2
*)

let isa_nesting = ref 0

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

let empty = { id = 0; level = 0; freshs = []; rigids = [] }

(* FREEZE CONTEXT *)

let freeze_fresh (fresh: fresh) =
  { bind = fresh.bind; lower = fresh.lower; upper = fresh.upper }

let freeze ctx =
  let freshs = List.map freeze_fresh ctx.freshs in
  (* TODO: SUS AF *)
  { id = 0; level = 0; freshs = []; rigids = ctx.rigids @ freshs }

let with_freeze f ctx =
  let frozen = freeze ctx in
  let show_isa = !Global.show_isa in
  Global.show_isa := false;
  let x, _ = f frozen in
  Global.show_isa := show_isa;
  x, ctx

(* PRINT CONTEXT *)

let show string ctx =
    Util.string_indent (List.length ctx.rigids + List.length ctx.freshs + !isa_nesting) string
    |> print_endline;
  (), ctx

let show_ctx ctx =
  List.mapi (fun i (var: rigid) -> Util.string_indent i (var.bind.name) ^ ": " ^ Display.display var.lower ^ " .. " ^ Display.display var.upper ^ "\n") (List.rev ctx.rigids)
  |> String.concat ""
  |> print_string;
  let rigids_length = List.length ctx.rigids in
  List.mapi (fun i (var: fresh) -> Util.string_indent (rigids_length + i) (var.bind.name) ^ "~: " ^ Display.display var.lower ^ " .. " ^ Display.display var.upper ^ "\n") (List.rev ctx.freshs)
  |> String.concat ""
  |> print_string;
  (), ctx

let show_isa string ctx =
  if !Global.show_isa then
    show string ctx
  else
    (), ctx

let show_isa_ctx ctx =
  if !Global.show_isa then
    show_ctx ctx
  else
    (), ctx

(* ACCESS TYPE VARIABLES *)

let rec collect_freshs ctx =
  let fresh = List.nth_opt ctx.freshs 0 in
  match fresh with
  | Some fresh when fresh.level >= ctx.level ->
    let _ = show_isa ("collect " ^ fresh.bind.name) ctx in
    let ctx = { ctx with freshs = List.tl ctx.freshs } in
    collect_freshs ctx
  | _ ->
    ctx

let update_fresh (var: fresh) ctx =
  let freshs = List.map (fun (old: fresh) -> if old.bind == var.bind then var else old) ctx.freshs in
  (), { ctx with freshs }

let with_param_fresh (param: Node.param) type' f ctx =
  let bind = { Abt.name = "'" ^ string_of_int ctx.id } in
  let var = { bind = bind; level = ctx.level + 1; lower = param.lower; upper = param.upper } in
  let ctx = { ctx with id = ctx.id + 1; level = ctx.level + 1; freshs = var :: ctx.freshs } in
  let _ = show_isa ("fresh_isa " ^ var.bind.name) ctx in
  let type' = Rename.rename param.bind bind type' in
  let x, ctx = f type' ctx in
  let ctx = collect_freshs ctx in
  let ctx = { ctx with level = ctx.level - 1 } in
  x, ctx

let with_param_rigid (param: Node.param) f ctx =
  let var = { bind = param.bind; lower = param.lower; upper = param.upper } in
  let ctx = { ctx with rigids = var :: ctx.rigids } in
  let x, ctx = f ctx in
  let ctx = { ctx with rigids = List.tl ctx.rigids } in
  x, ctx

(** Type variable, either fresh or rigid. *)
type var =
| Fresh of fresh
| Rigid of rigid

(** Returns the fresh or rigid variable corresponding to a given bind in the context. *)
let get_var bind ctx =
  let fresh = List.find_opt (fun (var: fresh) -> var.bind == bind) ctx.freshs in
  let rigid = List.find_opt (fun (var: rigid) -> var.bind == bind) ctx.rigids in
  match fresh, rigid with
  | Some _, Some _ ->
    failwith ("Type variable `" ^ bind.name ^ "` found twice in the type context.")
  | Some var, None ->
    Fresh var, ctx
  | None, Some var ->
    Rigid var, ctx
  | None, None ->
    failwith ("Type variable `" ^ bind.name ^ "` not found in the type context.")

(* REORDER FRESH VARIABLES *)

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

(** Compares the polymorphic level of two fresh type variables. *)
let cmp_level left right ctx =
  cmp_level left right ctx.freshs, ctx

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

let reorder bind other ctx =
  let freshs = reorder bind other ctx.freshs in
  (), { ctx with freshs }

(* DISPLAY CONTEXT *)

let display_fresh (var: fresh) =
  var.bind.name
  ^ ": "
  ^ Display.display var.lower
  ^ " .. "
  ^ Display.display var.upper

let display_rigid (var: rigid) =
  var.bind.name
  ^ ": "
  ^ Display.display var.lower
  ^ " .. "
  ^ Display.display var.upper

let display ctx =
  let rigids = List.map display_rigid (List.rev ctx.rigids) |> String.concat ", " in
  let freshs = List.map display_fresh (List.rev ctx.freshs) |> String.concat ", " in
  rigids ^ "\n" ^ freshs, ctx

(* MONAD *)

module Monad = Util.Monad.Monad(Util.Monad.StateMonad(struct
  type s = ctx
end))

let get_context ctx = ctx, ctx
