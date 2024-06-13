open Abt.Display
open Abt.Type
open Context
open Context.Monad
open Util
open Util.Func

type kind =
  | Proper
  | Higher of higher

and higher = {
  lower: type';
  upper: type';
  body: kind;
}

let rec get_kind type' =
  match type' with
  | Var var     -> get_kind_var var
  | Abs abs     -> get_kind_abs abs
  | App app     -> get_kind_app app
  | Rec rec'    -> get_kind_rec rec'
  | Union union -> get_kind_union union
  | Inter inter -> get_kind_inter inter
  | _           -> return (Some Proper)

and get_kind_union union =
  (* Both left and right have the same kind in a well-formed type. *)
  let* left  = get_kind union.left  in
  let* right = get_kind union.right in
  return (option_join const left right)

and get_kind_inter inter =
  (* Both left and right have the same kind in a well-formed type. *)
  let* left  = get_kind inter.left  in
  let* right = get_kind inter.right in
  return (option_join const left right)

and get_kind_var var =
  let* var = get_var var.bind in
  match var with
  | Fresh _ ->
    return (Some Proper)
  | Rigid rigid ->
    (* Both the lower and upper bounds have the same kind in a well-formed
      type. *)
    get_kind rigid.lower

and get_kind_abs abs =
  let* body = with_param_rigid abs.param (get_kind abs.body) in
  match body with
  | Some body ->
    return (Some (Higher { lower = abs.param.lower; upper = abs.param.upper; body }))
  | None ->
    return None

and get_kind_app app =
  let* abs = get_kind app.abs in
  match abs with
  | None ->
    return None
  | Some abs ->
  match abs with
  | Higher { body; _ } ->
    return (Some body)
  | Proper ->
    failwith "Ill-formed type application in `Kind.get_kind_app`."

and get_kind_rec rec' =
  get_kind rec'.body

let get_kind type' =
  let* kind = get_kind type' in
  match kind with
  | Some kind ->
    return kind
  | None ->
    failwith "Ill-formed type application in `Kind.get_kind`."

let rec get_kind_min span kind =
  match kind with
  | Proper ->
    Bot { span }
  | Higher { lower; upper; body } ->
    let bind: bind_type = { name = "_" } in
    let param: param = { span; bind; lower; upper } in
    Abs { span; param; body = get_kind_min span body }

let rec get_kind_max span kind =
  match kind with
  | Proper ->
    Top { span }
  | Higher { lower; upper; body } ->
    let bind: bind_type = { name = "_" } in
    let param: param = { span; bind; lower; upper } in
    Abs { span; param; body = get_kind_max span body }

let rec display_kind kind =
  match kind with
  | Proper -> "*"
  | Higher higher -> "[" ^ display higher.lower ^ " .. " ^ display higher.upper ^ "] -> " ^ display_kind higher.body
