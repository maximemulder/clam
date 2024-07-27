open Ast
open Ident
open Util.Monad

type ctx_var = {
  ident: ident;
  type': term;
}

type ctx_univ = {
  ident: ident;
}

type ctx_exis = {
  ident: ident;
}

type ctx = {
  vars: ctx_var list;
  univs: ctx_univ list;
  exiss: ctx_exis list;
}

let ctx_empty = { vars = []; univs = []; exiss = [] }

let add_var ident type' ctx =
  { ctx with vars = { ident; type' } :: ctx.vars }

let add_exis ident ctx =
  { ctx with exiss = { ident } :: ctx.exiss }

module S = struct
  type state = ctx
end

type constrain =
  | SubType of { sub: term; sup: term }
  | InType  of { term: term; type': term }

module R = struct
  type error = constrain list
end

module M = Util.Monad2.Result.Result(R)

module Monad = Util.Monad2.State.StateT(Util.Monad2.Result.Result(R))(S)

open Monad

let all (fs: (ctx -> ((unit * ctx), R.error) result) list) (ctx: ctx) : ((unit * ctx), R.error) result =
  List.fold_left (fun prev f -> match prev with
    | Ok ((), ctx) -> f ctx
    | Error constraints -> Error constraints
  ) (Ok ((), ctx)) fs

let with_var type' f =
  let ident = new_ident_anon () in
  let* () = modify (add_var ident type') in
  f ident

let with_univ _f = failwith "TODO"

let with_exis span f =
  let ident = new_ident_anon () in
  let* () = modify (add_exis ident) in
  let var = Var { span; ident } in
  f var
