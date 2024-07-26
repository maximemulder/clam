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

module Monad = Util.Monad.StateMonad(struct
  type s = ctx
end)

open Monad

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
