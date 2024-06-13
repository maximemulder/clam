(*
  This file contains the data structures and algorithms for type inference for tuple projections
  and type applications. These algorithms are not constraint-based and not complete as type
  inference for these expressions is sometimes impossible, such as in the two following examples :

  ```
  (p) -> p.0
  ```

  ```
  (p) -> p[T]
  ```

  This file is bugged and needs a rewrite, but this requires determining the appropriate semantics
  for tuples and explicit type applications, if we choose to keep them.
*)

open Abt.Type
open Type.Context
open Type.Context.Monad
open Type.Rename
open Type.System

type 'a s = 'a t

module type SEARCHER = sig
  type t
  val bot: t
  val meet: t -> t -> t s
  val join: t -> t -> t s
end

module Searcher(S: SEARCHER) = struct
  let rec search f type' =
    match type' with
    | Bot _ ->
      return (Some S.bot)
    | Var var -> (
      let* var = get_var var.bind in
      match var with
      | Rigid rigid ->
        search f rigid.upper
      | Fresh fresh ->
        search f fresh.lower (* This line is bugged *)
      )
    | App app ->
      let* abs = promote_lower app.abs     in
      let* arg = compute abs app.arg in
      search f arg
    | Union union ->
      let* left  = search f union.left  in
      let* right = search f union.right in
      option_meet S.join left right
    | Inter inter ->
      let* left  = search f inter.left  in
      let* right = search f inter.right in
      option_join S.meet left right
    | _ ->
      f type'

    let search_m f type' ctx =
      search (fun x ctx -> f x, ctx) type' ctx |> fst
end

module SearcherProj = struct
  type t   = type'
  let bot  = Type.Build.bot
  let join = join
  let meet = meet
end

let make_param bound: param =
  { span = Code.span_primitive; bind = { name = "_" }; lower = Type.Build.bot; upper = bound } (* TODO lower *)

module SearcherAppType = struct
  type t = { param: param; ret: type' }

  let bot = { param = make_param Type.Build.top; ret = Type.Build.bot }

  let with_merge_param bound left right f =
    let param = make_param bound in
    let left_ret  = rename left.param.bind  param.bind left.ret  in
    let right_ret = rename right.param.bind param.bind right.ret in
    let* ret = f left_ret right_ret in
    return { param; ret }

  let join left right =
    let* upper = Type.System.meet left.param.upper right.param.upper in
    with_merge_param upper left right Type.System.join

  let meet left right =
    let* upper = Type.System.join left.param.upper right.param.upper in
    with_merge_param upper left right Type.System.meet
end

module SearchProj    = Searcher(SearcherProj)
module SearchAppType = Searcher(SearcherAppType)

let search_proj     = SearchProj.search_m
let search_app_type = SearchAppType.search_m
