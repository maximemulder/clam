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
*)

open Type.Context
open Type.Context.Monad

type 'a s = 'a t

module type SEARCHER = sig
  type t
  val bot: t
  val meet: t -> t -> t s
  val join: t -> t -> t s
end

module Searcher(S: SEARCHER) = struct
  let rec search f (type': Type.type') =
    search_union f type'.dnf

  and search_union f types =
    let* types = list_map (search_inter f) types in
    list_option_meet types (S.join)

  and search_inter f types =
    let* types = list_map (search_base f) types in
    list_option_join types (S.meet)

  and search_base f type' =
    match type' with
    | Bot ->
      return (Some S.bot)
    | Var var -> (
      let* var = get_var var.bind in
      match var with
      | Rigid rigid ->
        search f rigid.lower
      | Fresh fresh ->
        search f fresh.lower
      )
    | App app ->
      let* abs = Type.System.promote_lower app.abs     in
      let* arg = Type.System.compute abs app.arg in
      search f arg
    | _ ->
      f type'

    let search_m f type' ctx =
      search (fun x ctx -> f x, ctx) type' ctx |> fst
end

module SearcherProj = struct
  type t = Type.type'
  let bot = Type.bot
  let join = Type.System.join
  let meet = Type.System.meet
end

let make_param bound =
  { Type.bind = { name = "_" }; lower = Type.bot; upper = bound } (* TODO lower *)

module SearcherAppType = struct
  type t = { param: Type.param; ret: Type.type' }

  let bot = { param = make_param Type.top; ret = Type.bot }

  let with_merge_param bound left right f =
    let param = make_param bound in
    let left_ret  = Type.rename left.param.bind  param.bind left.ret  in
    let right_ret = Type.rename right.param.bind param.bind right.ret in
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
