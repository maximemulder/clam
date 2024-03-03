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

open State

module type SEARCHER = sig
  type t
  val bot: t
  val meet: state -> t -> t -> t
  val join: state -> t -> t -> t
end

module Searcher(S: SEARCHER) = struct
  let rec search state f (type': Type.type') =
    search_union state f type'

  and search_union state f union =
    let types = List.map (search_inter state f) union.union in
    Util.list_option_meet types (S.join state)

  and search_inter state f inter =
    let types = List.map (search_base state f) inter.inter in
    Util.list_option_join types (S.meet state)

  and search_base state f type' =
    match type' with
    | Bot ->
      Some S.bot
    | Var var -> (
      let entry, _ = get_var var.bind state in
      match entry with
      | Param entry ->
        search state f entry.upper (* TODO check this *)
      | Infer entry ->
        search state f entry.lower
      )
    | App app ->
      let ctx, _ = get_context state in
      let abs = Type.System.promote ctx app.abs in
      let type' = Type.System.compute ctx abs app.arg in
      search state f type'
    | _ ->
      f type'

    let search_m f type' state =
      search state f type', state
end

module SearcherProj = struct
  type t = Type.type'
  let bot = Type.bot
  let join state =
    let ctx, _ = get_context state in
    Type.System.join ctx
  let meet state =
    let ctx, _ = get_context state in
    Type.System.meet ctx
end

let make_param bound =
  { Type.bind = { name = "_" }; lower = Type.bot; upper = bound } (* TODO lower *)

module SearcherAppType = struct
  type t = { param: Type.param; ret: Type.type' }

  let bot = { param = make_param Type.top; ret = Type.bot }

  let with_merge_param ctx bound left right f =
    let param = make_param bound in
    let left_ret  = Type.System.substitute_body ctx param left.param  left.ret  in
    let right_ret = Type.System.substitute_body ctx param right.param right.ret in
    let ctx = Type.Context.add_param ctx param in
    let ret = f ctx left_ret right_ret in
    { param; ret }

  let join state left right =
    let ctx, _ = get_context state in
    let bound = Type.System.meet ctx left.param.upper right.param.upper in
    with_merge_param ctx bound left right Type.System.join

  let meet state left right =
    let ctx, _ = get_context state in
    if not (Type.System.is_param ctx left.param right.param) then
      bot
    else
    with_merge_param ctx left.param.upper left right Type.System.meet
end

module SearchProj    = Searcher(SearcherProj)
module SearchAppType = Searcher(SearcherAppType)

let search_proj     = SearchProj.search_m
let search_app_type = SearchAppType.search_m
