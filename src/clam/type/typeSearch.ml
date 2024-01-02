let make_anonymous_param bound =
  { Type.bind = { name = "_" }; bound }

module type SEARCHER = sig
  type t
  val bot: t
  val meet: TypeContext.context -> t -> t -> t
  val join: TypeContext.context -> t -> t -> t
end

module Searcher(S: SEARCHER) = struct
  let rec search ctx f (type': Type.type') =
    search_union ctx f type'

  and search_union ctx f union =
    let types = List.map (search_inter ctx f) union.union in
    Utils.list_option_meet types (S.join ctx)

  and search_inter ctx f inter =
    let types = List.map (search_base ctx f) inter.inter in
    Utils.list_option_join types (S.meet ctx)

  and search_base ctx f type' =
    match type' with
    | Type.Bot ->
      Some S.bot
    | Type.Var var ->
      let bound = TypeContext.get_bind_type ctx var.bind in
      search ctx f bound
    | Type.App app ->
      let abs = TypeSystem.promote ctx app.abs in
      let type' = TypeSystem.compute ctx abs app.arg in
      search ctx f type'
    | _ ->
      f type'
end

module SearcherProj = struct
  type t = Type.type'
  let bot = Type.bot
  let join = TypeSystem.join
  let meet = TypeSystem.meet
end

module SearcherApp = struct
  type t = { param: Type.type'; ret: Type.type' }

  let bot = { param = Type.top; ret = Type.bot }

  let join ctx left right =
    let param = TypeSystem.meet ctx left.param right.param in
    let ret = TypeSystem.join ctx left.ret right.ret in
    { param; ret }

  let meet ctx left right =
    let param = TypeSystem.join ctx left.param right.param in
    let ret = TypeSystem.meet ctx left.ret right.ret in
    { param; ret }
end

module SearcherAppType = struct
  type t = { param: Type.param; ret: Type.type' }

  let bot = { param = make_anonymous_param Type.top; ret = Type.bot }

  let with_merge_param ctx bound left right f =
    let param = make_anonymous_param bound in
    let left_ret  = TypeSystem.substitute_body ctx param left.param  left.ret  in
    let right_ret = TypeSystem.substitute_body ctx param right.param right.ret in
    let ctx = TypeContext.add_bind_type ctx param.bind param.bound in
    let ret = f ctx left_ret right_ret in
    { param; ret }

  let join ctx left right =
    let bound = TypeSystem.meet ctx left.param.bound right.param.bound in
    with_merge_param ctx bound left right TypeSystem.join

  let meet ctx left right =
    if not (TypeSystem.is_param ctx left.param right.param) then
      bot
    else
    with_merge_param ctx left.param.bound left right TypeSystem.meet
end

module SearchProj    = Searcher(SearcherProj)
module SearchApp     = Searcher(SearcherApp)
module SearchAppType = Searcher(SearcherAppType)

let search_proj     = SearchProj.search
let search_app      = SearchApp.search
let search_app_type = SearchAppType.search
