let make_anonymous_param bound =
  { Type.bind = { name = "_" }; bound }

module type SEARCHER = sig
  type t
  val bot: t
  val meet: TypeContext.context -> t -> t -> t
  val join: TypeContext.context -> t -> t -> t
end

module Searcher(S: SEARCHER) = struct
  let rec infer ctx f (type': Type.type') =
    infer_union ctx f type'

  and infer_union ctx f union =
    let types = List.map (infer_inter ctx f) union.union in
    Utils.list_option_meet types (S.join ctx)

  and infer_inter ctx f inter =
    let types = List.map (infer_base ctx f) inter.inter in
    Utils.list_option_join types (S.meet ctx)

  and infer_base ctx f type' =
    match type' with
    | Type.Bot ->
      Some S.bot
    | Type.Var var ->
      let bound = TypeContext.get_bind_type ctx var.bind in
      infer ctx f bound
    | Type.App _ ->
      (* TODO: Recurse on applied bounds ? *)
      None
    | _ ->
      f type'
end

module SearcherProj = struct
  type t = Type.type'
  let bot = TypePrimitive.bot
  let join = Typing2.join
  let meet = Typing2.meet
end

module SearchProj = Searcher(SearcherProj)

module SearcherApp = struct
  type t = { param: Type.type'; ret: Type.type' }

  let bot = { param = TypePrimitive.top; ret = TypePrimitive.bot }

  let join ctx left right =
    let param = Typing2.meet ctx left.param right.param in
    let ret = Typing2.join ctx left.ret right.ret in
    { param; ret }

  let meet ctx left right =
    let param = Typing2.join ctx left.param right.param in
    let ret = Typing2.meet ctx left.ret right.ret in
    { param; ret }
end

module SearchApp = Searcher(SearcherApp)

module SearcherAppType = struct

  type t = { param: Type.param; ret: Type.type' }

  let bot = { param = make_anonymous_param TypePrimitive.top; ret = TypePrimitive.bot }

  let with_merge_param ctx bound left right f =
    let param = make_anonymous_param bound in
    let left_ret  = Typing2.substitute_body ctx param left.param  left.ret  in
    let right_ret = Typing2.substitute_body ctx param right.param right.ret in
    let ctx = TypeContext.add_bind_type ctx param.bind param.bound in
    let ret = f ctx left_ret right_ret in
    { param; ret }

  let join ctx left right =
    let bound = Typing2.meet ctx left.param.bound right.param.bound in
    with_merge_param ctx bound left right Typing2.join

  let meet ctx left right =
    if not (Typing2.is_param ctx left.param right.param) then
      bot
    else
    with_merge_param ctx left.param.bound left right Typing2.meet
end

module SearchAppType = Searcher(SearcherAppType)
