open Type

let rec substitute type' bind other =
  substitute_union type' bind other

and substitute_union type' bind other =
  { union = List.map (fun type' -> substitute_inter type' bind other) type'.union }

and substitute_inter type' bind other =
  { inter = List.map (fun type' -> substitute_base type' bind other) type'.inter }

and substitute_base type' bind other =
  match type' with
  | Top    -> Top
  | Bot    -> Bot
  | Unit   -> Unit
  | Bool   -> Bool
  | Int    -> Int
  | Char   -> Char
  | String -> String
  | Var var ->
    if var.bind == bind then
      other
    else
      (Var var)
  | Tuple tuple ->
    let elems = List.map (fun elem -> substitute elem bind other) tuple.elems in
    Tuple { elems }
  | Record record ->
    let attrs = Utils.NameMap.map (fun attr -> substitute_attr attr bind other) record.attrs in
    Record { attrs }
  | AbsExpr abs ->
    let param = substitute abs.param bind other in
    let ret = substitute abs.ret bind other in
    AbsExpr { param; ret }
  | AbsTypeExpr abs ->
    let param = substitute_param abs.param bind other in
    let ret = substitute abs.ret bind other in
    AbsTypeExpr { param; ret }
  | Abs abs ->
    let param = substitute_param abs.param bind other in
    let body = substitute abs.body bind other in
    Abs { param; body }
  | App app ->
    let abs = substitute app.abs bind other in
    let arg = substitute app.arg bind other in
    App { abs; arg }

and substitute_param param bind other =
  { bind = param.bind; bound = substitute param.bound bind other }

and substitute_attr attr bind other =
  { name = attr.name; type' = substitute attr.type' bind other }

let rec compare (left: type') (right: type') =
  compare_union left right

and compare_union left right =
  List.equal compare_inter left.union right.union

and compare_inter left right =
  List.equal compare_base left.inter right.inter

and compare_base left right =
  match left, right with
  | Top    , Top    -> true
  | Bot    , Bot    -> true
  | Unit   , Unit   -> true
  | Bool   , Bool   -> true
  | Int    , Int    -> true
  | Char   , Char   -> true
  | String , String -> true
  | Var left_var, Var right_var ->
    left_var.bind == right_var.bind
  | Tuple left_tuple, Tuple right_tuple ->
    Utils.compare_lists compare left_tuple.elems right_tuple.elems
  | Record left_record, Record right_record ->
    Utils.compare_maps compare_attr left_record.attrs right_record.attrs
  | AbsExpr left_abs, AbsExpr right_abs ->
    compare left_abs.param right_abs.param
    && compare left_abs.ret right_abs.ret
  | AbsTypeExpr left_abs, AbsTypeExpr right_abs ->
    compare_param left_abs.param right_abs.param
    && let right_ret = substitute right_abs.ret right_abs.param.bind (Var { bind = left_abs.param.bind }) in
    compare left_abs.ret right_ret
  | Abs left_abs, Abs right_abs ->
    compare_param left_abs.param right_abs.param
    && let right_body = substitute right_abs.body right_abs.param.bind (Var { bind = left_abs.param.bind }) in
    compare left_abs.body right_body
  | App left_app, App right_app ->
    compare left_app.abs right_app.abs
    && compare left_app.arg right_app.arg
  | _ -> false

and compare_param left_param right_param =
  left_param.bind.name == right_param.bind.name
  && compare left_param.bound right_param.bound

and compare_attr left_attr right_attr =
  compare left_attr.type' right_attr.type'
