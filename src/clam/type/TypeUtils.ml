let rec contains bind (type': Type.type') =
  contains_union bind type'

and contains_union bind union =
  List.exists (contains_inter bind) union.union

and contains_inter bind inter =
  List.exists (contains_base bind) inter.inter

and contains_base bind type' =
  match type' with
  | Var var -> var.bind == bind
  | Tuple tuple ->
    List.exists (contains bind) tuple.elems
  | Record record ->
    Util.NameMap.exists (contains_attr bind) record.attrs
  | AbsExpr abs ->
    contains bind abs.param || contains bind abs.ret
  | AbsTypeExpr abs ->
    contains_param bind abs.param || contains bind abs.ret
  | Abs abs ->
    contains_param bind abs.param || contains bind abs.body
  | App app ->
    contains bind app.abs || contains bind app.arg
  | _ -> false

and contains_param bind param =
  contains bind param.bound

and contains_attr bind _ attr =
  contains bind attr.type'

let contains type' bind =
  contains bind type'
