type entry = {
  bind: Model.bind_type;
  type': Type.type';
}

let entry (bind: Model.bind_type) (type': Type.type') =
  { bind; type' }

let entry_param pos (left: Type.param) (right: Type.param) =
  { bind = right.bind; type' = Type.base (Var { pos; bind = left.bind }) }

let is_bind entry bind =
  entry.bind = bind

type context = {
  assumptions: entry list;
}

let get_bind_type context bind =
  let entry = List.find (Utils.flip is_bind bind) context.assumptions in
  entry.type'

let with_bind_type context bind type' f =
  f { assumptions = {bind; type'} :: context.assumptions }
