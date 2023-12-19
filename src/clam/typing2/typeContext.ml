type entry = {
  bind: Model.bind_type;
  type': Type.type';
}

let entry (bind: Model.bind_type) (type': Type.type') =
  { bind; type' }

let entry_param (left: Type.param) (right: Type.param) =
  { bind = right.bind; type' = Type.base (Var { bind = left.bind }) }

let is_bind entry bind =
  entry.bind = bind

type context = {
  assumptions: entry list;
}

let empty = { assumptions = [] }

let get_bind_type ctx bind =
  let entry = List.find (Utils.flip is_bind bind) ctx.assumptions in
  entry.type'

let add_bind_type ctx bind type' =
  { assumptions = { bind; type' } :: ctx.assumptions }
