type entry = {
  bind: Abt.bind_type;
  bound: Type.type';
}

let entry (bind: Abt.bind_type) (bound: Type.type') =
  { bind; bound }

let entry_param (left: Type.param) (right: Type.param) =
  { bind = right.bind; bound = Type.var left.bind }

let is_bind entry bind =
  entry.bind == bind

type context = {
  assumptions: entry list;
}

let empty = { assumptions = [] }

let get_bind_type ctx bind =
  let entry = List.find (Utils.flip is_bind bind) ctx.assumptions in
  entry.bound

let add_bind_type ctx bind bound =
  { assumptions = { bind; bound } :: ctx.assumptions }
