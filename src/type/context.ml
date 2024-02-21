open Node

type entry = {
  bind: Abt.bind_type;
  bound: type';
}

let entry bind bound =
  { bind; bound }

let entry_param (left: param) (right: param) =
  { bind = right.bind; bound = var left.bind }

let is_bind entry bind =
  entry.bind == bind

type context = {
  assumptions: entry list;
}

let empty = { assumptions = [] }

let get_bind_type ctx bind =
  let entry = List.find (Util.flip is_bind bind) ctx.assumptions in
  entry.bound

let add_bind_type ctx bind bound =
  { assumptions = { bind; bound } :: ctx.assumptions }
