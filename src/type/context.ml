open Node

type entry = {
  bind: Abt.bind_type;
  lower: type';
  upper: type';
}

type context = {
  assumptions: entry list;
}

let empty = { assumptions = [] }

let add_bounds ctx bind lower upper =
  { assumptions = { bind; lower; upper } :: ctx.assumptions }

let add_param ctx param =
  { assumptions = { bind = param.Node.bind; lower = param.lower; upper = param.upper } :: ctx.assumptions }

let get_bounds ctx bind =
  let entry = List.find (fun entry -> entry.bind == bind) ctx.assumptions in
  entry.lower, entry.upper

let display_entry entry =
  entry.bind.name ^ ": " ^
  let lower = Compare.compare entry.lower Node.bot in
  let upper = Compare.compare entry.upper Node.bot in
  (if lower then Display.display entry.lower else "") ^
  (if lower || upper then " < " else "") ^
  (if upper then Display.display entry.upper else "")

let display ctx =
  let entries = List.map display_entry ctx.assumptions in
  String.concat ", " entries
