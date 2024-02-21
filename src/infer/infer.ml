module Error = Error

exception Error = Error.Error

(**
  Type check a program.
*)
let check abt primitives =
  let _ = Walk.check_types abt.Abt.types in
  Walk.check_defs abt.exprs primitives
  |> List.filter (fun def -> not(List.exists (fun primitive -> fst primitive == fst def) primitives))
