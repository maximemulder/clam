open Abt

(**
  Type check a program.
*)
let check abt primitives =
  let _ = TypeInfer.check_types abt.types in
  TypeInfer.check_defs abt.exprs primitives
  |> List.filter (fun def -> not(List.exists (fun primitive -> fst primitive == fst def) primitives))
