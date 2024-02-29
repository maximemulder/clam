module Error = Error

exception Error = Error.Error

include Rename

(**
  Type check a program.
*)
let check abt primitives =
  let _ = Walk.check_types abt.Abt.types in
  Walk.check_defs abt.exprs primitives
  |> List.filter (fun def -> not(List.exists (fun primitive -> fst primitive == fst def) primitives))
  |> List.sort (fun (a, _) (b, _) ->
    Option.get (List.find_index (fun (def: Abt.def_expr) -> def.bind.id = a.Abt.id) abt.exprs) -
    Option.get (List.find_index (fun (def: Abt.def_expr) -> def.bind.id = b.Abt.id) abt.exprs)
  )
