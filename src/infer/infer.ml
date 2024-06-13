module Error = Error

exception Error = Error.Error

(**
  Type check a program.
*)
let check abt primitives =
  let kinds = Walk.check_types abt.Abt.Program.types in
  kinds, Walk.check_defs abt.exprs primitives
  |> List.filter (fun def -> not(List.exists (fun primitive -> fst primitive == fst def) primitives))
  |> List.sort (fun (a, _) (b, _) ->
    Option.get (List.find_index (fun (def: Abt.Program.def_expr) -> def.bind.id = a.Abt.Expr.id) abt.exprs) -
    Option.get (List.find_index (fun (def: Abt.Program.def_expr) -> def.bind.id = b.Abt.Expr.id) abt.exprs)
  )
