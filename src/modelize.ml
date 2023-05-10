let modelize (program: Ast.program) =
  let state = Modelize_types.modelize_program program in
  let scope = Modelize_types.NameMap.fold (fun done' scope -> Scope.add_type done' scope) state.dones Scope.empty in
  Modelize_exprs.modelize_program program scope
