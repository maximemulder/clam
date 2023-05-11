let modelize (program: Ast.program) =
  let types = Modelize_types.modelize_program program in
  Modelize_exprs.modelize_program program types
