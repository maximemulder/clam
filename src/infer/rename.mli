(**
  Rename the inference variables inside a type to a human-readable format. The
  function assumes the type is at the top level, so the first variable will
  always be renamed `'A`, the second `'B`...
*)
val rename: Type.type' -> Type.type'
