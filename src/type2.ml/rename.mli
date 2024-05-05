(**
  Rename a variable inside a type. This operation is performed via simple
  substitution, with no simplification or bound-checking performed.
*)
val rename: Abt.bind_type -> Abt.bind_type -> Node.type' -> Node.type'
