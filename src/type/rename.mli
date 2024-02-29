(**
  Rename a variable inside a type. This operation is performed via
  substitution, however, the two variables should be equivalent as
  no simplification or bound-checking is performed.
*)
val rename: Node.type' -> Abt.bind_type -> Abt.bind_type -> Node.type'
