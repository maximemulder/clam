(**
  Rename a variable in a type, replacing all occurrences of one binding with
  another binding. This operation is performed via simple substitution, with no
  bound-checking or simplification performed, and should generally only be
  performed when the bindings are equivalent.
*)
val rename: Abt.Type.bind_type -> Abt.Type.bind_type -> Abt.Type.type' -> Abt.Type.type'
