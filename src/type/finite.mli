(**
  Check that a recursive typed is well-formed, that is, that it has a base
  case.
*)
val check : Node.type' -> Abt.bind_type list -> bool
