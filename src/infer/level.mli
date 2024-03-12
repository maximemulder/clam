(**
  Transitively lower the variables in a type to before a given variable.
*)
val levelize : Abt.bind_type -> Type.type' -> unit State.t
