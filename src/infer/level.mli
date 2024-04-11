(*(**
  Transitively lower the variables in a type to before a given variable.
*)
val levelize : Abt.bind_type -> Type.type' -> unit State.t

(**
  Get the level of the highest inference variable in the type, if there is one
*)
val get_level : Type.type' -> int option State.t
*)