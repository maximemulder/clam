(**
  Transitively lower the level of the variables contained in a type to the
  given level.
*)
val levelize : int -> Type.type' -> unit State.t
