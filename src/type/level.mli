
(**
  Update the context to ensure all the type variables that appear in a given
  type appear before the given fresh type variable.
*)
val levelize : Context.fresh -> Node.type' -> unit Context.Monad.t
