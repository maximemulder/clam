
(**
  [map f type']

  Map over [type'] by applying [f] to each of its components, and provides the
  type context to this function.

  Usually, [map] should be called in a recursive function that handles the
  special cases and calls [map] with itself, possibly partially applied, as
  an argument to handle the other cases where no special treatement is needed.
*)
val map : (Node.type' -> Node.type' Context.Monad.t) -> Node.type' -> Node.type' Context.Monad.t
