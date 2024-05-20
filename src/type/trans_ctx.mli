(**
  This module provides generic helpers for contextual transformations on types.
*)

(**
  [map f type']

  Map over [type'] by applying [f] to each of its components in the given
  typing context.

  Usually, [map] should be called in a recursive function that handles the
  special cases and calls [map] with itself, possibly partially applied, as
  an argument to handle the other cases where no special treatement is needed.
*)
val map : (Node.type' -> Node.type' Context.Monad.t) -> Node.type' -> Node.type' Context.Monad.t

(**
  [map_pol f pol type']

  Map over [type'] by applying [f] to each of its components while tracking the
  type polarity starting with [pol] in the given typing context.

  Usually, [map_pol] should be called in a recursive function that handles the
  special cases and calls [map_pol] with itself, possibly partially applied, as
  an argument to handle the other cases where no special treatement is needed.
*)
val map_pol : (Pol.pol -> Node.type' -> Node.type' Context.Monad.t) -> Pol.pol -> Node.type' -> Node.type' Context.Monad.t
