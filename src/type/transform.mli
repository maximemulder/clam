(**
  [map f type']

  Map over [type'] by applying [f] to each of its components.

  Usually, [map] should be called in a recursive function that handles the
  special cases and calls [map] with itself, possibly partially applied, as an
  argument to handle the other cases where no special treatement is needed.
*)
val map : (Node.type' -> Node.type') -> Node.type' -> Node.type'

(**
  [fold f1 f2 acc type']

  Fold over [type'] by mapping its components with [f1] and folding the results
  with [f2] and [acc].

  Usually, [fold] should be called in a recursive function that handles the
  special cases and calls [fold] with itself, possibly partially applied, as an
  argument to handle the other cases where no special treatement is needed.
*)
val fold : (Node.type' -> 'a) -> ('a -> 'a -> 'a) -> 'a -> Node.type' -> 'a
