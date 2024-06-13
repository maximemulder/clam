(**
  This module provides generic helpers for syntactic transformations on types.
*)

(**
  [map f type']

  Map over [type'] by applying [f] to each of its components.

  Usually, [map] should be called in a recursive function that handles the
  special cases and calls [map] with itself, possibly partially applied, as an
  argument to handle the other cases where no special treatement is needed.
*)
val map : (Abt.Type.type' -> Abt.Type.type') -> Abt.Type.type' -> Abt.Type.type'

(**
  [fold f1 f2 acc type']

  Map over the components of [type'] using [f1], and fold the results with [f2]
  starting with [acc].

  Usually, [fold] should be called in a recursive function that handles the
  special cases and calls [fold] with itself, possibly partially applied, as an
  argument to handle the other cases where no special treatement is needed.
*)
val fold : (Abt.Type.type' -> 'a) -> ('a -> 'a -> 'a) -> 'a -> Abt.Type.type' -> 'a

(**
  [fold f1 f2 acc type']

  Map over the components of [type'] using [f1] while tracking the type
  polarity starting with [pol], and fold the results with [f2] starting with
  [acc].

  Usually, [fold_pol] should be called in a recursive function that handles the
  special cases and calls [fold] with itself, possibly partially applied, as an
  argument to handle the other cases where no special treatement is needed.
*)
val fold_pol : (Pol.pol -> Abt.Type.type' -> 'a) -> ('a -> 'a -> 'a) -> 'a -> Pol.pol -> Abt.Type.type' -> 'a
