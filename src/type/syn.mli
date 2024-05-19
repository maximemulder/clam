(**
  This module provides generic helpers for syntactic transformations on types.
*)

(**
  [syn_map f type']

  Map over [type'] by applying [f] to each of its components.

  Usually, [syn_map] should be called in a recursive function that handles the
  special cases and calls [syn_map] with itself, possibly partially applied, as an
  argument to handle the other cases where no special treatement is needed.
*)
val syn_map : (Node.type' -> Node.type') -> Node.type' -> Node.type'

(**
  [syn_fold f1 f2 acc type']

  Map over the components of [type'] using [f1], and fold the results with [f2]
  starting with [acc].

  Usually, [syn_fold] should be called in a recursive function that handles the
  special cases and calls [syn_fold] with itself, possibly partially applied, as an
  argument to handle the other cases where no special treatement is needed.
*)
val syn_fold : (Node.type' -> 'a) -> ('a -> 'a -> 'a) -> 'a -> Node.type' -> 'a

(**
  [syn_fold f1 f2 acc type']

  Map over the components of [type'] using [f1] while tracking the type
  polarity starting with [pol], and fold the results with [f2] starting with
  [acc].

  Usually, [syn_fold_pol] should be called in a recursive function that handles
  the special cases and calls [syn_fold] with itself, possibly partially
  applied, as an argument to handle the other cases where no special treatement
  is needed.
*)
val syn_fold_pol : (Polar.pol -> Node.type' -> 'a) -> ('a -> 'a -> 'a) -> 'a -> Polar.pol -> Node.type' -> 'a
