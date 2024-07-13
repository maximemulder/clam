(**
  This module implements the algorithms that form the core of Clam's type
  system.

  These functions are all located in a single module because they often perform
  type simplification, which requires them to be mutually recursive.
  Theoretically, these functions could also work without type simplification,
  but they would then produce more complex types.
*)

(**
  [map_set f type']

  Apply [f] to [type'], or to its components if [type'] is an union or
  intersection, and simplify the result.
*)
val map_set : (Abt.Type.type' -> Abt.Type.type' Context.Monad.t) -> Abt.Type.type' -> Abt.Type.type' Context.Monad.t

(**
  [substitute bind other type']

  Substitute all occurences of the variable [bind] by the type [other] in the
  type [type'].

  Requirements:
  - [other] should be a valid substitute for [bind].
*)
val substitute : Abt.Type.bind_type -> Abt.Type.type' -> Abt.Type.type' -> Abt.Type.type' Context.Monad.t

(**
  [compute abs arg]

  Apply the type argument [arg] to the type abstraction [abs], and simplify the
  result.

  Requirements:
  - [abs] must be a valid type abstraction.
  - [arg] should be a valid type argument for [abs].
*)
val compute : Abt.Type.type' -> Abt.Type.type' option Context.Monad.t

(**
  [promote_lower type']

  Return the highest subtype of [type'] whose shape is not a type variable.
*)
val promote_lower : Abt.Type.type' -> Abt.Type.type' Context.Monad.t

(**
  [promote_lower type']

  Return the lowest supertype of [type'] whose shape is not a type variable.
*)
val promote_upper : Abt.Type.type' -> Abt.Type.type' Context.Monad.t

(**
  [is left right]

  Constrain the typing context such that, if possible, the type [left] is
  equivalent of [right]. Two types are equivalent when they are subtype of
  each other.
*)
val is : Abt.Type.type' -> Abt.Type.type' -> bool Context.Monad.t

(**
  [isa sub sup]

  Constrain the typing context such that, if possible, the type [sub] is a
  subtype of [sup].
*)
val isa : Abt.Type.type' -> Abt.Type.type' -> bool Context.Monad.t

(**
  [join left right]

  Return the lowest common supertype of [left] and [right], which is equivalent
  to the union of these types.

  Requirements:
  - [left] and [right] should be of the same kind.
*)
val join : Abt.Type.type' -> Abt.Type.type' -> Abt.Type.type' Context.Monad.t

(**
  [meet left right]

  Return the highest common subtype of [left] and [right], which is equivalent
  to the intersection of these types.

  Requirements:
  - [left] and [right] should be of the same kind.

  Notes:
  - Incompatible type constructors are not coerced into [Bot] in Clam.
*)
val meet : Abt.Type.type' -> Abt.Type.type' -> Abt.Type.type' Context.Monad.t

(**
  [is_kind left right]

  Check whether [left] and [right] are equivalent kinds or not.
*)
val is_kind : Kind.kind -> Kind.kind -> bool Context.Monad.t

(**
  [is_proper type']

  Check whether [type'] is a proper type or not.
*)
val is_proper : Abt.Type.type' -> bool Context.Monad.t
