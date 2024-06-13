(**
  [validate type']

  Check that the type [type'] is well-formed in a given context, and throw an
  exception otherwise.

  The checks performed to ensure a type is well-formed are the following:
  - Tuples, records, functions, and universal types must have proper types as
    their components (only required in the return type for the latter).
  - Type applications must be applied to the right kinds, that is, to type
    abstractions whose bounds correspond to the type argument.
  - Unions and intersections must have components of the same kind.
  - Recursive types must have a path for terminatation.
*)
val validate : Abt.Type.type' -> unit Context.Monad.t

(**
  [validate_param param]

  Check that the type parameter [param] is well-formed in a given context, and
  throw an exception otherwise.
*)
val validate_param : Abt.Type.param -> unit Context.Monad.t

(**
  [validate_proper type']

  Check that the type [type'] is well-formed and a proper type in a given
  context, and throw an exception otherwise.
*)
val validate_proper : Abt.Type.type' -> unit Context.Monad.t
