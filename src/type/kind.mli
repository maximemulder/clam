(**
  The kind of a type, which is either a proper type or a higher-kinded type.
*)
type kind =
  | Proper
  | Higher of higher

(**
  A higher kind, which has lower and upper bounds for its parameters and a
  return kind.
*)
and higher = {
  lower: Abt.Type.type';
  upper: Abt.Type.type';
  body: kind;
}

(**
  [get_kind type']

  Get the kind of the type [type'] in a given context.

  This function can return `None` if [type'] is .
*)
val get_kind : Abt.Type.type' -> kind Context.Monad.t

(**
  Get the minimal type of a given kind, that is, the type that is a subtype of
  all other types of this kind.
*)
val get_kind_min : Code.span -> kind -> Abt.Type.type'

(**
  Get the maximal type of a given kind, that is, the type that is a supertype
  of all other types of this kind.
*)
val get_kind_max : Code.span -> kind -> Abt.Type.type'

(**
  Convert a kind to its string representation.
*)
val display_kind : kind -> string
