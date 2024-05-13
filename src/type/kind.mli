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
  lower: Node.type';
  upper: Node.type';
  body: kind;
}

(**
  Get the kind of a type in a given context.
*)
val get_kind : Node.type' -> kind Context.Monad.t

(**
  Get the minimal type of a given kind, that is, the type that is a subtype of
  all other types of this kind.
*)
val get_kind_min : kind -> Node.type'

(**
  Get the maximal type of a given kind, that is, the type that is a supertype
  of all other types of this kind.
*)
val get_kind_max : kind -> Node.type'

(**
  Convert a kind to its string representation.
*)
val display : kind -> string
