(**
  The kind of a type, which is either a proper type or unary.
*)
type kind =
  | Type
  | Abs of abs

(**
  A higher kind, which has both a type interval for its parameter, and a return
  kind.
*)
and abs = {
  lower: Node.type';
  upper: Node.type';
  body: kind;
}

(**
  Get the kind of a type in a given context.
*)
val get_kind : Node.type' -> kind Context2.Monad.t

(**
  Get the kind of a type base in a given context.
*)
val get_kind_base : Node.base -> kind Context2.Monad.t

(**
  Get the maximal type of a given kind, that is, the type that is a supertype
  of all other types of this kind.
*)
val get_kind_max : kind -> Node.type'

(**
  Get the minimal type of a given kind, that is, the type that is a subtype of
  all other types of this kind.
*)
val get_kind_min : kind -> Node.type'

(**
  Convert a kind to a string.
*)
val display : kind -> string
