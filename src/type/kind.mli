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
  ret: kind;
}

(**
  Get the kind of a type in a given context.
*)
val get_kind : Context.context -> Node.type' -> kind

(**
  Get the kind of a type base in a given context.
*)
val get_kind_base : Context.context -> Node.base -> kind

(**
  Get the maximal type of a given kind, that is, the type that is a supertype
  of all other types of this kind.
*)
val get_kind_max : Context.context -> kind -> Node.type'

(**
  Get the minimal type of a given kind, that is, the type that is a subtype of
  all other types of this kind.
*)
val get_kind_min : Context.context -> kind -> Node.type'

(**
  Convert a kind to a string.
*)
val display : kind -> string
