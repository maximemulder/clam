open Model

(** Promotes a type to its least supertype that is not a type variable. *)
val promote : type' -> type'

(** Promotes a type variable to its least supertype that is not a type variable. *)
val promote_var : type_var -> type'

(** Determines if a type is equivalent to the bottom type. *)
val type_is_bot : type' -> bool

(** Determines if a type variable is equivalent to the bottom type. *)
val var_is_bot : type_var -> bool

(** Determines if two types are equivalent. *)
val is : type' -> type' -> bool

(** Determines whether two type parameters are equivalent. The names of the parameters are
  ignored, only their bounds are checked invariantly. *)
val is_param : param_type -> param_type -> bool

(** Determines if the left type is a subtype of the right type. *)
val isa : type' -> type' -> bool

(** Normalizes a type by transforming its unions and intersections into their disjunctive normal
  form. *)
val normalize : type' -> type'

(** Decomposes two types and combines them into their simplified lowest common supertype, which
  is either the more general input type, or the union of these types. *)
val join : type' -> type' -> type'

(** Combines two types into their simplified lowest common supertype, which is either the more
  general input type, or the union of these types. *)
val join_type : type' -> type' -> type'

(** Decomposes two types and combines them simplified highest common subtype. *)
val meet : type' -> type' -> type'

(** Combines two types into their simplified highest common subtype. Input interesections are not
  decomposed. *)
val meet_type : type' -> type' -> type'
