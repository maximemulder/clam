open Context.Monad

(** Update the context to ensure all the type variables present in a given type
  appear before the given fresh type variable.*)
val levelize : Context.fresh -> Node.type' -> unit t

(** Check if any fresh type variable appears in a given type. *)
val appears_fresh : Node.type' -> bool t

(** Check if any fresh type variable appears in a given base type. *)
val appears_fresh_base : Node.base -> bool t

(** Check if a type variable appears in a given type. *)
val appears : Abt.bind_type -> Node.type' -> bool t
