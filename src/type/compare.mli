(**
  Compare two types syntactically, returning [true] only if they are exactly
  identical. Two equivalent types may not be syntactically identical, and may
  therefore be considered different by this function.
*)
val compare : Node.type' -> Node.type' -> bool
