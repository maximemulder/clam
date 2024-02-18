module Display = Display
module Error   = Error
module Value   = Value

exception Error = Error.Error

(**
  Evaluate a program.
*)
let eval def defs primitives writer =
  let _ = Walk.eval_def def defs primitives writer in ()
