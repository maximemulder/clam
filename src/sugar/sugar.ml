open Abt

exception Error = Errors.Error

module Errors = Errors

(**
  Modelize a program, building an abstract biding tree from an abstract syntax tree.
*)
let desugar ast primitives =
  let (types, all_types) = Types.modelize_program ast in
  let (exprs, types) = Exprs.modelize_program ast types all_types primitives in
  { types; exprs }
