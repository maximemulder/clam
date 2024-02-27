open Abt

exception Error = Errors.Error

module Errors = Errors

(**
  Modelize a program, building an abstract biding tree from an abstract syntax tree.
*)
let desugar ast primitives =
  let (types, _all_types) = Types.desugar_program ast in
  let exprs = Exprs.desugar_program ast types (Util.NameMap.of_list primitives) in
  { types = []; exprs }
