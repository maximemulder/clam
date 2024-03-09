open Abt

exception Error = Errors.Error

module Errors = Errors

(**
  Modelize a program, building an abstract biding tree from an abstract syntax tree.
*)
let desugar ast primitives =
  let primitives = Util.NameMap.of_list primitives in
  let state = State.make_state ast primitives in
  let types, state = Types.desugar_program state in
  let exprs, _ = Exprs.desugar_program state in
  { types; exprs }
