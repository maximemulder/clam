(* RUN CONFIGURATION *)

type config = {
  file: string option;
  help: bool;
  show_ast: bool;
  show_types: bool;
  show_values: bool;
}

let empty_config = {
  file = None;
  help = false;
  show_ast = false;
  show_types = false;
  show_values = false;
}

(* SETUP RUN CONFIGURATION *)

let configure_help config =
  { config with help = true }

let configure_show_ast config =
  { config with show_ast = true }

let configure_show_types config =
  { config with show_types = true }

let configure_show_values config =
  { config with show_values = true }

let configure_default arg config =
  if String.starts_with ~prefix:"-" arg then
    Failure.raise ("Unknown option '" ^ arg ^ "'.")
  else if Option.is_some config.file then
    Failure.raise "Duplicate definition of <file>."
  else
    { config with file = Some arg }

(* PARSE ARGUMENTS *)

let parse_arg arg =
  match arg with
  | "-h"   | "--help"        -> configure_help
  | "-ast" | "--show-ast"    -> configure_show_ast
  | "-t"   | "--show-types"  -> configure_show_types
  | "-v"   | "--show-values" -> configure_show_values
  | _                        -> configure_default arg

let parse_args args =
  List.fold_left (Util.flip parse_arg) empty_config args
