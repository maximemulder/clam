(* RUN CONFIGURATION *)

type config = {
  file        : string option;
  help        : bool;
  show_ast    : bool;
  show_kinds  : bool;
  show_types  : bool;
  show_values : bool;
  debug_infer : bool;
}

let empty_config = {
  file        = None;
  help        = false;
  show_ast    = false;
  show_kinds  = false;
  show_types  = false;
  show_values = false;
  debug_infer = false;
}

(* SETUP RUN CONFIGURATION *)

let configure_default arg config =
  if String.starts_with ~prefix:"-" arg then
    Failure.raise ("Unknown option '" ^ arg ^ "'.")
  else if Option.is_some config.file then
    Failure.raise "Duplicate definition of <file>."
  else
    { config with file = Some arg }

(* PARSE ARGUMENTS *)

let parse_arg arg config =
  match arg with
  | "-h"   | "--help" ->
    { config with help = true }
  | "-ast" | "--show-ast" ->
    { config with show_ast = true }
  | "-k"   | "--show-kinds" ->
    { config with show_kinds = true }
  | "-t"   | "--show-types" ->
    { config with show_types = true }
  | "-v"   | "--show-values" ->
    { config with show_values = true }
  | "--debug-infer" ->
    { config with debug_infer = true }
  | _ ->
    if String.starts_with ~prefix:"-" arg then
      Failure.raise ("Unknown option '" ^ arg ^ "'.")
    else if Option.is_some config.file then
      Failure.raise "Duplicate definition of <file>."
    else
      { config with file = Some arg }

let parse_args args =
  List.fold_left (Util.flip parse_arg) empty_config args
