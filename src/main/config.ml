open Clam

(* RUN CONFIGURATION *)

type config = {
  file: string option;
  interactive: bool;
  types: bool;
  values: bool;
  help: bool;
}

let empty_config = {
  file = None;
  interactive = false;
  types = false;
  values = false;
  help = false;
}

(* SETUP RUN CONFIGURATION *)

let configure_help config =
  { config with help = true }

let configure_interactive config =
  if config.interactive then
    Failure.raise "Duplicate definition of '--interactive'."
  else
    { config with interactive = true }

let configure_types config =
  if config.types then
    Failure.raise "Duplicate definition of '--types'."
  else
    { config with types = true }

let configure_values config =
  if config.values then
    Failure.raise "Duplicate definition of '--values'."
  else
    { config with values = true }

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
  | "-h" | "--help" ->
    configure_help
  | "-i" | "--interactive" ->
    configure_interactive
  | "-t" | "--types" ->
    configure_types
  | "-v" | "--values" ->
    configure_values
  | _ ->
    configure_default arg

let parse_args args =
  List.fold_left (Utils.flip parse_arg) empty_config args
