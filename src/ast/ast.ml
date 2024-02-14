include Display
include Node
include Span

let get_program_types program =
  List.filter_map (fun def -> match def with
    | DefType type' -> Some type'
    | DefExpr _     -> None
  ) program.defs

let get_program_exprs program =
  List.filter_map (fun def -> match def with
    | DefType _    -> None
    | DefExpr expr -> Some expr
  ) program.defs
