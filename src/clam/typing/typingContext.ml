open Model

type context = {
  parent: context option;
  entries: (param_type * type') list;
}

let empty = { parent = None; entries = [] }

let child parent entries = { parent = Some parent; entries }

let rec find_arg param context =
  let entry = List.find_opt (fun entry -> fst entry = param) context.entries in
  match entry with
  | Some entry -> Some (snd entry)
  | None ->
  match context.parent with
  | Some parent -> find_arg param parent
  | None -> None
