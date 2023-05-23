open Model

type context = {
  parent: context option;
  params: (param_type * type') list;
}

let empty_context = { parent = None; params = [] }
