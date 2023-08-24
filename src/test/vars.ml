open Clam.Model

let pos = prim_pos

let var name =
  TypeVar { pos; param = { name; type' = Clam.Model.prim_top  }}

let union left right =
  TypeUnion { pos; left; right }

let inter left right =
  TypeInter { pos; left; right }

let a = var "A"
let b = var "B"
let c = var "C"
let d = var "D"
