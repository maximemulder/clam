open Clam.Model

let pos = prim_pos

let var name =
  TypeVar { pos; param = { name; type' = Clam.Model.prim_top  }}

let union left right =
  TypeUnion { pos; left; right }

let inter left right =
  TypeInter { pos; left; right }

let prim_top    = prim_top
let prim_bot    = prim_bot
let prim_unit   = prim_unit
let prim_bool   = prim_bool
let prim_int    = prim_int
let prim_char   = prim_char
let prim_string = prim_string

let a = var "A"
let b = var "B"
let c = var "C"
let d = var "D"
