open Ast
open Ident

let ident_unit  = new_ident "Unit"
let ident_true  = new_ident "True"
let ident_false = new_ident "False"
let ident_bool  = new_ident "Bool"

let var_unit span =
  Var { span; ident = ident_unit }

let var_true span =
  Var { span; ident = ident_true }

let var_false span =
  Var { span; ident = ident_false }

let var_bool span =
  Var { span; ident = ident_bool }
