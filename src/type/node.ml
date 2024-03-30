type type' =
  (* Disjunctive normal form *)
  | Dnf of base list list
  (* Conjunctive normal form *)
  | Cnf of base list list

and base =
  | Top
  | Bot
  | Unit
  | Bool
  | Int
  | String
  | Var    of var
  | Tuple  of tuple
  | Record of record
  | Lam    of lam
  | Univ   of univ
  | Abs    of abs
  | App    of app

and var = {
  bind: Abt.bind_type;
}

and tuple = {
  elems: type' list;
}

and record = {
  attrs: attr Util.NameMap.t;
}

and lam = {
  param: type';
  ret: type';
}

and univ = {
  param: param;
  ret: type';
}

and abs = {
  param: param;
  body: type';
}

and app = {
  abs: type';
  arg: type';
}

and attr = {
  label: String.t;
  type': type';
}

and param = {
  bind: Abt.bind_type;
  lower: type';
  upper: type';
}

let base base = Dnf [[base]]

let top               = base Top
let bot               = base Bot
let unit              = base Unit
let bool              = base Bool
let int               = base Int
let string            = base String
let var    bind       = base (Var { bind })
let tuple  elems      = base (Tuple { elems })
let record attrs      = base (Record { attrs })
let lam    param ret  = base (Lam { param; ret })
let univ   param ret  = base (Univ { param; ret })
let abs    param body = base (Abs { param; body })
let app    abs arg    = base (App { abs; arg })
