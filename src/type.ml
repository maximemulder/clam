type type' =
  | TypeVoid
  | TypeInt
  | TypeChar
  | TypeString
  | TypeFun    of (type' list) * type'
  | TypeTuple  of type' list
  | TypeRecord of attr list
  | TypeInter  of type' * type'
  | TypeUnion  of type' * type'
  | TypeAbs    of (param list) * type'
  | TypeApp    of type' * (type' list)

and attr = {
  attr_name: string;
  attr_type: type';
}

and param = {
  param_name: string;
  param_type: type';
}
