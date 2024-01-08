open Utils
open RuntimeValue

let rec display value =
  match value with
  | VUnit -> "unit"
  | VBool bool -> string_of_bool bool
  | VInt int -> string_of_int int
  | VString string -> "\"" ^ (String.escaped string) ^ "\""
  | VTuple values ->
    let values = List.map display values in
    "(" ^ String.concat ", " values ^ ")"
  | VRecord attrs ->
    let attrs = List.of_seq (NameMap.to_seq attrs) in
    let attrs = List.map (fun (name, value) -> name ^ " = " ^ (display value)) attrs in
    "{" ^ String.concat ", " attrs ^ "}"
  | VExprAbs _ -> "<function>"
