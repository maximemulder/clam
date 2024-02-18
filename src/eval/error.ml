type error = {
  message: string;
}

exception Error of error

let raise message =
  raise (Error { message })

let raise_value name =
  raise ("Unexpected value type, expected " ^ name)
