type error = {
  message: string;
  span: Code.span;
}

exception Error of error

let raise message span =
  raise (Error { message; span })
