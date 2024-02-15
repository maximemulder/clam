open Code
include Error

let parse code =
  let lexbuf = Lexing.from_string code.text in
  try
    Global.code := Some code;
    Grammar.program Lexer.read lexbuf
  with
  | Lexer.Error _ ->
    Error.raise_lexer code lexbuf
  | Grammar.Error ->
    Error.raise_parser code lexbuf
