{
  open Parser

  exception SyntaxError of string
}

let int = ['0'-'9']+
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let white = [' ' '\t' '\r' '\n']+

rule read =
  parse
  | int      { INT (Lexing.lexeme lexbuf) }
  | white    { read lexbuf }
  | "as"     { AS }
  | "def"    { DEF }
  | "else"   { ELSE }
  | "false"  { FALSE }
  | "if"     { IF }
  | "then"   { THEN }
  | "true"   { TRUE }
  | "type"   { TYPE }
  | "void"   { VOID }
  | ident    { IDENT (Lexing.lexeme lexbuf) }
  | '&'      { AMPERSAND }
  | "->"     { ARROW }
  | '@'      { AT }
  | '{'      { BRACE_LEFT }
  | '}'      { BRACE_RIGHT }
  | '['      { CROTCHET_LEFT }
  | ']'      { CROTCHET_RIGHT }
  | ':'      { COLON }
  | ','      { COMMA }
  | "="      { EQUAL }
  | "-"      { MINUS }
  | '('      { PARENTHESIS_LEFT }
  | ')'      { PARENTHESIS_RIGHT }
  | '|'      { PIPE }
  | "+"      { PLUS }
  | ';'      { SEMICOLON }
  | '\''     { read_char (Buffer.create 2) lexbuf }
  | '"'      { read_string (Buffer.create 16) lexbuf }
  | _        { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }

and read_char buf =
  parse
  | '\\' '/'  { Buffer.add_char buf '/'; read_char_end buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_char_end buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_char_end buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_char_end buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_char_end buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_char_end buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_char_end buf lexbuf }
  | [^ '\'' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_char_end buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal char character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("Char is not terminated")) }

and read_char_end buf =
  parse
  | '\'' { CHAR (Buffer.contents buf) }
  | _    { raise (SyntaxError ("Invalid char character: ")) }
  | eof  { raise (SyntaxError ("Char is not terminated")) }

and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }
