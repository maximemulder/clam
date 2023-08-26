{
  open Parser
  open Lexing

  exception Error of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_bol = lexbuf.lex_curr_pos;
      pos_lnum = pos.pos_lnum + 1;
    }
}

let int = ['0'-'9']+
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let space = [' ' '\t']+
let line = '\r' | '\n' | "\r\n"

rule read =
  parse
  | int      { INT (Lexing.lexeme lexbuf) }
  | space    { read lexbuf }
  | line     { next_line lexbuf; read lexbuf }
  | "def"    { DEF }
  | "else"   { ELSE }
  | "false"  { FALSE }
  | "if"     { IF }
  | "then"   { THEN }
  | "true"   { TRUE }
  | "type"   { TYPE }
  | "unit"   { UNIT }
  | "var"    { VAR }
  | ident    { IDENT (Lexing.lexeme lexbuf) }
  | "->"     { ARROW }
  | "++"     { CONCAT }
  | "=>"     { DOUBLE_ARROW }
  | "=="     { EQ }
  | ">="     { GE }
  | "<="     { LE }
  | "!="     { NE }
  | '&'      { AND }
  | '{'      { BRACE_LEFT }
  | '}'      { BRACE_RIGHT }
  | ':'      { COLON }
  | ','      { COMMA }
  | '['      { CROTCHET_LEFT }
  | ']'      { CROTCHET_RIGHT }
  | '/'      { DIV }
  | '.'      { DOT }
  | '>'      { GT }
  | '='      { ASSIGN }
  | '<'      { LT }
  | '-'      { MINUS }
  | '%'      { MOD }
  | '*'      { MUL }
  | '!'      { NOT }
  | '|'      { OR }
  | '('      { PARENTHESIS_LEFT }
  | ')'      { read_parenthesis_right lexbuf }
  | "+"      { PLUS }
  | ';'      { SEMICOLON }
  | '\''     { read_char (Buffer.create 2) lexbuf }
  | '"'      { read_string (Buffer.create 16) lexbuf }
  | _        { raise (Error ("unexpected character: `" ^ Lexing.lexeme lexbuf ^ "`")) }
  | eof      { EOF }

and read_parenthesis_right =
  parse
  | space { read_parenthesis_right lexbuf }
  | line  { next_line lexbuf; read_parenthesis_right lexbuf }
  | "->"  { PARENTHESIS_RIGHT_ARROW }
  | ""    { PARENTHESIS_RIGHT }

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
  | _ { raise (Error ("invalid literal character: `" ^ Lexing.lexeme lexbuf ^ "`")) }
  | eof { raise (Error ("character literal is not terminated")) }

and read_char_end buf =
  parse
  | '\'' { CHAR (Buffer.contents buf) }
  | _    { raise (Error ("invalid literal character: `" ^ Lexing.lexeme lexbuf ^ "`")) }
  | eof  { raise (Error ("character literal is not terminated")) }

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
  | _ { raise (Error ("invalid string character: `" ^ Lexing.lexeme lexbuf ^ "`")) }
  | eof { raise (Error ("string literal is not terminated")) }
