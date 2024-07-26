type ident = {
  name: string;
  index: int;
}

val new_ident : string -> ident

val new_ident_anon : unit -> ident
