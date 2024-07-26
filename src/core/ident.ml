type ident = {
  name: string;
  index: int;
}

let counter = ref 0

let new_index () =
  let index = !counter in
  counter := index + 1;
  index

let new_ident name =
  let index = new_index () in
  { name; index }

let new_ident_anon () =
  let index = new_index () in
  { name = ""; index }
