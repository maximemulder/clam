open Reflect

let rec indent buffer level =
  match level with
  | 0 ->
    Buffer.add_string buffer "\n"
  | _ ->
    indent buffer (level - 1);
    Buffer.add_string buffer "  "

let rec display buffer level value =
  match value with
  | VString string ->
    Buffer.add_string buffer "\"";
    Buffer.add_string buffer string;
    Buffer.add_string buffer "\""
  | VMaybe value -> (
    match value with
    | Some value ->
      Buffer.add_string buffer "some ";
      display buffer level value
    | None ->
      Buffer.add_string buffer "none")
  | VList values ->
    Buffer.add_string buffer "[";
    List.iter (fun value ->
      let level = level + 1 in
      indent buffer level;
      display buffer level value
    ) values;
    if List.length values > 0 then
      indent buffer level;
    Buffer.add_string buffer "]";
  | VNode node ->
    let name = get_name node in
    let attrs = get_attrs node in
    Buffer.add_string buffer name;
    if List.length attrs > 0 then (
      Buffer.add_string buffer " {";
      List.iter (fun (name, value) ->
        let level = level + 1 in
        indent buffer level;
        Buffer.add_string buffer name;
        Buffer.add_string buffer ": ";
        display buffer level value
      ) attrs;
      indent buffer level;
      Buffer.add_string buffer "}"
    )

let display value =
  let buffer = Buffer.create 16 in
  display buffer 0 value;
  Buffer.contents buffer

let display_program program =
  display (v_program program)
