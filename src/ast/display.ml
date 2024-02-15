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
  | VString string -> display_string buffer string
  | VMaybe  maybe  -> display_maybe  buffer level maybe
  | VList   list   -> display_list   buffer level list
  | VNode   node   -> display_node   buffer level node

and display_string buffer string =
  Buffer.add_string buffer "\"";
  Buffer.add_string buffer string;
  Buffer.add_string buffer "\""

and display_maybe buffer level maybe =
  match maybe with
  | Some value ->
    Buffer.add_string buffer "some ";
    display buffer level value
  | None ->
    Buffer.add_string buffer "none"

and display_list buffer level list =
  Buffer.add_string buffer "[";
  List.iter (fun value ->
    let level = level + 1 in
    indent buffer level;
    display buffer level value
  ) list;
  if List.length list > 0 then
    indent buffer level;
  Buffer.add_string buffer "]";

and display_node buffer level node =
  let name = get_name node in
  let span = get_span node in
  let attrs = get_attrs node in
  Buffer.add_string buffer name;
  Buffer.add_string buffer " ";
  Buffer.add_string buffer (span |> Code.get_span_start_y |> string_of_int);
  Buffer.add_string buffer ":";
  Buffer.add_string buffer (span |> Code.get_span_start_x |> string_of_int);
  Buffer.add_string buffer "~";
  Buffer.add_string buffer (span |> Code.get_span_end_y |> string_of_int);
  Buffer.add_string buffer ":";
  Buffer.add_string buffer (span |> Code.get_span_end_x |> string_of_int);
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
