(** A code corresponds to a textual code resource, such as a file or a console *)
type code = {
  name: string;
  text: string;
}

(** A span corresponds to the location of a syntax node in a code. *)
type span = {
  code: code;
  start: int;
  end': int;
}

let rec fold_until_pos text pos n f acc =
  if n = pos then
    acc
  else
    let acc = f (String.get text n) acc in
    fold_until_pos text pos (n + 1) f acc

let fold_until_pos text pos f acc =
  fold_until_pos text pos 0 f acc

let rec fold_from_pos_until text pos f acc =
  let stop, acc = f (String.get text pos) acc in
  if stop then
    acc
  else
    fold_from_pos_until text (pos + 1) f acc

(** Gets the column number of an index in a code, starting at 1 *)
let get_x code index =
  fold_until_pos code.text index (fun char count ->
    match char with
    | '\n' -> 1
    | _ -> count + 1
  ) 1

(** Gets the line number of an index in a code, starting at 1 *)
let get_y code index =
  fold_until_pos code.text index (fun char count ->
    match char with
    | '\n' -> count + 1
    | _ -> count
  ) 1

(** Gets the column number of the start of a span, starting at 1 *)
let get_span_start_x span =
  get_x span.code span.start

(** Gets the line number of the start of a span, starting at 1 *)
let get_span_start_y span =
  get_y span.code span.start

(** Gets the column number of the end of a span, starting at 1 *)
let get_span_end_x span =
  get_x span.code span.end'

(** Gets the line number of the end of a span, starting at 1 *)
let get_span_end_y span =
  get_y span.code span.end'

(** Gets the index of the line start of the start of a span *)
let get_span_line_start span =
  fold_until_pos span.code.text span.start (fun char (line_count, full_count) ->
    let line_count = line_count + 1 in
    match char with
    | '\n' -> 0, full_count + line_count
    | _ -> line_count, full_count
  ) (0, 0) |> snd

(** Gets the index of the line end of the start of a span *)
let get_span_line_end span =
  fold_from_pos_until span.code.text span.start (fun char count ->
    match char with
    | '\n' -> true, count
    | _ -> false, count + 1
  ) span.start

(** Gets the line of a string start *)
let get_span_line span =
  let start = get_span_line_start span in
  let end'  = get_span_line_end   span in
  String.sub span.code.text start (end' - start)
