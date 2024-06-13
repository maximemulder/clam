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

(** Dummy span used by primitive definitions *)
let span_primitive = {
  code = {
    name = "primitives.clam";
    text = "";
  };
  start = 0;
  end' = 0;
}

let merge_span span_1 span_2 =
  if span_1.code.name <> span_2.code.name then
    (* If the two spans come from different files, do not try to merge them and
      return one of the parameter spans. *)
    span_1
  else if span_1.start <= span_2.start then
    { code = span_1.code; start = span_1.start; end' = span_2.end' }
  else
    { code = span_1.code; start = span_2.start; end' = span_1.end' }

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
