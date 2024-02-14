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

(** Gets the line number of the start of the span, starting at 1 *)
let get_span_line span =
  fold_until_pos span.code.text span.start (fun char count ->
    match char with
    | '\n'-> count + 1
    | _ -> count
  ) 1

(** Gets the column number of the start of the span, starting at 1 *)
let get_span_column span =
  fold_until_pos span.code.text span.start (fun char count ->
    match char with
    | '\n'-> 1
    | _ -> count + 1
  ) 1
