(** Global flags used to show algorithmic steps to the user. Global mutable
  variables are used here to avoid cluttering the algorithms with additional
  parameters unrelated to their inputs and outputs. *)

(** Show the type inference steps. *)
let show_infer = ref false

(** Show the subtype constrain steps. *)
let show_constrain = ref false

(** Show the cooccurrence removals. *)
let show_cooccur = ref false

(** Show the join operations. *)
let show_join = ref false

(** Show the meet operations. *)
let show_meet = ref false

(** Temporarily turn off a given flag. *)
let with_flag_off flag f =
  let old = !flag in
  flag := false;
  let res = f () in
  flag := old;
  res
