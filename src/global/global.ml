(** Global flags used to show algorithmic steps to the user. Global mutable
  variables are used here to avoid cluttering the algorithms with additional
  parameters unrelated to their inputs and outputs. *)

(** Flag to show the type inference steps. *)
let show_infer = ref false

(** Flag to show the subtyping steps *)
let show_isa = ref false
