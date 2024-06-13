(**
  [check_rec_path bind type']

  Check that the type [type'] has a termination path, considering it is an
  alias for the binding [bind] and may be recursive.
*)
val check_rec_path : Abt.Type.bind_type -> Abt.Type.type' -> bool
