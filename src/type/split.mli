(**
  Split a type into two union components if this type is union-splittable.
  Union-splittable types include unions, and intersections whose at least one
  operand is union-splittable.
*)
val split_union : Abt.Type.type' -> (Abt.Type.type' * Abt.Type.type') option

(**
  Split a type into two intersection components if this type is
  intersection-splittable.
  Intersection-splittable types include intersections, and unions whose at
  least one operand is intersection-splittable. Other intersection-splittable
  types such as lambdas, records... are not split by this function.
*)
val split_inter : Abt.Type.type' -> (Abt.Type.type' * Abt.Type.type') option
