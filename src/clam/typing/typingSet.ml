open Model

let rec collect_union type' =
  match type' with
  | TypeUnion { left; right; _ } ->
    let lefts = collect_union left in
    let rights = collect_union right in
    List.append lefts rights
  | TypeInter { left; right; _ } ->
    let lefts = collect_inter left in
    let rights = collect_inter right in
    Utils.product_lists (fun left right -> make_inter (List.append left right)) lefts rights
  | _ -> [type']

and collect_inter type' =
  match type' with
  | TypeInter { left; right; _ } ->
    let lefts = collect_inter left in
    let rights = collect_inter right in
    Utils.product_lists List.append lefts rights
  | TypeUnion { left; right; _ } ->
    let lefts = collect_union left in
    let rights = collect_union right in
    let types = List.append lefts rights in
    List.map (fun type' -> [type']) types
  | _ -> [[type']]

and make_union types =
  match types with
  | [type'] -> type'
  | left :: rights ->
    (* TypingJoin.join left (make_union rights) *)
    TypeUnion { pos = type_pos left; left; right = make_union rights }
  | _ ->
    invalid_arg "TypingSort.make_union"

and make_inter types =
  match types with
  | [type'] -> type'
  | left :: rights ->
    (* TypingMeet.meet left (make_inter rights) *)
    TypeInter { pos = type_pos left; left; right = make_inter rights }
  | _ ->
    invalid_arg "TypingSort.make_inter"

let normalize type' =
  make_union (collect_union type')
