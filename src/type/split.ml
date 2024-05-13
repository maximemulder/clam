open Display
open Node

let rec split_union type' =
  let res = split_union_base type' in
  if !Global.show_split then (
    match res with
    | Some (left, right) ->
      Global.print ("split_union `" ^ display type' ^ "` = `" ^ display left ^ "` | `" ^ display right ^ "`")
    | _ -> ());
  res

and split_union_base type' =
  match type' with
  | Union union ->
    split_union_union union
  | Inter inter ->
    split_union_inter inter
  | _ ->
    None

and split_union_union union =
  Some (union.left, union.right)

and split_union_inter inter =
  match split_union inter.left with
  | Some (left, right) ->
    Some (
      Inter { inter with left = left  },
      Inter { inter with left = right }
    )
  | None ->
  match split_union inter.right with
  | Some (left, right) ->
    Some (
      Inter { inter with right = left  },
      Inter { inter with right = right }
    )
  | None ->
    None

let rec split_inter type' =
  let res = split_inter_base type' in
  if !Global.show_split then (
    match res with
    | Some (left, right) ->
      Global.print ("split_inter `" ^ display type' ^ "` = `" ^ display left ^ "` & `" ^ display right ^ "`")
    | _ -> ());
  res

and split_inter_base type' =
  match type' with
  | Union union ->
    split_inter_union union
  | Inter inter ->
    split_inter_inter inter
  | _ ->
    None

and split_inter_union union =
  match split_inter union.left with
  | Some (left, right) ->
    Some (
      Union { union with left = left  },
      Union { union with left = right }
    )
  | None ->
  match split_inter union.right with
  | Some (left, right) ->
    Some (
      Union { union with right = left  },
      Union { union with right = right }
    )
  | None ->
    None

and split_inter_inter inter =
  Some (inter.left, inter.right)
