open Model

let rec join (left: type') (right: type'): type' =
  let pos = type_pos left in
  match (left, right) with
  | (TypeTop    _,            _) -> TypeTop    { pos }
  | (_           , TypeTop    _) -> TypeTop    { pos }
  | (TypeBot    _, right       ) -> right
  | (left        , TypeBot    _) -> left
  | (TypeUnit   _, TypeUnit   _) -> TypeUnit   { pos }
  | (TypeBool   _, TypeBool   _) -> TypeBool   { pos }
  | (TypeInt    _, TypeInt    _) -> TypeInt    { pos }
  | (TypeChar   _, TypeChar   _) -> TypeChar   { pos }
  | (TypeString _, TypeString _) -> TypeString { pos }
  | (TypeVar left_var, TypeVar right_var) when left_var = right_var ->
    TypeVar left_var
  | (TypeUnion left_union, _) ->
    let union = join left_union.left left_union.right in
    join union right
  | (_, TypeUnion right_union) ->
    let union = join right_union.left right_union.right in
    join left union
  | (_, _) ->
    TypeUnion { pos; left; right }
