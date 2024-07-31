module type BASE = sig
  type 'a t

  val return: 'a -> 'a t

  val bind: 'a t -> ('a -> 'b t) -> 'b t
end

module type MONAD = sig
  include BASE

  val (let*) : 'a t -> ('a -> 'b t) -> 'b t

  module MUtil : sig
    val option_map : ('a -> 'b t) -> 'a option -> 'b option t

    val list_map : ('a -> 'b t) -> 'a list -> 'b list t
  end
end

module type TRANS = sig
  include MONAD

  type 'a m

  val lift : 'a m -> 'a t
end

module Make (B: BASE) = struct
  include B

  let (let*) = B.bind

  module MUtil = struct
    let option_map f x =
      match x with
      | Some x ->
        let* x = f x in
        return (Some x)
      | None ->
        return None

    let rec list_map f xs =
      match xs with
      | [] ->
        return []
      | x :: xs ->
        let* x = f x in
        let* xs = list_map f xs in
        return (x :: xs)
  end
end
