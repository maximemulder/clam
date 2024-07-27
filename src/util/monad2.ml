module type MONAD = sig
  type 'a t
  val return: 'a -> 'a t
  val bind: 'a t -> ('a -> 'b t) -> 'b t
end

module Syntax (M: MONAD) = struct
  let (let*) = M.bind
end

module Util (M: MONAD) = struct
  open M
  open Syntax(M)

  (* Monad utilitary functions here *)
end

module Monad (M: MONAD) = struct
  include M
  include Syntax(M)
  module Monad = Util(M)
end

module Identity = struct
    type 'a t = 'a

    let return a =
      a

    let bind m f =
      f m
end

module type RESULT = sig
  type error
end

(* sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end *)

(* module ResultT (M: MONAD) (R: RESULT) = Monad(struct
  type 'a t = ('a M.t, R.error) result

  let return (a: 'a) : ('a M.t, 'b) result =
    Ok (M.return a)

  let bind (m: ('a M.t, 'b) result) (f: 'a -> ('c M.t, 'b) result) : ('c M.t, 'b) result =
    match m with
    | Ok v ->
      let e = f v in
      let d = M.bind v (fun x ->
        let e = f x in
        failwith "A"
        ) in
      let a = f (return v) in
      let b = return ( M.bind v f) in
      b
    | Error e ->
      Error e

  (* let bind m f =
    match m with
    | Ok v ->
      failwith "A"
    | Error e ->
      Error e
    (*
    match m with
    | Ok  v -> f v
    | Error e -> Error e
    ) *) *)

end) *)

module ResultT (M: MONAD) (R: RESULT) = struct
  module Base = struct
    type 'a t = ('a, R.error) result M.t

    let return a =
      M.return (Ok a)

    let bind m f =
      M.bind m (fun m ->
        match m with
        | Ok v ->
          f v
        | Error e ->
          M.return (Error e)
      )
  end

  include Monad(Base)
  include Base

  let fail e =
    M.return (Error e)

end

module Result = ResultT(Identity)

module type READER = sig
  type reader
end

module ReaderT (R: READER) = Monad(struct
  type 'a t = R.reader -> 'a

  let return a _ =
    a

  let bind m f r =
    f (m r) r

  let get s =
    s

end)

module type STATE = sig
  type state
end

module State (S: STATE) = struct
  module Base = struct
    type 'a t = S.state -> ('a * S.state)

    let return a s = (a, s)

    let bind m f s =
      let (a, s1) = m s in
      f a s1
  end

  include Monad(Base)

  let get s =
    s, s

  let put s _ =
    (), s

  let modify f s =
    (), f s

  let run m s =
    m s

end

module Stato : STATE with type state = int = struct
  type state = int
end

module Error : RESULT with type error = string = struct
  type error = string
end

module I = State(Stato)

open I

let j = return true 2

open ResultT(State(Stato))(Error)

let i = return true

(* let b () =
  let i = (return true) 2 in
  return ()

let c = b () *)
