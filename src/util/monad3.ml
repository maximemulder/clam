open Func

module type MONAD = sig
  type 'a t

  val return: 'a -> 'a t

  val bind: 'a t -> ('a -> 'b t) -> 'b t
end

module type TRANS = sig
  type 'a m
  type 'a t

  val lift : 'a m -> 'a t
end

module Syntax (M: MONAD) = struct
  let (let*) = M.bind
end

module Identity : sig
  include MONAD with type 'a t = 'a
end = struct
  type 'a t = 'a

  let return a =
    a

  let bind m f =
    f m
end

module type RESULT = sig
  type error
end

module ResultT (M: MONAD) (R: RESULT) : sig
  include MONAD
    with type 'a t = ('a, R.error) result M.t

  include TRANS
    with type 'a m := 'a M.t
     and type 'a t := 'a t

  val fail : R.error -> 'a t
end = struct
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

  let fail e =
    M.return (Error e)

  let lift m =
    M.bind m return
end

module Result = ResultT(Identity)

module type STATE = sig
  type state
end

module StateT (M: MONAD) (S: STATE) : sig
  include MONAD with type 'a t = S.state -> ('a * S.state) M.t

  include TRANS
    with type 'a m := 'a M.t
     and type 'a t := 'a t

  val get : S.state t

  val put : S.state -> unit t

  val modify : (S.state -> S.state) -> unit t
end = struct
  type 'a t = S.state -> ('a * S.state) M.t

  let return a s =
    M.return (a, s)

  let bind m f s =
    M.bind (m s) (uncurry f)

  let get s =
    M.return (s, s)

  let put s _ =
    M.return ((), s)

  let modify f s =
    M.return ((), f s)

  let lift m s =
    M.bind m (fun a -> return a s)
end

module State = StateT(Identity)

module S = struct
  type state = int
end

module E = struct
  type error = string
end

open State(S)
open ResultT(State(S))(E)
open Syntax(ResultT(State(S))(E))

let a () =
  let* i = return 0 in
  let* e = lift get in
  let* d = fail "" in
  return i

let c, b = a () 2

open Result(E)
open StateT(Result(E))(S)
open Syntax(StateT(Result(E))(S))

let a () =
  let* i = return 0 in
  let* i = lift (fail "A") in
  let* i = get in
  return i

let a () =
  let a = return 0 2 in
  match a with
  | Ok a -> ()
  | Error a -> ()

(* module S : STATE with type state = int = struct
  type state = int
end

module R = struct
end *)
