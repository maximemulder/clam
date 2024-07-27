open Base

module Identity : sig
  include MONAD
    with type 'a t = 'a
end
