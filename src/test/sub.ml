open Vars

let assert_sub type' constraint' =
  assert (Clam.TypingSub.is_subtype type' constraint' Clam.TypingContext.context_empty)

;; assert_sub a a
;; assert_sub a (union a b)
;; assert_sub (union a b) (union a b)
;; assert_sub (inter a b) (union b a)
;; assert_sub (union (inter a b) (inter a c)) (inter a (union b c))
