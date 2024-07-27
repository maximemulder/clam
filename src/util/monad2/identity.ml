open Base

module Identity = Make(struct
  type 'a t = 'a

  let return a =
    a

  let bind m f =
    f m
end)
