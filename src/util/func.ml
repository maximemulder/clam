let id x = x

let const x _ = x

let apply x f = f x

let flip f x y = f y x

let curry f x y = f (x, y)

let uncurry f (x, y) = f x y
