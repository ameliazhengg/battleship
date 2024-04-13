type t = int * int

let new_coord (x : int) (y : int) = (x, y)

let equals (c1 : t) (c2 : t) =
  if fst c1 = fst c2 && snd c1 = snd c2 then true else false

let get_coord (coord : t) = (fst coord, snd coord)
