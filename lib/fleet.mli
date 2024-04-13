type 'a t

val new_fleet : unit -> 'a t
val contains : 'a t -> Coord.t -> bool
val add_ship : 'a t -> Coord.t -> 'a t * bool
val to_string : 'a t -> string
