type ship = {
  name : string;
  length : int;
  coordinates : (string * string) list;
  hits : int;
}

let coord_add coord str str2 = (str, str2) :: coord
let create_ship name length a = { name; length; coordinates = a; hits = 0 }

(**let output match_emoji = function | true -> "❌" | false -> "⚪️" **)
