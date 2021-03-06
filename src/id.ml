type t = string
[@@deriving show]

type label = Label of string
[@@deriving show]


let compare =
  Pervasives.compare


let rec pp_list_sep sep = function
  | [] -> ""
  | [x] -> x
  | x :: xs -> x ^ sep ^ pp_list_sep sep xs


let pp_list =
  pp_list_sep " "


let counter = ref 0


let genid s =
  incr counter ;
  Printf.sprintf "%s.%d" s !counter


let id_of_typ = function
  | Type.Unit -> "u"
  | Type.Bool -> "b"
  | Type.Int -> "i"
  | Type.Float -> "d"
  | Type.Fun _ -> "f"
  | Type.Tuple _ -> "t"
  | Type.Array _ -> "a" 
  | Type.Var _ -> assert false


let gentmp typ =
  incr counter ;
  Printf.sprintf "T%s.%d" (id_of_typ typ) !counter
