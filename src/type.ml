type t =
  | Unit
  | Bool
  | Int
  | Float
  | Fun of t list * t
  | Tuple of t list
  | Array of t
  | Var of t option ref
[@@deriving show]


let gentyp () =
  Var(ref None)
