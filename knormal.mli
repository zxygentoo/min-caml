type t =
  | Unit
  | Int of int
  | Float of float
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | FNeg of Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | IfEq of Id.t * Id.t * t * t * Type.t
  | IfLE of Id.t * Id.t * t * t * Type.t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of Id.t * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  (* externals *)
  | ExtArray of Id.t
  | ExtFunApp of Id.t * Id.t list

and fundef =
  { name : Id.t * Type.t
  ; args : (Id.t * Type.t) list
  ; body : t
  }



val convert_let : t * Type.t -> (Id.t -> t * Type.t) -> t * Type.t

val free_vars : t -> S.t

val g : Type.t M.t -> Syntax.t -> t * Type.t

val normalize : Syntax.t -> t
