exception Error of Syntax.t * Type.t * Type.t

val extenv : Type.t M.t ref

val infer : Syntax.t -> Syntax.t
