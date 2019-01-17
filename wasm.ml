module C = Closure

type t =
  | Asm of exp
  | Let of (Id.t * Type.t) * exp * t

and exp =
  | Comment of string
  | Nop
  | Int of int
  | Float of float
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | Select of Id.t * Id.t * Id.t * t * t * t
  | CallCls of Id.t * Id.t list * Id.t list
  | CallDir of Id.label * Id.t list * Id.t list

type fundef = {
  name : Id.label * Type.t;
  args : (Id.t * Type.t) list;
  formal_fv : (Id.t * Type.t) list;
  body : t;
  ret : Type.t
}

type prog = Prog of fundef list * t


let f _env = function
  | C.Unit ->
    Asm(Nop)

  | C.Int(i) ->
    Asm(Int(i))

  | C.Float(a) ->
    Asm(Float(a))

  | C.Neg(_) ->
    Asm(Nop)

  | C.Add(_, _) ->
    Asm(Nop)

  | C.Sub(_, _) ->
    Asm(Nop)

  | C.FNeg(_) ->
    Asm(Nop)

  | C.FAdd(_, _) ->
    Asm(Nop)

  | C.FSub(_, _) ->
    Asm(Nop)

  | C.FMul(_, _) ->
    Asm(Nop)

  | C.FDiv(_, _) ->
    Asm(Nop)

  | C.IfEq(_, _, _, _) ->
    Asm(Nop)

  | C.IfLE(_, _, _, _) ->
    Asm(Nop)

  | C.Let((_, _), _, _) ->
    Asm(Nop)

  | C.Var(_) ->
    Asm(Nop)

  | C.MakeCls((_, _), { entry = _; actual_fv = _ }, _) ->
    Asm(Nop)

  | C.AppCls(_, _) ->
    Asm(Nop)

  | C.AppDir(_, _) ->
    Asm(Nop)

  | C.Tuple(_) ->
    Asm(Nop)

  | C.LetTuple(_, _, _) ->
    Asm(Nop)

  | C.Get(_, _) ->
    Asm(Nop)

  | C.Put(_, _, _) ->
    Asm(Nop)

  | C.ExtArray(_) ->
    Asm(Nop)



let codegen ast =
  ast
