module C = Closure

type t =
  | Asm of exp
  | Let of (Id.t * Type.t) * exp * t

and exp =
  | Comment of string
  | Nop
  | Int of int
  (* | Float of float *)
  (* | Neg of Id.t *)
  | Add of Id.t * Id.t
  (* | Sub of Id.t * Id.t *)
  (* | FSub of Id.t * Id.t *)
  (* | FMul of Id.t * Id.t *)
  (* | FDiv of Id.t * Id.t *)
  | MakeCls of (Id.t * Type.t) * closure * t
  | Call of Id.t * Id.t list * Id.t list
  | CallInDir of Id.label * Id.t list * Id.t list

and closure = {
  entry : Id.label;
  actual_fv : Id.t list;
}

type fundef = {
  name : Id.label * Type.t;
  args : (Id.t * Type.t) list;
  formal_fv : (Id.t * Type.t) list;
  body : t;
  (* ret : Type.t *)
}

type prog = Prog of fundef list * t


let g _env = function
  | C.Unit ->
    Asm(Nop)

  | C.Int(i) ->
    Asm(Int(i))

  | C.Float(_a) ->
    Asm(Nop)

  | C.Neg(_) ->
    Asm(Nop)

  | C.Add(x, y) ->
    Asm(Add(x, y))

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

  | C.MakeCls((_x, _t), { C.entry = _; C.actual_fv = _ }, _e) ->
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


let genfundef {
  C.name = (Id.Label(x), t);
  C.args = args;
  C.formal_fv = formal_fv;
  C.body = body;
  C.is_cls = _
} = {
  name = (Id.Label(x), t);
  args = args;
  formal_fv = formal_fv;
  body = g
    (M.empty |> M.add_list formal_fv |> M.add_list args |> M.add x t) body;
  (* ret = Type.Int *)
}


let codegen (C.Prog(fundefs, e)) =
  Prog(
    List.map genfundef fundefs,
    g M.empty e
  )
