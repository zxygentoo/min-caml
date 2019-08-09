module K = Knormal


type closure =
  { entry : Id.label
  ; actual_fv : Id.t list
  }
(* [@@deriving show] *)


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
  | MakeCls of (Id.t * Type.t) * closure * t
  | AppCls of Id.t * Id.t list
  | AppDir of Id.label * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  | ExtArray of Id.label
  (* [@@deriving show] *)


type fundef =
  { name : Id.label * Type.t
  ; args : (Id.t * Type.t) list
  ; formal_fv : (Id.t * Type.t) list
  ; body : t
  }
(* [@@deriving show] *)


type prog = Prog of fundef list * t
(* [@@deriving show] *)


let toplevel : fundef list ref = ref []

let backup_toplevel () = !toplevel
let restore_toplevel v = toplevel := v
let add_toplevel_fundef f = toplevel := f :: !toplevel


let rec free_vars = function
  | Unit
  | Int _
  | Float _
  | ExtArray _ ->
    S.empty

  | Neg x
  | FNeg x ->
    S.singleton x

  | Add(x, y)
  | Sub(x, y)
  | FAdd(x, y)
  | FSub(x, y)
  | FMul(x, y)
  | FDiv(x, y)
  | Get(x, y) ->
    S.of_list [x; y]

  | IfEq(x, y, e1, e2, _)
  | IfLE(x, y, e1, e2, _) ->
    S.union (free_vars e1) (free_vars e2) |> S.add y |> S.add x

  | Let((x, _), e1, e2) ->
    S.union (free_vars e1) (S.remove x (free_vars e2))

  | Var x ->
    S.singleton x

  | MakeCls((x, _), { actual_fv ; _ }, e) ->
    S.remove x (S.union (S.of_list actual_fv) (free_vars e))

  | AppCls(x, ys) ->
    S.of_list (x :: ys)

  | AppDir(_, xs) | Tuple(xs) ->
    S.of_list xs

  | LetTuple(xts, y, e) ->
    S.add y (S.diff (free_vars e) (S.of_list (List.map fst xts)))

  | Put(x, y, z) ->
    S.of_list [x; y; z]


let args_fv args =
  S.of_list (List.map fst args)


let make_cls entry fvs =
  { entry = Id.Label entry ; actual_fv = fvs }


let make_fundef id t args formal_fv body =
  { name = Id.Label id, t ; args ; formal_fv ; body }


let rec g env known = function
  | K.Unit ->
    Unit

  | K.Int i ->
    Int i

  | K.Float d ->
    Float d

  | K.Neg x ->
    Neg x

  | K.Add(x, y) ->
    Add(x, y)

  | K.Sub(x, y) ->
    Sub(x, y)

  | K.FNeg x ->
    FNeg x

  | K.FAdd(x, y) ->
    FAdd(x, y)

  | K.FSub(x, y) ->
    FSub(x, y)

  | K.FMul(x, y) ->
    FMul(x, y)

  | K.FDiv(x, y) ->
    FDiv(x, y)

  | K.IfEq(x, y, e1, e2, t) ->
    IfEq(x, y, g env known e1, g env known e2, t)

  | K.IfLE(x, y, e1, e2, t) ->
    IfLE(x, y, g env known e1, g env known e2, t)

  | K.Let((x, t), e1, e2) ->
    Let((x, t), g env known e1, g (M.add x t env) known e2)

  | K.Var x ->
    Var x

  | K.LetRec({ name = x, t ; args ; body }, e) ->
    (* backup toplevel *)
    let backup = backup_toplevel () in
    let env' = M.add x t env in
    let fn_env' = M.add_list args env' in
    (* assuming function no fvs and convert body *)
    let known' = S.add x known in
    let body' = g fn_env' known' body in
    let no_fvs = S.is_empty (S.diff (free_vars body') (args_fv args)) in
    (* restore toplevel if actually has fvs *)
    if not no_fvs then restore_toplevel backup ;
    (* real known and body *)
    let known, body = if no_fvs
      then known', body'
      else known, g fn_env' known body in
    let fvs = S.elements (S.diff (free_vars body) (S.add x (args_fv args))) in
    let fvts = List.map (fun fv -> (fv, M.find fv env')) fvs in
    (* add toplevel fundef *)
    add_toplevel_fundef (make_fundef x t args fvts body) ;
    (* make closure if needed *)
    let e' = g env' known e in
    let is_cls = S.mem x (free_vars e') in
    if is_cls then MakeCls((x, t), make_cls x fvs, e') else e'

  | K.App(x, ys) when S.mem x known ->
    AppDir(Id.Label(x), ys)

  | K.App(f, xs) ->
    AppCls(f, xs)

  | K.Tuple xs ->
    Tuple xs

  | K.LetTuple(xts, y, e) ->
    LetTuple(xts, y, g (M.add_list xts env) known e)

  | K.Get(x, y) ->
    Get(x, y)

  | K.Put(x, y, z) ->
    Put(x, y, z)

  | K.ExtArray x ->
    ExtArray(Id.Label x)

  | K.ExtFunApp(x, ys) ->
    AppDir(Id.Label("min_caml_" ^ x), ys)


let flattern e =
  toplevel := [];
  let e' = g M.empty S.empty e in
  let prog = Prog(List.rev !toplevel, e') in
  (* Printf.eprintf "==> Clourse Prog: \n%s\n" (show_prog prog) ; *)
  prog
