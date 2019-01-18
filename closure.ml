module K = Knormal

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
  | IfEq of Id.t * Id.t * t * t
  | IfLE of Id.t * Id.t * t * t
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

and closure = {
  entry : Id.label;
  actual_fv : Id.t list
} [@@deriving show]

type fundef = {
  name : Id.label * Type.t;
  is_cls : bool;
  args : (Id.t * Type.t) list;
  formal_fv : (Id.t * Type.t) list;
  body : t
}
[@@deriving show]

type prog = Prog of fundef list * t [@@deriving show]


let toplevel : fundef list ref = ref []

let backup_toplevel _ =
  !toplevel

let restore_toplevel v =
  toplevel := v

let add_toplevel_fundef f =
  toplevel := f :: !toplevel


let rec free_vars = function
  | Unit | Int(_) | Float(_) | ExtArray(_) ->
    S.empty

  | Neg(x) | FNeg(x) ->
    S.singleton x

  | Add(x, y) | Sub(x, y) | FAdd(x, y) | FSub(x, y)
  | FMul(x, y) | FDiv(x, y) | Get(x, y) ->
    S.of_list [x; y]

  | IfEq(x, y, e1, e2)| IfLE(x, y, e1, e2) ->
    S.union (free_vars e1) (free_vars e2) |> S.add y |> S.add x

  | Let((x, _), e1, e2) ->
    S.union (free_vars e1) (S.remove x (free_vars e2))

  | Var(x) ->
    S.singleton x

  | MakeCls((x, _), { entry = _; actual_fv }, e) ->
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

(* closure conversion *)
let rec g env known = function
  | K.Unit ->
    Unit

  | K.Int(i) ->
    Int(i)

  | K.Float(d) ->
    Float(d)

  | K.Neg(x) ->
    Neg(x)

  | K.Add(x, y) ->
    Add(x, y)

  | K.Sub(x, y) ->
    Sub(x, y)

  | K.FNeg(x) ->
    FNeg(x)

  | K.FAdd(x, y) ->
    FAdd(x, y)

  | K.FSub(x, y) ->
    FSub(x, y)

  | K.FMul(x, y) ->
    FMul(x, y)

  | K.FDiv(x, y) ->
    FDiv(x, y)

  | K.IfEq(x, y, e1, e2) ->
    IfEq(x, y, g env known e1, g env known e2)

  | K.IfLE(x, y, e1, e2) ->
    IfLE(x, y, g env known e1, g env known e2)

  | K.Let((x, t), e1, e2) ->
    Let((x, t), g env known e1, g (M.add x t env) known e2)

  | K.Var(x) ->
    Var(x)

  | K.LetRec({ name = (x, t); args; body }, exp) ->
    (* backup toplevel so we may rewind it later *)
    let backup = backup_toplevel ()
    and env' = M.add x t env in

    let cls_convert_body _ =
      (* assuming function has no free variables *)
      let known' = S.add x known in
      let body' = g (M.add_list args env') known' body in
      let fvs = S.diff (free_vars body') (args_fv args) in
      if S.is_empty fvs then
        begin
          Format.eprintf
            "--> function `%s` has no free variables, \
             can be directly applied@."x ;
          known', body'
        end
      else
        begin          
          Format.eprintf
            "--> function `%s` has free variable(s) %s, \
             can not be directly applied@." x (Id.pp_list (S.elements fvs)) ;
          (* restore toplevel from backup *)
          restore_toplevel backup ;
          known, g (M.add_list args env') known body
        end
    in
    let known', body' = cls_convert_body () in

    let actual_fv =
      S.elements (S.diff (free_vars body') (S.add x (args_fv args))) in
    let formal_fv =
      List.map (fun z -> (z, M.find z env')) actual_fv in
    let exp' = g env' known' exp in
    let is_cls = S.mem x (free_vars exp') in

    (* add toplevel fundef *)
    add_toplevel_fundef
      {
        name = (Id.Label(x), t);
        is_cls = is_cls;
        args;
        formal_fv;
        body = body'
      } ;

    (* make or eliminate closure *)
    let cls_convert_exp e =
      if is_cls then
        begin
          Format.eprintf "--> making closure `%s`@." x ;
          Format.eprintf
            "==> Cls: %s\n"
            (show_closure { entry = Id.Label(x); actual_fv }) ;
          MakeCls((x, t), { entry = Id.Label(x); actual_fv }, e)
        end
      else
        begin
          Format.eprintf "--> eliminating closure `%s`@." x ;
          exp'
        end
    in
    cls_convert_exp exp'

  | K.App(x, ys) when S.mem x known ->
    Format.eprintf "--> directly applying %s@." x ;
    AppDir(Id.Label(x), ys)

  | K.App(f, xs) ->
    AppCls(f, xs)

  | K.Tuple(xs) ->
    Tuple(xs)

  | K.LetTuple(xts, y, e) ->
    LetTuple(xts, y, g (M.add_list xts env) known e)

  | K.Get(x, y) ->
    Get(x, y)

  | K.Put(x, y, z) ->
    Put(x, y, z)

  | K.ExtArray(x) ->
    ExtArray(Id.Label(x))

  | K.ExtFunApp(x, ys) ->
    AppDir(Id.Label("min_caml_" ^ x), ys)

let flattern e =
  toplevel := [];
  let e' = g M.empty S.empty e in
  let prog = Prog(List.rev !toplevel, e')
  in
  (* Printf.eprintf "==> Prog: \n%s\n" (show_prog prog) ; *)
  ignore (show_prog prog) ;
  Printf.eprintf "==> Prog:\n" ;
  (List.iteri 
     (fun n fs -> Printf.eprintf "==> Fundef %d: \n%s\n" n (show_fundef fs))
     (let Prog(fs, _) = prog in fs)
  ) ;
  Printf.eprintf "==> Expression: \n%s\n" (show e') ;
  prog
