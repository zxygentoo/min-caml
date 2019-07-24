module Stx = Syntax

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
  | ExtArray of Id.t
  | ExtFunApp of Id.t * Id.t list

and fundef =
  { name : Id.t * Type.t
  ; args : (Id.t * Type.t) list
  ; body : t
  }


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

  | Let((x, _t), e1, e2) ->
    S.union (free_vars e1) (S.remove x (free_vars e2))

  | Var x ->
    S.singleton x

  | LetRec({ name = (x, _t); args; body }, exp) ->
    let body_fvs = S.diff (free_vars body) (S.of_list (List.map fst args)) in
    S.diff (S.union body_fvs (free_vars exp)) (S.singleton x)

  | App(x, ys) ->
    S.of_list (x :: ys)

  | Tuple xs
  | ExtFunApp(_, xs) ->
    S.of_list xs

  | Put(x, y, z) ->
    S.of_list [x; y; z]

  | LetTuple(xs, y, e) ->
    S.add y (S.diff (free_vars e) (S.of_list (List.map fst xs)))


let convert_let (exp, ty) k =
  match exp with
  | Var(x) -> k x
  | _ ->
    let x = Id.gentmp ty in
    let exp2, ty2 = k x in
    Let((x, ty), exp, exp2), ty2


let rec g env = function
  | Stx.Unit ->
    Unit, Type.Unit

  | Stx.Bool b ->
    Int(if b then 1 else 0), Type.Int

  | Stx.Int i ->
    Int i, Type.Int

  | Stx.Float d ->
    Float d, Type.Float

  | Stx.Not e ->
    g env (Stx.If(e, Stx.Bool(false), Stx.Bool(true)))

  | Stx.Neg e ->
    convert_let (g env e) (fun x -> Neg(x), Type.Int)

  | Stx.Add(e1, e2) ->
    convert_let
      (g env e1)
      (fun x -> convert_let (g env e2) (fun y -> Add(x, y), Type.Int))

  | Stx.Sub(e1, e2) ->
    convert_let
      (g env e1)
      (fun x -> convert_let (g env e2) (fun y -> Sub(x, y), Type.Int))

  | Stx.FNeg e ->
    convert_let
      (g env e) (fun x -> FNeg(x), Type.Float)

  | Stx.FAdd(e1, e2) ->
    convert_let
      (g env e1)
      (fun x -> convert_let (g env e2) (fun y -> FAdd(x, y), Type.Float))

  | Stx.FSub(e1, e2) ->
    convert_let
      (g env e1)
      (fun x -> convert_let (g env e2) (fun y -> FSub(x, y), Type.Float))

  | Stx.FMul(e1, e2) ->
    convert_let
      (g env e1)
      (fun x -> convert_let (g env e2) (fun y -> FMul(x, y), Type.Float))

  | Stx.FDiv(e1, e2) ->
    convert_let
      (g env e1)
      (fun x -> convert_let (g env e2) (fun y -> FDiv(x, y), Type.Float))

  | Stx.Eq _
  | Stx.LE _ as cmp ->
    g env (Stx.If(cmp, Stx.Bool(true), Stx.Bool(false)))

  | Stx.If(Stx.Not(e1), e2, e3) ->
    g env (Stx.If(e1, e3, e2))

  | Stx.If(Stx.Eq(e1, e2), e3, e4) ->
    convert_let
      (g env e1)
      (fun x -> convert_let (g env e2)
          (fun y ->
             let e3', t3 = g env e3 in
             let e4', _ = g env e4 in
             IfEq(x, y, e3', e4', t3), t3))

  | Stx.If(Stx.LE(e1, e2), e3, e4) ->
    convert_let
      (g env e1)
      (fun x -> convert_let (g env e2)
          (fun y ->
             let e3', t3 = g env e3 in
             let e4', _ = g env e4 in
             IfLE(x, y, e3', e4', t3), t3))

  | Stx.If(e1, e2, e3) ->
    g env (Stx.If(Stx.Eq(e1, Stx.Bool(false)), e3, e2))

  | Stx.Let((x, t), e1, e2) ->
    let e1', _ = g env e1 in
    let e2', t2 = g (M.add x t env) e2 in
    Let((x, t), e1', e2'), t2

  | Stx.Var x when M.mem x env ->
    Var(x), M.find x env

  | Stx.Var x ->
    begin match M.find x !Typing.extenv with
      | Type.Array(_) as t ->
        ExtArray x, t

      | _ -> 
        failwith (Printf.sprintf "external `%s` doesn't have array type@." x)
    end

  | Stx.LetRec({ name = (x, t) ; args ; body }, e2) ->
    let env' = M.add x t env in
    let e2', t2 = g env' e2 in
    let e1', _ = g (M.add_list args env') body in
    LetRec({ name = (x, t) ; args ; body = e1' }, e2'), t2

  | Stx.App(Stx.Var(f), e2s) when not (M.mem f env) ->
    begin match M.find f !Typing.extenv with
      | Type.Fun(_, t) ->
        (* "xs" are identifiers for the arguments *)
        let rec bind xs = function
          | [] ->
            ExtFunApp(f, xs), t

          | e :: es ->
            convert_let (g env e)
              (fun x -> bind (xs @ [x]) es)
        in
        (* left-to-right evaluation *)
        bind [] e2s

      | _ ->
        failwith (Printf.sprintf "can't find external `%s`@." f)
    end

  | Stx.App(e1, e2s) ->
    begin match g env e1 with
      | _, Type.Fun(_, t) as g_e1 ->
        convert_let g_e1
          (fun f ->
             (* "xs" are identifiers for the arguments *)
             let rec bind xs = function
               | [] ->
                 App(f, xs), t

               | e2 :: e2s ->
                 convert_let (g env e2)
                   (fun x -> bind (xs @ [x]) e2s)
             in
             (* left-to-right evaluation *)
             bind [] e2s)

      | _ ->
        failwith (
          Printf.sprintf "frist expression of App doesn't have Fun type@."
        )
    end

  | Stx.Tuple es ->
    let rec bind xs ts = function
      (* "xs" and "ts" are identifiers and types for the elements *)
      | [] ->
        Tuple(xs), Type.Tuple(ts)

      | e :: es ->
        let _, t as g_e = g env e in
        convert_let g_e
          (fun x -> bind (xs @ [x]) (ts @ [t]) es)
    in
    bind [] [] es

  | Stx.LetTuple(xts, e1, e2) ->
    convert_let (g env e1)
      (fun y ->
         let e2', t2 = g (M.add_list xts env) e2 in
         LetTuple(xts, y, e2'), t2)

  | Stx.Array(e1, e2) ->
    convert_let (g env e1)
      (fun x ->
         let _, t2 as g_e2 = g env e2 in
         convert_let g_e2
           (fun y ->
              let l = match t2 with
                | Type.Float -> "make_float_array"
                | _ -> "make_array"
              in
              ExtFunApp(l, [x; y]), Type.Array(t2)))

  | Stx.Get(e1, e2) ->
    begin match g env e1 with
      | _, Type.Array(t) as g_e1 ->
        convert_let g_e1
          (fun x -> convert_let (g env e2)
              (fun y -> Get(x, y), t))

      | _ ->
        assert false
    end

  | Stx.Put(e1, e2, e3) ->
    convert_let (g env e1)
      (fun x -> convert_let (g env e2)
          (fun y -> convert_let (g env e3)
              (fun z -> Put(x, y, z), Type.Unit)))


let normalize e =
  fst (g M.empty e)
