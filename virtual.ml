(* translation into assembly with infinite number of virtual registers *)

open Asm

let data = ref []

let classify xts ini addf addi =
  List.fold_left
    (fun acc (x, t) ->
       match t with
       | Type.Unit ->
         acc

       | Type.Float ->
         addf acc x

       | _ ->
         addi acc x t)
    ini
    xts

(* seperate float args with other args *)
let separate xts =
  classify
    xts
    ([], [])
    (fun (i, a) x -> (i, a @ [x]))
    (fun (i, a) x _ -> (i @ [x], a))

let expand xts ini addf addi =
  classify
    xts
    ini
    (fun (offset, acc) x ->
       let offset = align offset in
       (offset + 8, addf x offset acc))
    (fun (offset, acc) x t ->
       (offset + 4, addi x t offset acc))

(* code generation *)
let rec g env = function
  | Closure.Unit ->
    Ans(Nop)

  | Closure.Int(i) ->
    Ans(Set(i))

  | Closure.Float(d) ->
    let l =
      try
        let (l, _) = List.find (fun (_, d') -> d = d') !data in
        l
      with Not_found ->
        let l = Id.Label(Id.genid "l") in
        data := (l, d) :: !data;
        l
    in
    let x = Id.genid "l" in
    Let((x, Type.Int), SetL(l), Ans(LdDF(x, C(0), 1)))

  | Closure.Neg(x) ->
    Ans(Neg(x))

  | Closure.Add(x, y) ->
    Ans(Add(x, V(y)))

  | Closure.Sub(x, y) ->
    Ans(Sub(x, V(y)))

  | Closure.FNeg(x) ->
    Ans(FNegD(x))

  | Closure.FAdd(x, y) ->
    Ans(FAddD(x, y))

  | Closure.FSub(x, y) ->
    Ans(FSubD(x, y))

  | Closure.FMul(x, y) ->
    Ans(FMulD(x, y))

  | Closure.FDiv(x, y) ->
    Ans(FDivD(x, y))

  | Closure.IfEq(x, y, e1, e2) ->
    (
      match M.find x env with
      | Type.Bool | Type.Int ->
        Ans(IfEq(x, V(y), g env e1, g env e2))

      | Type.Float ->
        Ans(IfFEq(x, y, g env e1, g env e2))

      | _ -> 
        failwith "equality supported only for bool, int, and float"
    )

  | Closure.IfLE(x, y, e1, e2) ->
    (
      match M.find x env with
      | Type.Bool | Type.Int ->
        Ans(IfLE(x, V(y), g env e1, g env e2))

      | Type.Float ->
        Ans(IfFLE(x, y, g env e1, g env e2))

      | _ ->
        failwith "inequality supported only for bool, int, and float"
    )

  | Closure.Let((x, t1), e1, e2) ->
    let e1' = g env e1 in
    let e2' = g (M.add x t1 env) e2 in
    concat e1' (x, t1) e2'

  | Closure.Var(x) ->
    (
      match M.find x env with
      | Type.Unit ->
        Ans(Nop)

      | Type.Float ->
        Ans(FMovD(x))

      | _ ->
        Ans(Mov(x))
    )

  | Closure.MakeCls(
      (x, t), { Closure.entry; Closure.actual_fv }, e2
    ) ->
    let e2' = g (M.add x t env) e2 in
    let offset, store_fv =
      expand
        (List.map (fun y -> (y, M.find y env)) actual_fv)
        (4, e2')
        (fun y offset store_fv -> seq(StDF(y, x, C(offset), 1), store_fv))
        (fun y _ offset store_fv -> seq(St(y, x, C(offset), 1), store_fv))
    in
    Let((x, t), Mov(reg_hp),
        Let((reg_hp, Type.Int), Add(reg_hp, C(align offset)),
            let z = Id.genid "entry" in
            Let((z, Type.Int), SetL(entry),
                seq(St(z, x, C(0), 1), store_fv))))

  | Closure.AppCls(x, ys) ->
    let (int, float) = separate (List.map (fun y -> (y, M.find y env)) ys) in
    Ans(CallCls(x, int, float))

  | Closure.AppDir(Id.Label(x), ys) ->
    let (int, float) = separate (List.map (fun y -> (y, M.find y env)) ys) in
    Ans(CallDir(Id.Label(x), int, float))

  | Closure.Tuple(xs) ->
    let y = Id.genid "t" in
    let (offset, store) =
      expand
        (List.map (fun x -> (x, M.find x env)) xs)
        (0, Ans(Mov(y)))
        (fun x offset store -> seq(StDF(x, y, C(offset), 1), store))
        (fun x _ offset store -> seq(St(x, y, C(offset), 1), store)) in
    Let((y, Type.Tuple(List.map (fun x -> M.find x env) xs)), Mov(reg_hp),
        Let((reg_hp, Type.Int), Add(reg_hp, C(align offset)),
            store))

  | Closure.LetTuple(xts, y, e2) ->
    let s = Closure.free_vars e2 in
    let (_offset, load) =
      expand
        xts
        (0, g (M.add_list xts env) e2)
        (fun x offset load ->
           if not (S.mem x s) then load else
             (* [XX] a little ad hoc optimization *)
             fletd(x, LdDF(y, C(offset), 1), load))
        (fun x t offset load ->
           if not (S.mem x s) then load else
             (* [XX] a little ad hoc optimization *)
             Let((x, t), Ld(y, C(offset), 1), load)) in
    load

  | Closure.Get(x, y) ->
    (
      match M.find x env with
      | Type.Array(Type.Unit) ->
        Ans(Nop)

      | Type.Array(Type.Float) ->
        Ans(LdDF(x, V(y), 8))

      | Type.Array(_) ->
        Ans(Ld(x, V(y), 4))

      | _ ->
        assert false
    )

  | Closure.Put(x, y, z) ->
    (
      match M.find x env with
      | Type.Array(Type.Unit) ->
        Ans(Nop)

      | Type.Array(Type.Float) ->
        Ans(StDF(z, x, V(y), 8))

      | Type.Array(_) ->
        Ans(St(z, x, V(y), 4))

      | _ ->
        assert false
    )

  | Closure.ExtArray(Id.Label(x)) ->
    Ans(SetL(Id.Label("min_caml_" ^ x)))

let genfundef {
    Closure.name = (Id.Label(x), t);
    Closure.args;
    Closure.formal_fv;
    Closure.body
  } =
  let (int, float) = separate args in
  let (_offset, load) =
    expand
      formal_fv
      (4, g (M.add x t (M.add_list args (M.add_list formal_fv M.empty))) body)
      (fun z offset load -> fletd(z, LdDF(x, C(offset), 1), load))
      (fun z t offset load -> Let((z, t), Ld(x, C(offset), 1), load))
  in
  match t with
  | Type.Fun(_, t2) ->
    { name = Id.Label(x); args = int; fargs = float; body = load; ret = t2 }

  | _ ->
    assert false

let gencode (Closure.Prog(fundefs, e)) =
  data := [];
  let fundefs = List.map genfundef fundefs in
  let e = g M.empty e in
  let prog = Prog(!data, fundefs, e) in
  Printf.eprintf "------------------------------\n" ;
  Printf.eprintf "==> Asm Prog: \n%s\n" (show_prog prog) ;
  prog
