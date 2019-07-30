open Knormal


let threshold = ref 0


let rec size = function
  | IfEq(_, _, e1, e2, _)
  | IfLE(_, _, e1, e2, _)
  | Let(_, e1, e2)
  | LetRec({ body = e1; name = _; args = _ }, e2) ->
    1 + size e1 + size e2

  | LetTuple(_, _, e) ->
    1 + size e

  | _ ->
    1


let rec g env = function
  | IfEq(x, y, e1, e2, t) ->
    IfEq(x, y, g env e1, g env e2, t)

  | IfLE(x, y, e1, e2, t) ->
    IfLE(x, y, g env e1, g env e2, t)

  | Let(xt, e1, e2) ->
    Let(xt, g env e1, g env e2)

  | LetRec({ name = (x, t) ; args = yts ; body = e1 }, e2) ->
    let env = if size e1 > !threshold then env else M.add x (yts, e1) env in
    LetRec({ name = (x, t) ; args = yts ; body = g env e1}, g env e2)

  | App(x, ys) when M.mem x env ->
    let (zs, e) = M.find x env in
    (* inlining *)
    let env' = List.fold_left2
        (fun env' (z, _t) y -> M.add z y env') M.empty zs ys in
    Alpha.g env' e

  | LetTuple(xts, y, e) ->
    LetTuple(xts, y, g env e)

  | e ->
    e


let f e =
  g M.empty e
