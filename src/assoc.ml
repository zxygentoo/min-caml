open Knormal


let rec f = function
  | IfEq(x, y, e1, e2, t) ->
    IfEq(x, y, f e1, f e2, t)

  | IfLE(x, y, e1, e2, t) ->
    IfLE(x, y, f e1, f e2, t)

  | Let(xt, e1, e2) ->
    let rec insert = function
      | Let(yt, e3, e4) -> Let(yt, e3, insert e4)
      | LetRec(fundefs, e) -> LetRec(fundefs, insert e)
      | LetTuple(yts, z, e) -> LetTuple(yts, z, insert e)
      | e -> Let(xt, e, f e2)
    in
    insert (f e1)

  | LetRec({ name = xt ; args = yts ; body = e1 }, e2) ->
    LetRec({ name = xt ; args = yts ; body = f e1 }, f e2)

  | LetTuple(xts, y, e) ->
    LetTuple(xts, y, f e)

  | e ->
    e
