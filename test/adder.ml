let rec make_adder x y =
  let u = y - 2 in
  let rec adder z = x + u + z in
  adder in
(make_adder 1 3) 5
