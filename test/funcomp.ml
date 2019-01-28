let rec compose f g =
  let rec composed x = g (f x) in
  composed in
let rec dbl x = x + x in
let rec inc x = x + 1 in
(* let rec dec x = x + 3 in *)
let h = compose inc (compose inc dbl) in
(* let h = compose dbl dec in *)
(h 123)
(* print_int (h 123) *)
