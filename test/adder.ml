(* let rec make_adder x y =
   let u = y - x in
   let rec adder z = z + u in
   adder
   in ((make_adder 111 222) 777)
*)

let rec make_adder x =
  let rec adder y = x + y in
  adder
in ((make_adder 333) 777)
(* in print_int ((make_adder 333) 777) *)

(* ;;
   let rec adder x y = x + y in
   let rec make_adder x = (adder, x) in
   let (f, fv_x) = make_adder 333 in
   let i = f fv_x 777 in
   print_int i
   ;;
*)