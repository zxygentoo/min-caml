(* 「素朴」なknown function optimizationでは駄目な場合 *)
(* Cf. http://www.yl.is.s.u-tokyo.ac.jp/~sumii/pub/compiler-enshu-2002/Mail/8 *)
let rec f x = x + 123 in
let rec g y = f in
((g 456) 789)

(* ;;
   let rec f x = x + 123 in
   let rec g x _y = (f, x) in
   let (f, fv) = g 456 in
   let i = f 789 in
   print_int i
   ;; *)
