(* Primeri formuli *)

open Bool

(* Seznam n spremenljivk *)
let rec range = function
  | 0 -> []
  | n -> Var n :: range (n - 1)
