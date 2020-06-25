(* Primeri formuli *)

open Bool

(* Seznam števil od a do b *)
let rec range a b =
  if a <= b then a :: range (a + 1) b else []

(* Kartezični produkt dveh seznamov *)
let prod lst1 lst2 =
  let rec prod acc (lst1, lst2) =
    match lst1 with
    | [] -> acc
    | x :: lst1 ->
      let acc =
        List.fold_left (fun acc y -> (x,y) :: acc) acc lst2
      in
        prod acc (lst1, lst2)
  in
    prod [] (lst1, lst2)

(* Latinski kvadrat (brez diagonal).
   Spremenljivke: (i,j,k) pomeni, da je na polju (i,j) število k *)
let latin n =
  let polja = prod (range 1 n) (range 1 n) in
    And [
     (* Vsako polje je zapolnjeno enkrat *)
      And (List.map (fun (i,j) ->
        Or (List.map (fun k ->
          And (List.map (fun m -> if k = m then Var (i,j,m) else Not (Var (i,j,m))) (range 1 n))
        ) (range 1 n))
      ) polja) ;
      (* Vrstice *)
      And (
        List.map (fun i ->
         And (List.map (fun k ->
           Or (List.map (fun j -> Var (i, j, k)) (range 1 n))
         ) (range 1 n))
        ) (range 1 n)
      ) ;
      (* Stolpci *)
      And (
        List.map (fun j ->
         And (List.map (fun k ->
           Or (List.map (fun i -> Var (i, j, k)) (range 1 n))
         ) (range 1 n))
        ) (range 1 n)
      )
    ]
