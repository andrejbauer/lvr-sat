type bool =
  | True
  | False
  | Var of string
  | Not of bool
  | Or of bool list
  | And of bool list

(* Valuacijo predstavimo s seznamom parov (asociativni seznam)
   [(x1, b1); (x2, b2); ...; (xn, bn)]
   Primer: [("p", true); ("q", false)] *)

let rec vrednost v p =
  match p with
    | True -> true
    | False -> false
    | Var x -> List.assoc x v
    | Not q -> not (vrednost v q) 
    | Or qs -> List.fold_left (fun b q -> b || vrednost v q) false qs
    | And qs -> List.fold_left (fun b q -> b && vrednost v q) true qs

(* Negation normal form *)
let rec nnf p =
  match p with
    | True -> True
    | False -> False
    | Var x -> Var x
    | Not True -> False
    | Not False -> True
    | Not (Var x) -> Not (Var x)
    | Not (Not q) -> nnf q
    | Not (Or qs) -> And (List.map (fun q -> nnf (Not q)) qs)
    | Not (And qs) -> Or (List.map (fun q -> nnf (Not q)) qs)
    | Or qs -> Or (List.map nnf qs)
    | And qs -> And (List.map nnf qs)

let rec simplify p =
  let rec insert x lst =
    match lst with
      | [] -> [x]
      | y :: ys ->
        let cmp = compare x y in
          if cmp < 0 then x :: lst
          else if cmp = 0 then lst
          else y :: (insert x ys)
  in

  let rec sort_uniq sorted unsorted =
    match unsorted with
      | [] -> sorted
      | x :: xs -> sort_uniq (insert x sorted) xs
  in        

  match nnf p with
    | False -> False
    | True -> True
    | Var x -> Var x
    | Not (Var x) -> Not (Var x)
    | Not _ -> assert false
    | Or qs ->
      (* Poenostavimo qs, obrdžimo tiste, ki niso False, uredimo *)
      let rs =
        List.sort compare (List.filter (fun r -> r <> False) (List.map simplify qs))
      in
        (match rs with
          | [] -> False
          | [r] -> r
          | rs when List.mem True rs -> True
          | rs -> Or (sort_uniq [] rs)
        )
    | And qs ->
      (* Poenostavimo qs, obrdžimo tiste, ki niso False, uredimo *)
      let rs =
        List.sort compare (List.filter (fun r -> r <> True) (List.map simplify qs))
      in
        (match rs with
          | [] -> True
          | [r] -> r
          | rs when List.mem False rs -> False
          | rs -> And (sort_uniq [] rs)
        )
