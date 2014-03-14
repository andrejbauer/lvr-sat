(* Podatkovni tip za predstavitev boolovih izrazov *)

type bool =
  | True
  | False
  | Var of string
  | Not of bool
  | Or of bool list
  | And of bool list

(* Podatkovni tipi za predstavitev CNF izrazov *)

type literal =
  | Lit of string
  | Til of string

type clause = Clause of literal list

type cnf = CNF of clause list

(* Nekaj pomoznih funkcij *)

let add x lst =  
  if List.mem x lst then lst else x :: lst

let rec del x = function
  | [] -> []
  | y :: lst -> if x = y then lst else y :: del x lst
  
let rec lookup x = function
  | [] -> None
  | (y,z) :: lst -> if x = y then Some z else lookup x lst

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

(* Spremenljivke, ki se pojavljajo v formuli *)
let vars p =
  let rec collect xs = function
    | True -> xs
    | False -> xs
    | Var x -> add x xs
    | Not p -> collect xs p
    | And lst -> List.fold_left collect xs lst
    | Or lst -> List.fold_left collect xs lst
  in
    collect [] p

(* Tilation normal form *)
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

(* Conjunctive normal form *)
let cnf p =
  let rec convert cs = function
    | True -> cs
    | False -> [Clause []]
    | Var x -> (Clause [Lit x]) :: cs
    | Not (Var x) -> (Clause [Til x]) :: cs
    | Not _ -> assert false
    | And ps -> List.fold_left convert cs ps
    | Or [] -> [Clause []]
    | Or [p] -> convert cs p
    | Or (p :: ps) ->
      let ds = convert [] p in
      let es = convert [] (Or ps) in
        List.fold_left
          (fun cs (Clause d) ->
            List.fold_left (fun cs (Clause e) -> (Clause (d @ e)) :: cs) cs es)
          cs ds
  in
    CNF (convert [] (nnf p))

(* Spremenljivke v CNF formuli *)
let vars_cnf (CNF cs) =
  List.fold_left
    (fun xs (Clause c) ->
      List.fold_left
        (fun xs l ->
          match l with
            | Lit x -> x :: xs
            | Til x -> x :: xs)
        xs c)
    [] cs

