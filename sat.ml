open Bool

(* SAT z golo silo *)
let brute_force p =
  let rec search v = function
    | [] -> if vrednost v p then Some v else None
    | x :: xs ->
      (match search ((x,false) :: v) xs with
        | Some w -> Some w
        | None -> search ((x,true) :: v) xs)
  in
    search [] (vars p)

(* SAT preko CNF *)

exception TrueClause

let rec eval_clause v ls =
  let rec eval = function
    | [] -> []
    | (Lit x) :: ls ->
      (match lookup x v with
        | None -> Lit x :: eval ls
        | Some false -> eval ls
        | Some true -> raise TrueClause)
    | (Til x) :: ls ->
      (match lookup x v with
        | None -> Til x :: eval ls
        | Some true -> eval ls
        | Some false -> raise TrueClause)
  in
    try
      Some (eval ls)
    with
      | TrueClause -> None

(* Skoraj DPLL algoritem, ne upošteva čistih spremenljivk *)
exception Unsatisfiable

let dpll p =
  let rec search v xs cs =
    try
      let (v, xs, cs) =
        List.fold_left
          (fun (v, xs, cs) (Clause c) ->
            match eval_clause v c with
              | None -> (v, xs, cs) 
              | Some [] -> raise Unsatisfiable
              | Some [Lit x] -> ((x,true) :: v, del x xs, cs)
              | Some [Til x] -> ((x,false) :: v, del x xs, cs)
              | Some ls -> (v, xs, Clause ls :: cs))
          (v, xs, []) cs
      in
        begin match cs with
          | [] -> Some v
          | _ ->
            (match xs with
              | [] -> assert false
              | x :: xs ->
                (match search ((x, false) :: v) xs cs with
                  | Some v -> Some v
                  | None -> search ((x, true) :: v) xs cs))
        end
    with
      | Unsatisfiable -> None
  in
  let CNF cs = cnf p in
  let xs = vars_cnf (CNF cs) in
    search [] xs cs
