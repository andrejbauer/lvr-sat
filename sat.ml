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

(* Skoraj DPLL algoritem, ne upošteva čistih spremenljivk *)

exception TrueClause

(* Poenostavi stavek gelde na dano valuacijo *)
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

exception Unsatisfiable

let dpll p =
  let rec search v xs cs =
    try
      (* Poberemo iz stavkov cs tiste, ki jih znamo takoj obravnavati.
         Sproti si gradimo valuacijo v in seznam xs spremenljivk, ki jih
         je se treba obdelati. *)
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
          | _ :: _ ->
            (match xs with
              | [] -> assert false (* XXX to verjetno ni prav,
                                      premisli o izboljsavah in pravilnosti *)
              | x :: xs' ->
                (match search ((x, false) :: v) xs' cs with
                  | Some v -> Some v
                  | None -> search ((x, true) :: v) xs cs))
        end
    with
      | Unsatisfiable -> None
  in
  let CNF cs = cnf p in
  let xs = vars_cnf (CNF cs) in
    search [] xs cs
