(* Author: Samuele Giraudo
 * Creation: 2024-07
 * Modifications: 2024-07
 *)

module Cla = Clauses
module Cnj = Conjunctions
module Exp = Expressions
module Lst = Lists
module Ope = Operations
module Par = Parameters
module Prf = Proofs
module Prp = Properties
module Sub = Substitutions

type resolutions = {
    proofs : Prf.proofs list;
    is_exhaustive : bool;
    duration : float
}

let empty =
    {proofs = []; is_exhaustive = true; duration = 0.0}

let proofs res =
    res.proofs

let is_exhaustive res =
    res.is_exhaustive

let duration res =
    res.duration

let truncate param res =
    match Par.max_cardinality param with
        |None -> res
        |Some max_cardinality ->
            if List.length res.proofs > max_cardinality then
                {res with
                    proofs = Lst.prefix max_cardinality res.proofs; is_exhaustive = false}
            else
                res

let concatenate res_1 res_2 =
    {
        proofs = res_1.proofs @ res_2.proofs;
        is_exhaustive = res_1.is_exhaustive && res_2.is_exhaustive;
        duration = res_1.duration +. res_2.duration
    }

let prerequisites subs level e =
    assert (level >= 0);
    match Prp.dominant_leaf e with
        |Exp.Constant (_, _, clauses) as e' ->
            clauses
            |> List.fold_left
                (fun res cl ->
                    let cl' = cl |> Cla.map (Ope.set_level_external_variables level) in
                    let Exp.Clause (e_lst_1, conj) = cl' in
                    let e1 = Ope.left_member_clause_to_expression e' e_lst_1 in
                    match Sub.unify subs e e1 with
                        |None -> res
                        |Some subs' ->
                            let goals =
                                conj |> Cnj.map (Ope.substitute_external_selfs e')
                            in
                            (goals, cl', subs') :: res)
                []
            |> List.rev
        |_ -> []

let resolve e param =
    let rec aux pr depth =
        let goals = Prf.last_conjunction pr in
        if Cnj.is_empty goals then
            {proofs = [pr]; is_exhaustive = true; duration = 0.0}
        else if not (Par.is_compatible_depth param depth) then
            {proofs = []; is_exhaustive = false; duration = 0.0}
        else
            let goal = Cnj.first goals |> Option.get in
            prerequisites (Proofs.last_substitution pr) depth goal
            |> List.fold_left
                (fun res (goals', cl', subs') ->
                    let len = List.length res.proofs in
                    if not (Par.is_compatible_cardinality param len) then
                        res
                    else
                        let goals'' =
                            goals
                            |> Cnj.replace_first goals'
                            |> Cnj.map (Sub.apply_on_expression subs')
                        in
                        let proof' = Prf.update pr goals'' cl' subs' in
                        concatenate res (aux proof' (depth + 1)))
                {proofs = []; is_exhaustive = true; duration = 0.0}
            |> truncate param
    in
    let clock_start = Unix.gettimeofday () in
    let res = aux (Prf.from_expression e) 1 in
    let clock_end = Unix.gettimeofday () in
    {res with duration = clock_end -. clock_start}

