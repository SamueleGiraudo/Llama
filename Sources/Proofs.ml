(* Author: Samuele Giraudo
 * Creation: 2024-06
 * Modifications: 2024-06, 2024-07
 *)

module Cnj = Conjunctions
module Exp = Expressions
module Lst = Lists
module Sub = Substitutions

type proofs = {
    query : Exp.expressions;
    goals : Exp.expressions list;
    conjunctions : Exp.conjunctions list;
    clauses : Exp.clauses list;
    substitutions : Sub.substitutions list
}

let from_expression e =
    {query = e; goals = []; conjunctions = []; clauses = []; substitutions = []}

let query pr =
    pr.query

let goals pr =
    pr.goals

let conjunctions pr =
    pr.conjunctions

let clauses pr =
    pr.clauses

let substitutions pr =
    pr.substitutions

let last_conjunction pr =
    if List.is_empty pr.conjunctions then
        Cnj.singleton pr.query
    else
        List.hd pr.conjunctions

let last_substitution pr =
    if List.is_empty pr.conjunctions then
        Sub.empty
    else
        List.hd pr.substitutions

let depth pr =
    List.length pr.conjunctions

let width pr =
    pr.conjunctions |> List.fold_left (fun res c -> max res (Cnj.length c)) 1

let combine pr =
    Lst.combine_4 pr.goals pr.conjunctions pr.clauses pr.substitutions

let update pr conj cl subs =
    let goal = Cnj.first (last_conjunction pr) |> Option.get in
    {pr with
        goals = goal :: pr.goals;
        conjunctions = conj :: pr.conjunctions;
        clauses = cl :: pr.clauses;
        substitutions = subs :: pr.substitutions
    }

