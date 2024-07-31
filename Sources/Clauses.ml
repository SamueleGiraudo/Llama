(* Author: Samuele Giraudo
 * Creation: 2024-06
 * Modifications: 2024-06, 2024-07
 *)

module Cnj = Conjunctions
module Exp = Expressions

let head cl =
    let Exp.Clause (head, _) = cl in
    head

let body cl =
    let Exp.Clause (_, body) = cl in
    body

let map f cl =
    let Exp.Clause (head, body) = cl in
    Exp.Clause (head |> List.map f, body |> Cnj.map f)

let fold f s cl =
    let Exp.Clause (head, Exp.Conjunction e_lst) = cl in
    head @ e_lst |> List.fold_left f s

let variables cl =
    cl |> fold (fun res e -> res @ (Properties.variables e)) []

