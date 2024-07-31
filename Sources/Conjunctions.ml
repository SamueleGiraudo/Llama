(* Author: Samuele Giraudo
 * Creation: 2024-06
 * Modifications: 2024-06, 2024-07
 *)

module Exp = Expressions
module Opt = Options
module Prp = Properties

let empty =
    Exp.Conjunction []

let singleton e =
    Exp.Conjunction [e]

let expressions conj =
    let Exp.Conjunction e_lst = conj in
    e_lst

let is_empty conj =
    conj |> expressions |> List.is_empty

let first conj =
    match expressions conj with
        |e :: _ -> Some e
        |_ -> None

let rest conj =
    match expressions conj with
        |_ :: e_lst -> Some (Exp.Conjunction e_lst)
        |_ -> None

let length conj =
    conj |> expressions |> List.length

let variables conj =
    conj
    |> expressions
    |> List.map Prp.variables
    |> List.concat
    |> List.sort_uniq compare

let map f conj =
    Exp.Conjunction (conj |> expressions |> List.map f)

(* Returns the conjunction obtained formed by the terms of the conjunctions conj_1 and
 * conjunction_2 where the first term of conj_2 is removed. *)
let replace_first conj_1 conj_2 =
    let e_lst = expressions conj_1 @ (rest conj_2 |> Opt.value empty |> expressions) in
    Exp.Conjunction e_lst

