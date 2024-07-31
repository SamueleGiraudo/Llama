(* Author: Samuele Giraudo
 * Creation: 2024-06
 * Modifications: 2024-06, 2024-07
 *)

module Exp = Expressions
module Opt = Options
module Prp = Properties

type substitutions =
    Substitution of (Variables.variables -> (Exp.expressions option))

let map subs =
    let Substitution map = subs in
    map

let empty =
    Substitution (Fun.const None)

let singleton v t =
    Substitution (fun v' -> if v' = v then Some t else None)

let apply_on_expression subs e =
    let rec aux e =
        match e with
            |Exp.Variable (_, v) -> map subs v |> Opt.value e
            |Exp.Constant _ |Exp.Self _ |Exp.Alias _ |Exp.Put _ -> e
            |Exp.Application (fp, e1, e2) -> Exp.Application (fp, aux e1, aux e2)
            |Exp.AliasDefinition (fp, alias, e1, e2) ->
                    Exp.AliasDefinition (fp, alias, aux e1, aux e2)
    in
    aux e

let add subs v e =
    let subs' = singleton v e in
    let map v' =
        match map subs v' with
            |None -> map subs' v'
            |Some e' -> Some (apply_on_expression subs' e')
    in
    Substitution map

let rec unify subs e1 e2 =
    match apply_on_expression subs e1, apply_on_expression subs e2 with
        |Exp.Variable (_, v1), Exp.Variable (_, v2) when v1 = v2 -> Some subs
        |Exp.Variable (_, v1), e2' when Prp.has_external_variable e2' v1 -> None
        |e1', Exp.Variable (_, v2) when Prp.has_external_variable e1' v2 -> None
        |Exp.Variable (_, v), e |e, Exp.Variable (_, v) -> Some (add subs v e)
        |Exp.Constant (_, c1, _), Exp.Constant (_, c2, _) when c1 = c2 -> Some subs
        |Exp.Self _, Exp.Self _ -> Some subs
        |Exp.Application (_, e1', e1''), Exp.Application (_, e2', e2'') ->
            unify subs e1' e2' |> Opt.bind (fun subs' -> unify subs' e1'' e2'')
        |Exp.AliasDefinition (_, alias_1, e1', e1''),
        Exp.AliasDefinition (_, alias_2, e2', e2'') when alias_1 = alias_2 ->
            unify subs e1' e2' |> Opt.bind (fun subs' -> unify subs' e1'' e2'')
        |Exp.Put (_, path_1), Exp.Put (_, path_2) when path_1 = path_2 -> Some subs
        |_ -> None

