(* Author: Samuele Giraudo
 * Creation: 2024-07
 * Modifications: 2024-07
 *)

module Exp = Expressions

let root_file_position e =
    match e with
        |Exp.Variable (fp, _) |Exp.Constant (fp, _, _) |Exp.Self fp
        |Exp.Application (fp, _, _) |Exp.Alias (fp, _) |Exp.AliasDefinition (fp, _, _, _)
        |Exp.Put (fp, _) ->
            fp

let has_external_variable e v =
    let rec aux e =
        match e with
            |Exp.Variable (_, v') -> v' = v
            |Exp.Constant _ |Exp.Self _ |Exp.Alias _ |Exp.Put _ -> false
            |Exp.Application (_, e1, e2) |Exp.AliasDefinition (_, _, e1, e2) ->
                aux e1 || aux e2
    in
    aux e

let variables e =
    let rec aux e =
        match e with
            |Exp.Variable (_, v) -> [v]
            |Exp.Constant _ |Exp.Self _ |Exp.Alias _ |Exp.AliasDefinition _ |Exp.Put _ -> []
            |Exp.Application (_, e1, e2) -> aux e1 @ aux e2
    in
    aux e

let dominant_leaf e =
    let rec aux e =
        match e with
            |Exp.Variable _ | Exp.Constant _ |Exp.Self _ |Exp.Alias _ |Exp.Put _ -> e
            |Exp.Application (_, e1, _) |Exp.AliasDefinition (_, _, e1, _) -> aux e1
    in
    aux e

(* Tests of the expression e is atomic. This is the case when e is a variable, a constant,
 * a self, an alias, or a put expression. *)
let is_atomic e =
    match e with
        |Exp.Variable _ |Exp.Constant _ |Exp.Self _ |Exp.Alias _ |Exp.Put _ -> true
        |Exp.Application _ |Exp.AliasDefinition _ -> false

