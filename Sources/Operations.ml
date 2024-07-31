(* Author: Samuele Giraudo
 * Creation: 2024-06
 * Modifications: 2024-06, 2024-07
 *)

module Cla = Clauses
module Exp = Expressions
module Fil = Files
module Pat = Paths
module Prp = Properties

(* Returns the expression obtained by putting the expression e as dominant expression
 * of the argument expressions of the list e_lst. *)
let left_member_clause_to_expression e e_lst =
    let fp = Prp.root_file_position e in
    e_lst |> List.fold_left (fun res e' -> Exp.Application (fp, res, e')) e

(* Returns the expression obtained by replacing all external selfs of the expression e by
 * the expression e_self. *)
let substitute_external_selfs e_self e =
    let rec aux e =
        match e with
            |Exp.Variable _ |Exp.Constant _ |Exp.Alias _ |Exp.Put _ -> e
            |Exp.Self _ -> e_self
            |Exp.Application (info, e1, e2) -> Exp.Application (info, aux e1, aux e2)
            |Exp.AliasDefinition (info, alias, e1, e2) ->
                Exp.AliasDefinition (info, alias, aux e1, aux e2)
    in
    aux e

(* Returns the expression obtained by replacing all free occurrences of the alias alias in
 * the expression e1 by the expression e2. *)
let substitute_free_aliases e1 alias e2 =
    let rec aux e1 =
        match e1 with
            |Exp.Variable _ |Exp.Self _ |Exp.Put _ -> e1
            |Exp.Constant (fp, c, clauses) ->
                Exp.Constant (fp, c, clauses |> List.map (Cla.map aux))
            |Exp.Application (fp, e1', e2') -> Exp.Application (fp, aux e1', aux e2')
            |Exp.Alias (_, alias') -> if alias' = alias then e2 else e1
            |Exp.AliasDefinition (fp, alias', e1', e2') ->
                let e2'' = if alias' = alias then e2' else aux e2' in
                Exp.AliasDefinition (fp, alias', aux e1', e2'')
    in
    aux e1

(* Returns the expression obtained from the expression e by resolving its inclusions. The
 * exception Lexer.Error is raised when there are syntax errors in the included files in the
 * program. *)
let resolve_inclusions e =
    let rec aux e =
        match e with
            |Exp.Variable _ |Exp.Self _ |Exp.Alias _ -> e
            |Exp.Constant (fp, c, clauses) ->
                Exp.Constant (fp, c, clauses |> List.map (Cla.map aux))
            |Exp.Application (fp, e1, e2) -> Exp.Application (fp, aux e1, aux e2)
            |Exp.AliasDefinition (fp, alias, e1, e2) ->
                Exp.AliasDefinition (fp, alias, aux e1, aux e2)
            |Exp.Put (_, path) ->
                let path' = path |> Pat.add_extension Fil.extension |> Pat.canonicalize in
                aux (Fil.path_to_expression path')
    in
    aux e

(* Returns the expression obtained by replacing each alias admitting a definition in the
 * expression e by its definition. *)
let resolve_alias_definitions e =
    let rec aux e =
        match e with
            |Exp.Variable _ |Exp.Alias _ |Exp.Self _ |Exp.Put _ -> e
            |Exp.Constant (fp, c, clauses) ->
                Exp.Constant (fp, c, clauses |> List.map (Cla.map aux))
            |Exp.Application (fp, e1, e2) -> Exp.Application (fp, aux e1, aux e2)
            |Exp.AliasDefinition (_, alias, e1, e2) ->
                substitute_free_aliases (aux e2) alias (aux e1)
    in
    aux e

let set_level_external_variables level e =
    assert (level >= 0);
    let rec aux e =
        match e with
            |Exp.Variable (fp, v) -> Exp.Variable (fp, Variables.set_level v level)
            |Exp.Constant _ |Exp.Self _ |Exp.Alias _ |Exp.AliasDefinition _ |Exp.Put _ -> e
            |Exp.Application (fp, e1, e2) -> Exp.Application (fp, aux e1, aux e2)
    in
    aux e

