(* Author: Samuele Giraudo
 * Creation: 2024-06
 * Modifications: 2024-06, 2024-07
 *)

module Cla = Clauses
module Cnj = Conjunctions
module Cns = Constants
module Exp = Expressions
module Fil = Files
module Lex = Lexer
module Lst = Lists
module Pat = Paths
module Str = Strings
module Var = Variables

(* The error kinds a Llama program can contain. *)
type kinds =
    |InvalidVariableName of string
    |InvalidConstantName of string
    |InvalidAliasName of string
    |UnknownAlias of string
    |InvalidSelfPosition
    |AmbiguousConstant of Cns.constants
    |InvalidInclusionPath of string
    |CircularInclusion of string
    |SyntaxError of Lex.error_kinds

(* A type to represent information about an error. *)
type errors = {
    (* The kind of the error. *)
    kind : kinds;

    (* Information about the subexpression where the error appears. *)
    file_position : FilePositions.file_positions
}

(* Returns the error obtained from the lexer error err. The kind of the returned error is
 * SyntaxError. *)
let syntax_error_from_lexer err =
    let kind = SyntaxError (Lex.error_to_error_kind err) in
    let file_position = Lex.error_to_position err in
    {kind = kind; file_position = file_position}

(* Returns a string representation of the error err. *)
let to_string err =
    FilePositions.to_string err.file_position ^ ":\n" ^
    match err.kind with
        |InvalidVariableName v ->
            "invalid variable name\n" ^ (v |> Str.indent 4)
        |InvalidConstantName c ->
            "invalid constant name\n" ^ (c |> Str.indent 4)
        |InvalidAliasName alias ->
            "invalid alias name\n" ^ (alias |> Str.indent 4)
        |UnknownAlias alias ->
            "unknown alias\n" ^ (alias |> Str.indent 4)
        |InvalidSelfPosition -> "invalid self position"
        |AmbiguousConstant c ->
            "already existing constant\n" ^ (c |> Cns.name |> Str.indent 4)
        |InvalidInclusionPath path ->
            "invalid inclusion path\n" ^ (path |> Str.indent 4)
        |CircularInclusion path ->
            "circular inclusion involving\n" ^ (path |> Str.indent 4)
        |SyntaxError err -> Lex.error_kind_to_string err

(* Returns the list of the inclusion errors in the expression e and recursively in the
 * expressions of the included Llama files. *)
let inclusion_errors e =
    let rec aux paths e =
        match e with
            |Exp.Variable _ |Exp.Self _ |Exp.Alias _ -> []
            |Exp.Constant (_, _, clauses) ->
                clauses
                |> List.map (fun cl -> cl |> Cla.fold (fun res e -> aux paths e @ res) [])
                |> List.flatten
            |Exp.Application (_, e1, e2) -> aux paths e1 @ aux paths e2
            |Exp.AliasDefinition (_, _, e1, e2) -> aux paths e1 @ aux paths e2
            |Exp.Put (fp, path) ->
                let path = Pat.add_extension Fil.extension path in
                if not (Sys.file_exists path) || not (Fil.is_inclusion_path path) then
                    [{kind = InvalidInclusionPath path; file_position = fp}]
                else
                    let path' = Pat.canonicalize path in
                    if List.mem path' paths then
                        []
                    else
                        try
                            let e0 = Fil.path_to_expression path' in
                            let paths' = path' :: paths in
                            if List.mem path' (Fil.included_paths e0) then
                                [{kind = CircularInclusion path'; file_position = fp}]
                            else
                                aux paths' e0
                        with
                            |Lex.Error err -> [syntax_error_from_lexer err]
    in
    aux [] e |> List.sort_uniq compare

let invalid_name_errors e =
    let rec aux e =
        match e with
            |Exp.Variable (fp, v) ->
                let name = Var.name v in
                if Fil.is_variable_name name then
                    []
                else
                    [{kind = InvalidVariableName name; file_position = fp}]
            |Exp.Constant (fp, c, clauses) ->
                let tmp =
                    clauses
                    |> List.map (fun cl -> cl |> Cla.fold (fun res e -> aux e @ res) [])
                    |> List.flatten
                in
                let name = Cns.name c in
                if Fil.is_constant_name name then
                    tmp
                else
                    {kind = InvalidConstantName name; file_position = fp} :: tmp
            |Exp.Self _ |Exp.Put _ -> []
            |Exp.Application (_, e1, e2) -> aux e1 @ aux e2
            |Exp.Alias (fp, alias) ->
                if Fil.is_alias alias then
                    []
                else
                    [{kind = InvalidAliasName alias; file_position = fp}]
            |Exp.AliasDefinition (fp, alias, e1, e2) ->
                let tmp = aux e1 @ aux e2 in
                if Fil.is_alias alias then
                    tmp
                else
                    {kind = InvalidAliasName alias; file_position = fp} :: tmp
    in
    aux e |> List.sort_uniq compare

let unknown_alias_errors e =
    let rec aux e =
        match e with
            |Exp.Variable _ |Exp.Self _ |Exp.Put _ -> []
            |Exp.Constant (_, _, clauses) ->
                clauses
                |> List.map (fun cl -> cl |> Cla.fold (fun res e -> aux e @ res) [])
                |> List.flatten
            |Exp.Application (_, e1, e2) -> aux e1 @ aux e2
            |Exp.Alias (fp, alias) -> [(alias, fp)]
            |Exp.AliasDefinition (_, alias, e1, e2) ->
                aux e1 @ (aux e2 |> List.filter (fun (alias', _) -> alias' <> alias))
    in
    aux e
    |> List.map (fun (alias, fp) -> {kind = UnknownAlias alias; file_position = fp})
    |> List.sort_uniq compare

let invalid_self_position_errors e =
    let rec aux allowed e =
        match e with
            |Exp.Variable _ |Exp.Alias _ |Exp.Put _ -> []
            |Exp.Constant (_, _, clauses) ->
                let tmp_1 =
                    clauses
                    |> List.map
                        (fun cl -> cl |> Cla.head |> List.map (aux false) |> List.flatten)
                    |> List.flatten
                in
                let tmp_2 =
                    clauses
                    |> List.map
                        (fun cl ->
                            cl
                            |> Cla.body
                            |> Cnj.expressions
                            |> List.map (aux true)
                            |> List.flatten)
                    |> List.flatten
                in
                tmp_1 @ tmp_2
            |Exp.Self fp ->
                if allowed then
                    []
                else
                    [{kind = InvalidSelfPosition; file_position = fp}]
            |Exp.Application (_, e1, e2) |Exp.AliasDefinition (_, _, e1, e2) ->
                aux allowed e1 @ aux allowed e2
    in
    aux false e

(* Returns the list of the errors about the ambiguous constants in expression e. Two
 * constants are ambiguous if they have the same name but different clauses. *)
let ambiguous_constant_errors e =
    let rec aux e =
        match e with
            |Exp.Variable _ |Exp.Self _ |Exp.Alias _ |Exp.Put _ -> []
            |Exp.Constant (fp, c, clauses) ->
                let tmp =
                    clauses
                    |> List.map (fun cl -> cl |> Cla.fold (fun res e -> aux e @ res) [])
                    |> List.flatten
                in
                (fp, c, clauses) :: tmp
            |Exp.Application (_, e1, e2) |Exp.AliasDefinition (_, _, e1, e2) ->
                aux e1 @ aux e2
    in
    e
    |> aux
    |> List.sort_uniq compare
    |> Lst.triangle_product
    |> List.filter
        (fun ((_, c, clauses), (_, c', clauses')) -> c = c' && clauses <> clauses')
    |> List.map (fun ((fp, c, _), _) -> {kind = AmbiguousConstant c; file_position = fp})
    |> List.sort_uniq compare

