(* Author: Samuele Giraudo
 * Creation: 2024-06
 * Modifications: 2024-06, 2024-07
 *)

module Cla = Clauses
module Exp = Expressions
module Lex = Lexer
module Par = Parser
module Pat = Paths

(* The extension of Llama files. *)
let extension = ".llm"

(* The maximal length for a variable. *)
let max_variable_length = 1024

(* The maximal length for a constant. *)
let max_constant_length = 1024

(* The maximal length for an alias. *)
let max_alias_length = 1024

(* The maximal length for a path. *)
let max_path_length = 1024

(* Tests if the character c is an alphabetic character. *)
let is_alpha_character c =
    ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

(* Tests if the character c is a numerical character. *)
let is_numerical_character c =
    '0' <= c && c <= '9'

(* Tests if the character c is a special character. *)
let is_special_character c =
     c = '_' || c = '-'

let is_path_character c =
    c = '.' || c = '/'

(* Tests if the character c is a character allowed in identifiers. *)
let is_plain_character c =
     is_alpha_character c || is_numerical_character c || is_special_character c

(* Tests if the string str is plain, that is, if it has length 1 or more and it is made of
 * plain characters. *)
let is_plain_string str =
    String.length str >= 1 && str |> String.for_all is_plain_character

(* Tests if the string str can be a variable. *)
let is_variable_name str =
    let len = String.length str in
    1 <= len && len <= max_variable_length && is_plain_string str

(* Tests if the string str can be a constant. *)
let is_constant_name str =
    let len = String.length str in
    1 <= len && len <= max_constant_length && is_plain_string str

(* Tests if the string str can be an alias. *)
let is_alias str =
    let len = String.length str in
    1 <= len && len <= max_alias_length && is_plain_string str

(* Tests if the string str can be a path of an included file. *)
let is_inclusion_path str =
    let len = String.length str in
    (1 + String.length extension) <= len && len <= max_path_length
    && Pat.has_extension extension str
    && str |> String.for_all (fun c -> is_plain_character c || is_path_character c)

(* Returns the expression e obtained by adding to all its inclusion paths the prefix
 * pref. *)
let complete_inclusion_paths pref e =
    let rec aux e =
        match e with
            |Exp.Variable _ |Exp.Self _ |Exp.Alias _ -> e
            |Exp.Constant (fp, c, clauses) ->
                Exp.Constant (fp, c, clauses |> List.map (Cla.map aux))
            |Exp.Application (fp, e1, e2) -> Exp.Application (fp, aux e1, aux e2)
            |Exp.AliasDefinition (fp, alias, e1, e2) ->
                Exp.AliasDefinition (fp, alias, aux e1, aux e2)
            |Exp.Put (fp, path) -> Exp.Put (fp, pref ^ path)
    in
    aux e

let complete_constant_names pref e =
    let rec aux e =
        match e with
            |Exp.Variable _ |Exp.Self _ |Exp.Alias _ -> e
            |Exp.Constant (fp, c, clauses) ->
                let c' = Constants.set_prefix c pref in
                Exp.Constant (fp, c', clauses |> List.map (Cla.map aux))
            |Exp.Application (fp, e1, e2) -> Exp.Application (fp, aux e1, aux e2)
            |Exp.AliasDefinition (fp, alias, e1, e2) ->
                Exp.AliasDefinition (fp, alias, aux e1, aux e2)
            |Exp.Put (fp, path) -> Exp.Put (fp, path)
    in
    aux e

(* This cache is used to optimize the function path_to_expression. *)
let cache_path_to_expression = Hashtbl.create 1024

(* Returns the expression specified by the Llama file at path path. This path must be
 * canonical. The exception Lex.Error is raised when there are syntax errors in the
 * program. *)
let path_to_expression path =
    assert (Sys.file_exists path);
    assert (path = Pat.canonicalize path);
    match Hashtbl.find_opt cache_path_to_expression path with
        |Some e -> e
        |None ->
            let e =
                Lex.value_from_file_path path Par.expression Lex.read
                |> complete_inclusion_paths (Pat.trim path)
                |> complete_constant_names (Pat.remove_extension path)
            in
            Hashtbl.add cache_path_to_expression path e;
            e

(* Returns the list of the included paths in the expression e and recursively included by
 * the expressions of the included Llama files. *)
let included_paths e =
    let rec aux paths e =
        match e with
            |Exp.Variable _ |Exp.Self _ |Exp.Alias _ -> []
            |Exp.Constant (_, _, clauses) ->
                clauses
                |> List.map (fun cl -> cl |> Cla.fold (fun res e -> aux paths e @ res) [])
                |> List.flatten
            |Exp.Application (_, e1, e2) |Exp.AliasDefinition (_, _, e1, e2) ->
                aux paths e1 @ aux paths e2
            |Exp.Put (_, path) ->
                let path = Pat.add_extension extension path in
                if not (Sys.file_exists path) || not (is_inclusion_path path) then
                    []
                else
                    let path' = Pat.canonicalize path in
                    if List.mem path' paths then
                        [path']
                    else
                        try
                            let e0 = path_to_expression path' in
                            let paths' = path' :: paths in
                            path' :: aux paths' e0
                        with
                            |Lex.Error _ -> []
    in
    aux [] e |> List.sort_uniq compare

