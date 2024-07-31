(* Author: Samuele Giraudo
 * Creation: 2024-06
 * Modifications: 2024-06, 2024-07
 *)

module Err = Errors
module Exp = Expressions
module Fil = Files
module Lex = Lexer
module Ope = Operations
module Par = Parameters
module Pat = Paths
module Res = Resolutions

type environments = {
    expression : Exp.expressions option;
    parameters : Par.parameters;
    resolution : Res.resolutions;
    errors : Err.errors list
}

let empty =
    {expression = None; parameters = Par.default; resolution = Res.empty; errors = []}

let has_expression env =
    Option.is_some env.expression

let expression env =
    assert (has_expression env);
    Option.get env.expression

let parameters env =
    env.parameters

let resolution env =
    env.resolution

let errors env =
    env.errors

let has_errors env =
    env.errors <> []

let check_errors f env =
    if has_errors env then
        env
    else
        match env.expression with
            |None -> env
            |Some e -> {env with errors = f e}

let map_expression f env =
    if has_errors env then
        env
    else
        match env.expression with
            |None -> env
            |Some e -> {env with expression = Some (f e)}

let from_path path =
    assert (Sys.file_exists path);
    try
        let e = Fil.path_to_expression (Pat.canonicalize path) in
        {empty with expression = Some e}
    with
        |Lex.Error err ->
            let errs = [Err.syntax_error_from_lexer err] in
            {empty with errors = errs}

let pre_process env =
    env
    |> check_errors Err.inclusion_errors
    |> map_expression Ope.resolve_inclusions
    |> check_errors Err.invalid_name_errors
    |> check_errors Err.unknown_alias_errors
    |> map_expression Ope.resolve_alias_definitions
    |> check_errors Err.invalid_self_position_errors
    |> check_errors Err.ambiguous_constant_errors

let set_parameters param env =
    {env with parameters = param}

let resolve env =
    assert (has_expression env);
    assert (env |> has_errors |> not);
    let res = Res.resolve (expression env) env.parameters in
    {env with resolution = res}

