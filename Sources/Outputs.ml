(* Author: Samuele Giraudo
 * Creation: 2024-07
 * Modifications: 2024-07
 *)

module Cla = Clauses
module Cnj = Conjunctions
module Cns = Constants
module Exp = Expressions
module Lst = Lists
module Prf = Proofs
module Prp = Properties
module Res = Resolutions
module Str = Strings
module Sub = Substitutions
module Var = Variables

(* Prints the string str as an error. *)
let print_error str =
    "?? " ^ str |> Str.csprintf Str.Red |> print_string;
    flush stdout

(* Prints the string str as an information. *)
let print_information_1 str =
    ">> " ^ str |> Str.csprintf Str.Blue |> print_string;
    flush stdout

(* Prints the string str as an information. *)
let print_information_2 str =
    ">> " ^ str |> Str.csprintf Str.Magenta |> print_string;
    flush stdout

(* Prints the string str as an information. *)
let print_information_3 str =
    ">> " ^ str |> Str.csprintf Str.Yellow |> print_string;
    flush stdout

(* Prints the string str as a success. *)
let print_success str =
    "!! " ^ str |> Str.csprintf Str.Green |> print_string;
    flush stdout

let add_spaces_buffer buffer nb =
    assert (nb >= 0);
    String.make nb ' ' |> Buffer.add_string buffer

let variable_to_buffered_string buffer v =
    "%" |> Str.csprintf Str.Green |> Buffer.add_string buffer;
    Var.name v |> Str.csprintf Str.Magenta |> Buffer.add_string buffer;
    "^" |> Str.csprintf Str.Green |> Buffer.add_string buffer;
    Var.level v |> string_of_int |> Str.csprintf Str.Green |> Buffer.add_string buffer

let constant_to_buffered_string buffer c =
    "'" |> Str.csprintf Str.Blue |> Buffer.add_string buffer;
    Cns.name c |> Str.csprintf Str.Blue |> Buffer.add_string buffer

let rec expression_to_buffered_string buffer with_clauses e =
    let rec aux e =
        match e with
            |Exp.Variable (_, v) -> variable_to_buffered_string buffer v
            |Exp.Constant (_, c, clauses) ->
                constant_to_buffered_string buffer c;
                if with_clauses && clauses <> [] then begin
                    " [" |> Str.csprintf Str.Magenta |> Buffer.add_string buffer;
                    clauses |> List.iter (clause_to_buffered_string buffer);
                    "]" |> Str.csprintf Str.Magenta |> Buffer.add_string buffer
                end
            |Exp.Self _ -> "@" |> Str.csprintf Str.Cyan |> Buffer.add_string buffer
            |Exp.Application (_, e1, e2) ->
                aux e1;
                " " |> Buffer.add_string buffer;
                if not (Prp.is_atomic e2) then
                    "(" |> Str.csprintf Str.Magenta |> Buffer.add_string buffer;
                aux e2;
                if not (Prp.is_atomic e2) then
                    ")" |> Str.csprintf Str.Magenta |> Buffer.add_string buffer
            |Exp.Alias (_, alias) -> alias |> Buffer.add_string buffer
            |Exp.AliasDefinition (_, alias, e1, e2) ->
                alias |> Buffer.add_string buffer;
                " = " |> Str.csprintf Str.Blue |> Buffer.add_string buffer;
                aux e1;
                " . " |> Str.csprintf Str.Blue |> Buffer.add_string buffer;
                aux e2
            |Exp.Put (_, path) ->
                "!" |> Str.csprintf Str.Blue |> Buffer.add_string buffer;
                path |> Buffer.add_string buffer
    in
    aux e

and clause_to_buffered_string buffer cl =
    " | " |> Str.csprintf Str.Red |> Buffer.add_string buffer;
    cl
    |> Cla.head
    |> Lst.iterate_with_interstices
        (fun e ->
            if Prp.is_atomic e then
                expression_to_buffered_string buffer false e
            else begin
                "(" |> Str.csprintf Str.Magenta |> Buffer.add_string buffer;
                expression_to_buffered_string buffer false e;
                ")" |> Str.csprintf Str.Magenta |> Buffer.add_string buffer
            end)
        (fun _ -> " " |> Buffer.add_string buffer);
    " # " |> Str.csprintf Str.Red |> Buffer.add_string buffer;
    cl |> Cla.body |> conjunction_to_buffered_string buffer

and conjunction_to_buffered_string buffer conj =
    let e_lst = Cnj.expressions conj in
    if e_lst = [] then
        "True" |> Str.csprintf Str.Green |> Buffer.add_string buffer
    else
        e_lst
        |> Lst.iterate_with_interstices
            (expression_to_buffered_string buffer false)
            (fun () -> " & " |> Str.csprintf Str.Red |> Buffer.add_string buffer)

let substitution_to_buffered_string buffer subs variables =
    let variables' =
        variables
        |> List.sort_uniq compare
        |> List.filter (fun v -> Sub.map subs v |> Option.is_some)
    in
    if variables' = [] then
        "Empty" |> Str.csprintf Str.Green |> Buffer.add_string buffer
    else
        variables'
        |> Lst.iterate_with_interstices
            (fun v ->
                variable_to_buffered_string buffer v;
                " = " |> Str.csprintf Str.Red |> Buffer.add_string buffer;
                expression_to_buffered_string buffer false (Sub.map subs v |> Option.get))
            (fun () -> " & " |> Str.csprintf Str.Red |> Buffer.add_string buffer)

let proof_to_buffered_string buffer pr =
    pr
    |> Prf.combine
    |> List.rev
    |> List.iter
        (fun (goal, conj, cl, sub) ->
            add_spaces_buffer buffer 8;
            "Current goal:\n" |> Str.csprintf Str.Black |> Buffer.add_string buffer;
            add_spaces_buffer buffer 12;
            expression_to_buffered_string buffer false goal;
            "\n" |> Buffer.add_string buffer;
            add_spaces_buffer buffer 8;
            "Selected clause:\n" |> Str.csprintf Str.Black |> Buffer.add_string buffer;
            add_spaces_buffer buffer 12;
            clause_to_buffered_string buffer cl;
            "\n" |> Buffer.add_string buffer;
            add_spaces_buffer buffer 8;
            "Unification:\n" |> Str.csprintf Str.Black |> Buffer.add_string buffer;
            add_spaces_buffer buffer 12;
            let vars = Cla.variables cl @ Prp.variables goal in
            substitution_to_buffered_string buffer sub vars;
            "\n" |> Buffer.add_string buffer;
            add_spaces_buffer buffer 8;
            "New goals:\n" |> Str.csprintf Str.Black |> Buffer.add_string buffer;
            add_spaces_buffer buffer 12;
            conjunction_to_buffered_string buffer conj;
            "\n" |> Buffer.add_string buffer;
            add_spaces_buffer buffer 8;
            "<=\n" |> Str.csprintf Str.Red |> Buffer.add_string buffer);
    add_spaces_buffer buffer 8;
    "Success" |> Str.csprintf Str.Black |> Buffer.add_string buffer

let resolution_to_buffered_string buffer verbosity resolution =
    Res.proofs resolution
    |> List.iteri
        (fun i proof ->
            if verbosity >= 1 then begin
                Printf.sprintf "Solution n°%d:\n" (i + 1)
                |> Str.csprintf Str.Black |> Buffer.add_string buffer;
                add_spaces_buffer buffer 4;
                Printf.sprintf "Depth: %d\n" (Prf.depth proof)
                |> Str.csprintf Str.Black |> Buffer.add_string buffer;
                add_spaces_buffer buffer 4;
                Printf.sprintf "Width: %d\n" (Prf.width proof)
                |> Str.csprintf Str.Black |> Buffer.add_string buffer;
                add_spaces_buffer buffer 4;
                Printf.sprintf "Query:\n"
                |> Str.csprintf Str.Black |> Buffer.add_string buffer;
                add_spaces_buffer buffer 8;
                let query = Prf.query proof in
                expression_to_buffered_string buffer false query;
                "\n" |> Buffer.add_string buffer;
                add_spaces_buffer buffer 4;
                Printf.sprintf "Answer:\n"
                |> Str.csprintf Str.Black |> Buffer.add_string buffer;
                add_spaces_buffer buffer 8;
                substitution_to_buffered_string
                    buffer
                    (Prf.last_substitution proof)
                    (Prp.variables query);
                "\n" |> Buffer.add_string buffer;
            end;
            if verbosity >= 2 then begin
                add_spaces_buffer buffer 4;
                Printf.sprintf "Proof:\n"
                |> Str.csprintf Str.Black |> Buffer.add_string buffer;
                proof_to_buffered_string buffer proof;
                "\n" |> Buffer.add_string buffer
            end);
    if verbosity >= 1 then begin
        "Exhaustive: "
        ^ (if Res.is_exhaustive resolution then "yes" else "no") ^ "\n"
        |> Str.csprintf Str.Black |> Buffer.add_string buffer;
        Printf.sprintf "Duration: %.2f s.\n" (Res.duration resolution)
        |> Str.csprintf Str.Black |> Buffer.add_string buffer
    end

