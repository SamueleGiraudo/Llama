(* Author: Samuele Giraudo
 * Creation: 2024-07
 * Modifications: 2024-07
 *)

module Opt = Options

type parameters = {
    max_depth : int option;
    max_cardinality : int option
}

let default =
    {max_depth = None; max_cardinality = None}

let make max_depth max_cardinality =
    assert (max_depth |> Opt.value 0 >= 0);
    assert (max_cardinality |> Opt.value 0 >= 0);
    {max_depth = max_depth; max_cardinality = max_cardinality}

let max_depth param =
    param.max_depth

let max_cardinality param =
    param.max_cardinality

let is_compatible_depth param depth =
    match param.max_depth with
        |None -> true
        |Some depth' -> depth <= depth'

let is_compatible_cardinality param cardinality =
    match param.max_cardinality with
        |None -> true
        |Some cardinality' -> cardinality <= cardinality'

