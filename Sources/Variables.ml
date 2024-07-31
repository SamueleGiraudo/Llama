(* Author: Samuele Giraudo
 * Creation: 2024-06
 * Modifications: 2024-06, 2024-07
 *)

type variables = {
    name : string;
    level : int
}

let make name =
    {name = name; level = 0}

let name v =
    v.name

let level v =
    v.level

let set_level v level =
    assert (level >= 0);
    {v with level = level}

