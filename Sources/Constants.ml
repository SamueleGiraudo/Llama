(* Author: Samuele Giraudo
 * Creation: 2024-07
 * Modifications: 2024-07
 *)

(* A type for constants. A constant is specified by its name and its prefix. The prefix is
 * important to distinguish variables having the same name but coming from two different
 * namespaces. *)
type constants = {
    name : string;
    prefix : string
}

let make name =
    {name = name; prefix = ""}

(* Returns the name of the constant c. *)
let name c =
    c.name

let prefix c =
    c.prefix

let set_prefix c pref =
    {c with prefix = pref}

