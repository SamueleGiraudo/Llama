(* Author: Samuele Giraudo
 * Creation: 2024-06
 * Modifications: 2024-06, 2024-07
 *)

(* Returns def if the optional value opt is None. Otherwise, returns the value carried by
 * opt. *)
let value def opt =
    match opt with
        |Some x -> x
        |None -> def

(* Returns the image by the map f of the value of the optional value of opt if any. None
 * is returned otherwise. *)
let bind f opt =
    match opt with
        |None -> None
        |Some x -> f x

