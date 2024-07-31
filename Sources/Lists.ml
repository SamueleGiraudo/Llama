(* Author: Samuele Giraudo
 * Creation: 2024-07
 * Modifications: 2024-07
 *)

(* Returns the list of the pairs (a, b) such that a appears before b in the list lst. *)
let rec triangle_product lst =
    match lst with
        |[] -> []
        |x :: lst' -> (lst' |> List.map (fun y -> (x, y))) @ (triangle_product lst')

let iterate_with_interstices f interstice lst =
    if lst <> [] then begin
        f (List.hd lst);
        lst
        |> List.tl
        |> List.iter (fun x -> interstice (); f x)
    end

let rec prefix len lst =
    assert (len >= 0);
    match lst, len with
        |[], _ |_, 0 -> []
        |x :: lst', _ -> x :: prefix (len - 1) lst'

let combine_4 lst1 lst2 lst3 lst4 =
    List.combine (List.combine (List.combine lst1 lst2) lst3) lst4
    |> List.map (fun (((x1, x2), x3), x4) -> (x1, x2, x3, x4))

