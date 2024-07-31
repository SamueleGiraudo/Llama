(* Author: Samuele Giraudo
 * Creation: 2024-06
 * Modifications: 2024-06, 2024-07
 *)

(* A type for the expressions of the Llama language. *)
type expressions =

    (* The nodes of the term expressions. *)
    |Variable of (FilePositions.file_positions * Variables.variables)
    |Constant of (FilePositions.file_positions * Constants.constants * (clauses list))
    |Self of FilePositions.file_positions
    |Application of (FilePositions.file_positions * expressions * expressions)

    (* The meta-nodes of the expressions for the management of aliases. *)
    |Alias of (FilePositions.file_positions * string)
    |AliasDefinition of (FilePositions.file_positions * string * expressions * expressions)

    (* The meta-node of the expressions for the management of inclusions. *)
    |Put of (FilePositions.file_positions * string)

and clauses = Clause of ((expressions list) * conjunctions)

and conjunctions = Conjunction of (expressions list)

(* An exception to handle cases where an expression has a inappropriate form. *)
exception ValueError of (expressions * string)

(* Raise the exception ValueError with the expression e and the string msg as
 * information. *)
let error e msg =
    ValueError (e, msg) |> raise

