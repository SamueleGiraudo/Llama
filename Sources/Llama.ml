(* Author: Samuele Giraudo
 * Creation: 2024-06
 * Modifications: 2024-06, 2024-07
 *)

module Arg = Arguments
module Env = Environments
module Err = Errors
module Fil = Files
module Out = Outputs
module Par = Parameters
module Pat = Paths
module Str = Strings

(* TODO Rename directory and file as "Llama". *)
let name = "Llama"

let logo = "||__ /\\/\\ /\\/\\"

let description =
    "Llama: Logical Applicative Machine Abstraction."

(*
let version = "0.0001" and version_date = "2024-07-01"
*)
let version = "0.0010" and version_date = "2024-07-28"

let author = "Samuele Giraudo"

let email = "giraudo.samuele@uqam.fr"

(* Returns a string of information about the Llama program. *)
let information =
    Printf.sprintf "\n%s\n%s\n%s\nCopyright (C) 2024--2024 %s\nWritten by %s [%s]\n\
        Version: %s (%s)\n"
        logo name description author author email version version_date

(* Returns the help string about the arguments of the program. *)
let help_string =
      "Usage:\n    ./Llama [--help] [--version] --file PATH [--verbose L] \
      [--max-depth D] [--max-cardinality C]\nwhere:\n"
    ^ "    + `--help` prints the short help (the present text).\n"
    ^ "    + `--version` prints the version and other information.\n"
    ^ "    + `--file PATH` sets `PATH` as the path to the Llama program to consider, \
             contained in a " ^ Fil.extension ^ " file.\n"
    ^ "    + `--verbose L` enables the verbose mode at level `L`, from 0 (nothing) to 2 \
             (full). By default, the level is 1.\n"
    ^ "    + `--max-depth D` sets `D` as the maximal search depth in the resolution.\n"
    ^ "    + `--max-cardinality C` sets `C` as the maximal number of computed solutions.\n"

(* Prints the error message msg followed by a line break, and halts the execution. *)
let error msg =
    "Error: " ^ msg ^ "\n" |> Out.print_error;
    exit 1

(* Prints the errors of the data errors ee for the specified verbosity level verbosity. *)
let print_errors verbosity errs =
    Printf.sprintf
        "There are errors in the program:\n%s"
        (errs |> List.map Err.to_string |> String.concat "\n" |> Str.indent 4)
    |> Out.print_error;
    if verbosity >= 2 then begin
        print_newline ();
        "The program has not been evaluated." |> Out.print_information_1
    end;
    print_newline ();
    exit 1

(* Main function. *)
let () =
    if Arg.exists "--version" then begin
        Out.print_success information;
        exit 0
    end;

    if Arg.exists "--help" then begin
        Out.print_information_1 help_string;
        exit 0
    end;

    let path =
        match Arg.option_value "--file" with
            |None -> error "the option --file must be given and followed by one path."
            |Some path -> path
    in
    if Sys.file_exists path |> not then
       Printf.sprintf "there is no file %s." path |> error;
    if not (Pat.has_extension Fil.extension path) then
        Printf.sprintf "the file %s has not %s as extension." path Fil.extension |> error;

    let max_depth =
        if Arg.exists "--max-depth" |> not then
            None
        else
            match Arg.option_value "--max-depth" with
                |None ->
                    error
                        "the option --max-depth must be followed by a nonnegative integer."
                |Some d -> Some (int_of_string d)
    in

    let max_cardinality =
        if Arg.exists "--max-cardinality" |> not then
            None
        else
            match Arg.option_value "--max-cardinality" with
                |None ->
                    error
                        "the option --max-cardinality must be followed by a nonnegative \
                        integer."
                |Some d -> Some (int_of_string d)
    in

    let verbosity =
        if Arg.exists "--verbose" |> not then
            1
        else
            match Arg.bounded_integer_option_value 0 2 "--verbose" with
                |None ->
                    "one integer between 0 and 2 must follow the --verbose argument."
                    |> error
                |Some lvl -> lvl
    in

    let parameters = Par.make max_depth max_cardinality in
    let env = path |> Env.from_path |> Env.pre_process |> Env.set_parameters parameters in
    if Env.has_errors env then
        print_errors verbosity (Env.errors env);
    let env = Env.resolve env in
    let buffer = Buffer.create 1024 in
    env |> Env.resolution |> Out.resolution_to_buffered_string buffer verbosity;
    Buffer.output_buffer stdout buffer;

    ()

