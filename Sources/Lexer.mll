(* Author: Samuele Giraudo
 * Creation: 2024-06
 * Modifications: 2024-06, 2024-07
 *)

{

module Fpo = FilePositions
module Par = Parser

(* A type for the kinds of possible errors during the phase of the lexer. *)
type error_kinds =
    |UnclosedComent
    |UnexpectedCharacter of char
    |Parsing

(* A type to communicate about parsing or lexing errors. *)
type errors = {
    position: Fpo.file_positions;
    kind: error_kinds
}

(* An exception raised when an error is encountered. *)
exception Error of errors

(* Returns the kind of the error err. *)
let error_to_error_kind err =
    err.kind

(* Returns the file position of the error err. *)
let error_to_position err =
    err.position

(* Returns a string representation of the error kind ek. *)
let error_kind_to_string ek =
    match ek with
        |UnclosedComent -> "unclosed comment"
        |UnexpectedCharacter c -> Printf.sprintf "unexpected character %c" c
        |Parsing -> "parsing error"

(* Returns the file position obtained from the lexing buffer lexbuf. *)
let lexbuf_to_position lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    Fpo.make
        pos.Lexing.pos_fname
        pos.Lexing.pos_lnum
        (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)

(* Modifies the lexing buffer lexbuf so that it contains the next line. *)
let next_line lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <-
        {pos with
            Lexing.pos_bol = lexbuf.Lexing.lex_curr_pos;
            Lexing.pos_lnum = pos.Lexing.pos_lnum + 1}

(* Raises Error with information about the unexpected character c. *)
let unexpected_character_error lexbuf c =
    Error {position = lexbuf_to_position lexbuf; kind = UnexpectedCharacter c} |> raise

(* Raises Error with information about an unclosed comment. *)
let unclosed_comment_error lexbuf =
    Error {position = lexbuf_to_position lexbuf; kind = UnclosedComent} |> raise

(* Raises Error with information about a parsing error. *)
let parsing_error lexbuf =
    Error {position = lexbuf_to_position lexbuf; kind = Parsing} |> raise

(* Returns the value computed by the parser parser_axiom, with the lexer lexer_axiom, and
 * with the lexing buffer lexbuf. If there is an error, the exception Error is raised. *)
let parse_lexer_buffer parser_axiom lexer_axiom lexbuf =
    try
        parser_axiom lexer_axiom lexbuf
    with
        |Par.Error -> parsing_error lexbuf
        |Error e -> Error e |> raise

(* Returns the value contained in the file at path path, interpreted with the parser
 * parser_axiom, with the lexing bufer lexer_axiom. If an error is found, the exception
 * Error is raised. *)
let value_from_file_path path parser_axiom lexer_axiom =
    assert (Sys.file_exists path);
    let ch = open_in path in
    let str = really_input_string ch (in_channel_length ch) in
    close_in ch;
    let lexbuf = Lexing.from_string str in
    lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = path};
    parse_lexer_buffer parser_axiom lexer_axiom lexbuf

}

let letters = ['a'-'z' 'A'-'Z']
let digits = ['0'-'9']
let specials = ['_' '-' '.' '/']
let char_string = (letters | specials | digits)+

rule read = parse
    |" " |"\t" {read lexbuf}
    |"\n" {next_line lexbuf; read lexbuf}
    |"(" {Par.L_PAR}
    |")" {Par.R_PAR}
    |"[" {Par.L_BRACK}
    |"]" {Par.R_BRACK}
    |"'" {Par.PRIME}
    |"%" {Par.PERCENT}
    |"@" {Par.AT}
    |"#" {Par.SHARP}
    |"|" {Par.PIPE}
    |"&" {Par.AMPERSAND}
    |"=" {Par.EQUAL}
    |"." {Par.DOT}
    |"!" {Par.BANG}
    |char_string {Par.CHAR_STRING (Lexing.lexeme lexbuf)}
    |"{" {comment 0 lexbuf}
    |eof {Par.EOF}
    |_ as c {unexpected_character_error lexbuf c}

and comment level = parse
    |"\n" {next_line lexbuf; comment level lexbuf}
    |"}" {if level = 0 then read lexbuf else comment (level - 1) lexbuf}
    |"{" {comment (level + 1) lexbuf}
    |eof {unclosed_comment_error lexbuf}
    |_ {comment level lexbuf}

