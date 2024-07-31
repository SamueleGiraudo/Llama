(* Author: Samuele Giraudo
 * Creation: 2024-06
 * Modifications: 2024-06, 2024-07
 *)

%token L_PAR R_PAR
%token L_BRACK R_BRACK
%token PERCENT
%token PRIME
%token AT
%token SHARP
%token PIPE
%token AMPERSAND
%token EQUAL
%token DOT
%token BANG
%token <string> CHAR_STRING
%token EOF

%start <Expressions.expressions> expression

%%


expression:
    |e=expression_1 EOF {e}


expression_1:
    |e=expression_2 {e}

   (* ALIAS = EXP_1 . EXP_2 *)
    |alias=CHAR_STRING EQUAL e1=expression_1 DOT e2=expression_1 {
        let fp = FilePositions.from_position $startpos in
        Expressions.AliasDefinition (fp, alias, e1, e2)
    }

expression_2:
    |e=expression_3 {e}

    (* EXP_1 EXP_2 *)
    |e1=expression_2 e2=expression_3 {
        let fp = FilePositions.from_position $startpos in
        Expressions.Application (fp, e1, e2)
    }

expression_3:
    (* (EXP) *)
    |L_PAR e=expression_1 R_PAR {e}

    (* %VAR *)
    |PERCENT name=CHAR_STRING {
        let fp = FilePositions.from_position $startpos in
        let v = Variables.make name in
        Expressions.Variable (fp, v)
    }

    (* 'CONST *)
    |PRIME name=CHAR_STRING {
        let fp = FilePositions.from_position $startpos in
        let c = Constants.make name in
        Expressions.Constant (fp, c, [])
    }

    (* 'CONST CLAUSES *)
    |PRIME name=CHAR_STRING clauses=clauses {
        let fp = FilePositions.from_position $startpos in
        let c = Constants.make name in
        Expressions.Constant (fp, c, clauses)
    }

    (* @ *)
    |AT {
        let fp = FilePositions.from_position $startpos in
        Expressions.Self fp
    }

    (* ALIAS *)
    |alias=CHAR_STRING {
        let fp = FilePositions.from_position $startpos in
        Expressions.Alias (fp, alias)
    }

    (* ! PATH *)
    |BANG path=CHAR_STRING {
        let fp = FilePositions.from_position $startpos in
        Expressions.Put (fp, path)
    }


clauses:
    (* [ CLAUSE_1 ... CLAUSE_k ] *)
    |L_BRACK clauses=list(clause) R_BRACK {
        clauses
    }


clause:
    (* | EXP_1 ... EXP_k # CONJUNCTION *)
    |PIPE e_lst=list(expression_3) SHARP conj=conjunction {
        Expressions.Clause (e_lst, conj)
    }


conjunction:
    (* EXP_1 & ... & EXP_k *)
    |e_lst=separated_list(AMPERSAND, expression_1) {
        Expressions.Conjunction e_lst
    }

