(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
}
rule token = parse
 | ([' ' '\t'])+                                      { token lexbuf  }
 | ['\n']                                             { Lexing.new_line lexbuf; token lexbuf  }
 | "(*" [^ '*']* '*' ( [^ ')'] [^ '*']* '*' )* ')'    { token lexbuf }  (* skip comments *)
 | " prim"                                            { TPRIME }
 | "IF"                                               { TIF }
 | "THEN"                                             { TTHEN }
 | "ELIF"                                             { TELIF }
 | "ELSE"                                             { TELSE }
 | ['a'-'z' 'A'-'Z']'_'['0'-'9']+ as s                { TVAR(s) }
 | ['a'-'z' 'A'-'Z']'\''+ as s                        { TVAR(s) }
 | ['a'-'z' 'A'-'Z'] as c                             { TVoS(String.make 1 c) }
 | ['0'-'9']+ as s                                    { TNUM(s) }
 | '+'                                                { TPLUS }
 | '*'                                                { TMULT }
 | '^'                                                { TPOW }
 | "<>"                                               { TNE }
 | ">="                                               { TGE }
 | "<="                                               { TLE }
 | '<'                                                { TLT }
 | '>'                                                { TGT }
 | '='                                                { TEQ }
 | '|'                                                { TBAR }
 | '&'                                                { TAMP }
 | '('                                                { TLPAREN }
 | ')'                                                { TRPAREN }
 | '{'                                                { TLBRACE }
 | '}'                                                { TRBRACE }
 | ','                                                { TCOMMA }
 | ':'                                                { TCOLON }
 | '\''                                               { TPRIME }
 | '_'                                                { TBLANK }
 | '-'                                                { TNEG }
 | eof                                                { TEOF }
