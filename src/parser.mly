/* File parser.mly */
/* The Header */
%{
    open Languages;;

    let string_of_char_list l = List.fold_right (fun c -> fun s -> (String.make 1 c) ^ s) l ""
    let string_to_char_list s =
      let rec clos aux i = if i < 0 then aux else clos ((String.get s i)::aux) (i-1) in
      clos [] ((String.length s)-1)

    let rec word_of_number s =
      let l = String.length s in
      let c = Letter(String.sub s 0 1) in
      if l = 1 then c else Comp(c, word_of_number (String.sub s 1 (l-1)))
      
%}
/* The Grammar */
%token <string> TVAR, TVoS
%token <string> TNUM
%token TPLUS, TMULT, TPOW, TNEG, TAMP
%token TGE, TGT, TLE, TLT, TEQ, TNE
%token TPRIME
%token TLPAREN TRPAREN TLBRACE TRBRACE
%token TCOMMA TBAR TBLANK TCOLON TPRIME
%token TEOF
%token TIF TTHEN TELIF TELSE

/* lowest precedence */
%right TCOMMA
%nonassoc TELIF
%right TPLUS
%left  TNEG
%right TMULT
%right TPOW
/* highest precedence */

%start langexpr wordexpr natexpr predicate alphabet      /* the entry points */ 
%type <Languages.alphabet -> Languages.natexpr>    natexpr
%type <Languages.alphabet -> Languages.wordexpr>   wordexpr
%type <Languages.alphabet -> Languages.langexpr>   langexpr
%type <Languages.alphabet -> Languages.predicate>  predicate
%type <Languages.alphabet>                         alphabet
%%

natexpr: nexpr TEOF                   { $1 }
;
nexpr:
      nexp                            { $1 }
    | TIF conditionals TELSE nexp     { fun sigma -> Cases($2 sigma, $4 sigma) }
;
nexp:
      TVAR                            { fun _ -> Var($1) }
    | TVoS                            { fun _ -> Var($1) }
    | TNUM                            { fun _ -> Const(int_of_string $1) }  
    | nexp TPLUS nexp                 { fun sigma -> Languages.plus ($1 sigma) ($3 sigma) }
    | nexp TMULT nexp                 { fun sigma -> Languages.times ($1 sigma) ($3 sigma) }
    | TNEG nexp                       { fun sigma -> Languages.neg ($2 sigma) }
    | nexp TPOW nexp                  { fun sigma -> Languages.tothe ($1 sigma) ($3 sigma) }
    | TBAR wexpr TBAR                 { fun sigma -> Languages.length ($2 sigma) } 
    | TLPAREN nexp TRPAREN            { $2 } 
;
conditionals:
      nepreds TTHEN nexp                     { fun sigma -> [($1 sigma, $3 sigma)] }
    | nepreds TTHEN nexp TELIF conditionals  { fun sigma -> ($1 sigma, $3 sigma)::($5 sigma) } 
;

wordexpr: wexpr TEOF                 { $1 }
;
wexpr:
                                     { fun _ -> Epsilon }
    | newexpr                        { $1 }
;
newexpr:
      TVAR                           { fun _ -> Variable($1) } 
    | TVoS                           { fun sigma -> let v = $1 in
  						    if Languages.Alphabet.mem v sigma then Letter(v) else Variable(v) }
    | TNUM                           { fun _ -> word_of_number $1 }
    | newexpr newexpr                { fun sigma -> Comp($1 sigma, $2 sigma) }
    | newexpr TPOW nexpr             { fun sigma -> Repeat($1 sigma, $3 sigma) }
    | TLPAREN wexpr TRPAREN          { $2 }
;

langexpr: lang TEOF                  { $1 }
;
lang:
      TVoS                                { fun _ -> LVar($1) }  
    | TLBRACE wexpr TBAR preds TRBRACE    { fun sigma -> PatLang($2 sigma, $4 sigma) }
    | lang TPLUS lang                     { fun sigma -> Union($1 sigma, $3 sigma) }
/* TODO: allow parsing of more coplex language expressions */
;

predicate: pred TEOF                 { $1 }
;
preds:
                                     { fun _ -> True }
    | nepreds                        { $1 }
;
nepreds:
      pred                           { $1 }
    | pred TCOMMA preds              { fun sigma -> conj ($1 sigma) ($3 sigma) }
;
pred:
      nexpr TPRIME                   { fun sigma -> prime ($1 sigma) }
    | nexpr TGE nexpr                { fun sigma -> ge ($1 sigma) ($3 sigma) }
    | nexpr TGT nexpr                { fun sigma -> gt ($1 sigma) ($3 sigma) }
    | nexpr TLE nexpr                { fun sigma -> le ($1 sigma) ($3 sigma) }
    | nexpr TLT nexpr                { fun sigma -> lt ($1 sigma) ($3 sigma) }
    | nexpr TEQ nexpr                { fun sigma -> eq ($1 sigma) ($3 sigma) }
    | nexpr TNE nexpr                { fun sigma -> ne ($1 sigma) ($3 sigma) }
    | wexpr TCOLON lang              { fun sigma -> element ($1 sigma) ($3 sigma) }
    | wexpr TEQ wexpr                { fun sigma -> equal ($1 sigma) ($3 sigma) }
    | TNEG pred                      { fun sigma -> bneg ($2 sigma) }
    | TLPAREN pred TRPAREN           { fun sigma -> $2 sigma }
    | pred TAMP pred                 { fun sigma -> conj ($1 sigma) ($3 sigma) }
    | pred TBAR pred                 { fun sigma -> disj ($1 sigma) ($3 sigma) }
;

alphabet: alph TEOF                  { $1 }
;
alph: TLBRACE TRBRACE                { Languages.Alphabet.empty }
    | TLBRACE symbols TRBRACE        { Languages.Alphabet.of_list $2 }
;
symbols:
      TVoS                           { [$1] }
    | TVoS TCOMMA symbols            { $1::$3 }
;
