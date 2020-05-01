module Alphabet = Set.Make(String);;

type alphabet = Alphabet.t
              
type assoc = Assoc
           | NonAssoc
           | RightAssoc
           | LeftAssoc

let show_assoc = function Assoc -> "assoc"
                        | NonAssoc -> "nonassoc"
                        | RightAssoc -> "rassoc"
                        | LeftAssoc -> "lassoc"

type formtype = Nat | Word | Lang | Pred

type case = Nom | Acc | Dat
type det = Det | UnDet
type gen = Fem | Neutr
                        
let show_formaltype d c t = let (s,g) = match t with Nat -> ("Zahl",Fem) | Word -> ("Wort",Neutr) | Lang -> ("Sprache",Fem) | Pred -> ("Prädikat",Neutr) in
                            let a = match (d,c,g) with
                                (Det,Acc,Fem)     | (Det,Nom,Fem)     -> "die"
                              | (Det,Acc,Neutr)   | (Det,Nom,Neutr)   -> "das"
                              | (Det,Dat,Fem)                         -> "der"
                              | (Det,Dat,Neutr)                       -> "dem"
                              | (UnDet,Acc,Fem)   | (UnDet,Nom,Fem)   -> "eine"
                              | (UnDet,Acc,Neutr) | (UnDet,Nom,Neutr) -> "ein"
                              | (UnDet,Dat,Fem)                       -> "einer"
                              | (UnDet,Dat,Neutr)                     -> "einem"
                            in
                            a ^ " " ^ s
                            

(* expressions for type Nat *)
type natexpr = Const of int
             | Var of string
             | BinOp of (string -> string -> string) * assoc * int * (int -> int -> int) * natexpr * natexpr
             | UnOp of (string -> string) * (int -> int) * natexpr
             | WordOp of (string -> string) * (string list -> int) * wordexpr
             | Cases of (predicate * natexpr) list * natexpr
(* expressions for type Word *)
and wordexpr = Epsilon
             | Letter of string
             | Variable of string
             | Comp of wordexpr * wordexpr
             | Repeat of wordexpr * natexpr
(* predicates over Nats and Words *)
and predicate = True
               | NatNatPred of (string -> string -> string) * (int -> int -> bool) * natexpr * natexpr
               | NatPred of (string -> string) * (int -> bool) * natexpr
               | WordWordPred of (string -> string -> string) * (string list -> string list -> bool) * wordexpr * wordexpr
               | WordLangPred of (string -> string -> string) * (string list -> (string list -> bool) -> bool) * wordexpr * langexpr
               | BinJunct of (string -> string -> string) * assoc * int * (bool -> bool -> bool) * predicate * predicate
               | UnJunct of (string -> string) * (bool -> bool) * predicate
(* expressions for languages *)
and langexpr = PatLang of wordexpr * predicate
             | LVar of string
             | Union of langexpr * langexpr
             | ISect of langexpr * langexpr
             | Concat of langexpr * langexpr
             | Star of langexpr

type expression = SomeNatExpr of natexpr
                | NoNatExpr
                | SomeWordExpr of wordexpr
                | NoWordExpr
                | SomeLangExpr of langexpr
                | NoLangExpr
                | SomePredicate of predicate
                | NoPredicate

let language l = (Lang, l)
let number n = (Nat, n)
let word w = (Word, w)

let plus e1 e2  = BinOp((fun s1 -> fun s2 -> s1 ^ "+" ^ s2), Assoc, -2, (+), e1, e2)
let times e1 e2 = BinOp((fun s1 -> fun s2 -> s1 ^ "*" ^ s2), Assoc, -1, (fun x y -> x*y), e1, e2)
let tothe e1 e2 = let rec exp b = function 0 -> 1
                                         | n -> b * (exp b n)
                  in
                  BinOp((fun s1 -> fun s2 -> s1 ^ "^" ^ s2), RightAssoc, -1, exp, e1, e2)
let neg e       = UnOp((fun s -> "-" ^ s), (fun x -> -x), e)
let length w    = WordOp((fun s -> "|" ^ s ^ "|"), List.length, w)
             
             
module VarRanges = Map.Make(String) ;;


let ge e1 e2 = NatNatPred((fun s1 -> fun s2 -> s1 ^ " >= " ^ s2), (>=), e1, e2)
let gt e1 e2 = NatNatPred((fun s1 -> fun s2 -> s1 ^ " > " ^ s2), (>), e1, e2)
let le e1 e2 = NatNatPred((fun s1 -> fun s2 -> s1 ^ " <= " ^ s2), (<=), e1, e2)
let lt e1 e2 = NatNatPred((fun s1 -> fun s2 -> s1 ^ " < " ^ s2), (<), e1, e2)
let eq e1 e2 = NatNatPred((fun s1 -> fun s2 -> s1 ^ " = " ^ s2), (=), e1, e2)
let ne e1 e2 = NatNatPred((fun s1 -> fun s2 -> s1 ^ " <> " ^ s2), (<>), e1, e2)

let prime e =
  let prime =
    let rec checkZero x d = (d * d > x) || (x mod d <> 0) && checkZero x (d+2) in
    function 0 | 1 -> false
           | 2 | 3 -> true
           | n -> (n mod 2 <> 0) && checkZero n 3
  in
  NatPred((fun s -> s ^ " prim"), prime, e)

let element w l = WordLangPred((fun s1 -> fun s2 -> s1 ^ ":" ^ s2), (fun w -> fun l -> l w), w, l)
let equal w1 w2 = WordWordPred((fun s1 -> fun s2 -> s1 ^ " = " ^ s2), (=), w1, w2)
let prefix w1 w2 = WordWordPred((fun s1 -> fun s2 -> s1 ^ " is prefix of " ^ s2),
                                (fun s1 -> fun s2 -> let l1 = String.concat "" s1 in
                                                     let l2 = String.concat "" s2 in
                                                     let l = String.length l1 in
                                                     l <= String.length l2 && l1 = String.sub l2 0 l),
                                w1, w2)
let suffix w1 w2 = WordWordPred((fun s1 -> fun s2 -> s1 ^ " is suffix of " ^ s2),
                                     (fun s1 -> fun s2 -> let w1 = String.concat "" s1 in
                                                          let w2 = String.concat "" s2 in
                                                          let l1 = String.length w1 in
                                                          let l2 = String.length w2 in
                                                          l1 <= l2 && w1 = String.sub w2 (l2-l1) l1),
                                     w1, w2)
  
let conj p1 p2 = BinJunct((fun s1 -> fun s2 -> s1 ^ " & " ^ s2), Assoc, -1, (&&), p1, p2)
let disj p1 p2 = BinJunct((fun s1 -> fun s2 -> s1 ^ " | " ^ s2), Assoc, -2, (||), p1, p2)
let bneg p     = UnJunct((fun s -> "-" ^ s), (not), p)
          
                       
let expr_precedence = function BinOp(_,_,p,_,_,_) -> p
                             | _ -> 0
let pred_precedence = function BinJunct(_,_,p,_,_,_) -> p
                             | _ -> 0


let rec show_natexpr = function Const(c) -> string_of_int c
                              | Var(v) -> v
                              | BinOp(s,a,p,_,e1,e2) -> s (let (lp,rp) = if expr_precedence e1 < p then ("(",")") else ("","") in lp ^ show_natexpr e1 ^ rp)
                                                          (let (lp,rp) = if expr_precedence e2 < p then ("(",")") else ("","") in lp ^ show_natexpr e2 ^ rp)
                              | UnOp(s,_,e) -> let (lp,rp) = if expr_precedence e < 0 then ("(",")") else ("","") in
                                               s (lp ^ show_natexpr e ^ rp)
                              | WordOp(s,_,w) -> s (show_wordexpr w)
                              | Cases(cs,d) -> "IF " ^ String.concat " ELIF " (List.map (fun (p,e) -> show_predicate p ^ " THEN " ^ show_natexpr e) cs) ^ " ELSE " ^ show_natexpr d
and show_wordexpr = function Epsilon -> ""
                           | Letter(c) -> c
                           | Variable(x) -> x
                           | Comp(p1,p2) -> show_wordexpr p1 ^ show_wordexpr p2
                           | Repeat(p,e) -> let sp = show_wordexpr p in
                                            let (lp,rp) = if String.length sp > 1 then ("(",")") else ("","") in
                                            lp ^ sp ^ rp ^ "^" ^
                                              let se = show_natexpr e in
                                              let (lp,rp) = if String.length se > 1 then ("(",")") else ("","") in
                                              lp ^ se ^ rp
and show_predicate = function True -> "true"   (* TODO: respect associativity when printing parentheses *)
                            | NatNatPred(s,_,e1,e2) -> s (show_natexpr e1) (show_natexpr e2)
                            | NatPred(s,_,e) -> s (show_natexpr e)
                            | WordWordPred(s,_,e1,e2) -> s (show_wordexpr e1) (show_wordexpr e2)
                            | WordLangPred(s,_,e1,e2) -> s (show_wordexpr e1) (show_langexpr e2)
                            | BinJunct(s,_,p,_,p1,p2) -> s (let (lp,rp) = if pred_precedence p1 < p then ("(",")") else ("","") in lp ^ show_predicate p1 ^ rp)
                                                           (let (lp,rp) = if pred_precedence p2 < p then ("(",")") else ("","") in lp ^ show_predicate p2 ^ rp)
                            | UnJunct(s,_,p) -> let (lp,rp) = if pred_precedence p < 0 then ("(",")") else ("","") in
                                                s (lp ^ show_predicate p ^ rp)
and show_langexpr = function PatLang(w,p)  -> "{ " ^ show_wordexpr w ^ " | " ^ show_predicate p ^ " }"
                           | LVar(v)       -> v
                           | Union(l1,l2)  -> show_langexpr l1 ^ " + " ^ show_langexpr l2
                           | ISect(l1,l2)  -> show_langexpr l1 ^ " \239 " ^ show_langexpr l2
                           | Concat(l1,l2) -> show_langexpr l1 ^ " \249 " ^ show_langexpr l2
                           | Star(l)       -> "(" ^ show_langexpr l ^ ")*"
                                           
let show_expr = function SomeNatExpr(e)   -> show_natexpr e
                       | SomeWordExpr(e)  -> show_wordexpr e
                       | SomeLangExpr(e)  -> show_langexpr e
                       | SomePredicate(e) -> show_predicate e
                       | _                -> ""
                                          
                                             
let string_of_char = String.make 1

                   (*
let rec show_pattern = function Epsilon -> "Epsilon"
                                | Letter(x) -> "Letter(" ^ x ^ ")"
                                | Variable(x) -> "Variable(" ^ x ^ ")"
                                | Comp(p1,p2) -> "Comp(" ^ show_pattern p1 ^ "," ^ show_pattern p2 ^ ")"
                                | Star(p) -> "Star(" ^ show_pattern p ^ ")"
                                | Repeat(p,e) -> "Repeat(" ^ show_pattern p ^ "," ^ show_expr e ^ ")"
                    *)

                                                 

let show_alphabet sigma = "{" ^ String.concat "," (Alphabet.elements sigma) ^ "}"


let separate sigma s =
  let suffix s = String.sub s 1 ((String.length s)-1) in
  let rec chop_off_letter s t = if String.length t = 0 then
                                  Some s
                                else if String.length s = 0 || String.get s 0 <> String.get t 0 then
                                  None
                                else
                                  chop_off_letter (suffix s) (suffix t)
  in                  
  let rec chop v aux = function "" -> List.rev (if v="" then aux else (Variable(v)::aux))
                              | s  -> let found_letter = Alphabet.fold (fun a -> fun found -> match found with
                                                                                                Some _ -> found
                                                                                              | _ -> match chop_off_letter s a with
                                                                                                       Some t -> Some (a,t)
                                                                                                     | None -> None)
                                                           sigma None
                                      in
                                      match found_letter with
                                        Some (a,t) -> chop "" (Letter(a)::(if v="" then aux else Variable(v)::aux)) t
                                      | None -> chop (v ^ (String.sub s 0 1)) aux (suffix s)  
  in
  chop "" [] s
