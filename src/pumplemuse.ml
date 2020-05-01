open Languages ;;
open Proofscripts ;;


let l1="{ a^n b^m | n >= m }"
let _ = set_alphabet "{a,b}";
        prove_nonregular l1
          

    (*
let _ =
  print_string ("Das Alphabet ist Sigma = " ^ show_alphabet sigma ^ ".\n")
  
(* examples *)

(* { a^n b^m | n > m } *)
(* let l1 = PatLang([('a', Var("n")); ('b', Var("m"))] , gt (Var("n")) (Var("m"))) *)


let l1 =
  let lexbuf = Lexing.from_string "{ a^n b^m | n > m }" in
  Parser.language Lexer.token lexbuf sigma 

let _ =
  print_string ("L1 = " ^ show_language l1 ^ "\n")

    
 
(* { a^n b^n c^n | n >= 1 } *)
(* let l2 = PatLang([('a', Var("n")); ('b', Var("n")); ('c', Var("n"))] , ge (Var("n")) (Const(1))) *)
let l2 =
  let lexbuf = Lexing.from_string "{ a^n b^n c^n | n >= 1 }" in
  Parser.language Lexer.token lexbuf sigma

let _ =
  print_string ("L2 = " ^ show_language l2 ^ "\n")

  
(* { a^n b^m c^k | n < m and m < k } *)
(* let l3 = PatLang([('a', Var("n")); ('b', Var("m")); ('c', Var("k"))] , disj (eq (Var("n")) (Var("m"))) (eq (Var("m")) (Var("k")))) *)
let l3 =
  let lexbuf = Lexing.from_string "{ a^n b^m c^k | n < m, m < k }" in
  Parser.language Lexer.token lexbuf sigma
  
let _ =
  print_string ("L3 = " ^ show_language l3 ^ "\n")

  
(* { a^n b^m c^k | n = m or m = k } *)
(* same language but constructed differently *)
(* let l4 = Union(PatLang([('a', Var("n")); ('b', Var("m")); ('c', Var("_"))] , eq (Var("n")) (Var("m"))),
               PatLang([('a', Var("_")); ('b', Var("m")); ('c', Var("k"))] , eq (Var("m")) (Var("k")))) *)
let l4 =
  let lexbuf = Lexing.from_string "{ a^n b^n c^m | } + { a^n b^m c^m | }" in
  Parser.language Lexer.token lexbuf sigma
           
let _ =
  print_string ("L4 = " ^ show_language l4 ^ "\n")

     *)
      
