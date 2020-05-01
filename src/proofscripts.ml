open Languages;;

let get_parser = function Nat -> (fun t l a -> SomeNatExpr(Parser.natexpr t l a))
			| Word -> (fun t l a -> SomeWordExpr(Parser.wordexpr t l a))
			| Lang -> (fun t l a -> SomeLangExpr(Parser.langexpr t l a))
                        | Pred -> (fun t l a -> SomePredicate(Parser.predicate t l a))
                                
type proofStep =
  ShowProof
| State of string
| Bind of (formtype * string) * string
| Introduce of (formtype * string) list
| Specify of (formtype * string) list * (string list)
| Assume of string list
| Check of string list
| ClearOutput

let output_tmp s = print_string s
let proof = ref ""
let output s = print_string ("\n" ^ s);
               proof := !proof ^ s
           
module VariableMap = Map.Make(String);;

type state = { mutable alphabet: Languages.alphabet;
               mutable vars: expression VariableMap.t }

let state = ref { alphabet = Languages.Alphabet.empty;
                  vars     = VariableMap.empty }

let add_binding key value = !state.vars <- VariableMap.add key value !state.vars
          
let run_proof_script script =
  let bind ft v es =
    let parser = get_parser ft in
    try
      let lexbuf = Lexing.from_string es in
      let e = parser Lexer.token lexbuf !state.alphabet in
      add_binding v e; 
      output ("Es gilt " ^ v ^ " = " ^ show_expr e ^ ". ")
    with Failure _ -> failwith ("run_proof_script.bind: cannot read " ^ (match ft with Nat -> "Nat" | Word -> "Word" | Lang -> "Lang" | Pred -> "Pred") ^ " expression from string `" ^ es ^ "'")
  in

  let introduce is  =
    List.iter (fun (ft,v) -> add_binding v (match ft with
                                              Nat -> NoNatExpr
                                            | Word -> NoWordExpr
                                            | Lang -> NoLangExpr
                                            | Pred -> failwith "Proofscript.run_proof_script: binding of predicate variables not supported!"))
      is;
    let s = if List.length is = 1 then "" else "en" in
    output ("Sei" ^ s ^ " " ^ String.concat ", " (List.map (fun (ft,v) -> show_formaltype UnDet Nom ft ^ " " ^ v) is) ^ " gegeben. ")
  in

  let unbounds _ = VariableMap.fold (fun v e l -> match e with NoNatExpr       -> (Nat,v)::l
                                                             | NoWordExpr      -> (Word,v)::l
                                                             | NoLangExpr      -> (Lang,v)::l
                                                             | _               -> l)
                     !state.vars []
  in
  
  let specify exists conds =
    output_tmp ("\nGesucht ist " ^
                  (match exists with 
                    []       -> failwith "Proofscript.run_proof_script: nothing here to specify!" 
                  | [(ft,v)] -> "ein Ausdruck für " ^ show_formaltype UnDet Acc ft ^ " " ^ v
                  | _        -> "jeweils einen Ausdruck für " ^ String.concat ", " (List.map (fun (ft,v) -> show_formaltype UnDet Acc ft ^ " " ^ v) exists)) ^
                    (match conds with
                       []  -> ". "
                     | [c] -> ", so dass " ^ show_predicate c ^ " gilt. "
                     | _   -> ", so dass " ^ String.concat ", " (List.map show_predicate conds) ^ " gelten. ") ^
                      let ubs = unbounds () in
                      match exists with
                        [_]  -> "Dieser kann von " ^ String.concat ", " (List.map (fun (ft,v) -> show_formaltype Det Dat ft ^ " " ^ v) ubs) ^ " abhängen."
                      | _::_ -> "Diese können von " ^ String.concat ", " (List.map (fun (ft,v) -> show_formaltype Det Dat ft ^ " " ^ v) ubs) ^ " abhängen."
                      | _    -> "");

    let found = ref false in

    List.iter (fun (ft,v) -> found := false;
                             let prompt = "\nGeben Sie jetzt einen Ausdruck für " ^ show_formaltype Det Acc ft ^ " " ^ v ^ " an: " in
                             let parser = get_parser ft in
                             while not !found do
                               try
                                 output_tmp prompt;
                                 let e = read_line () in
                                 let lexbuf = Lexing.from_string e in
                                 let e = parser Lexer.token lexbuf !state.alphabet in
                                 add_binding v e; 
                                 output ("Wir wählen " ^ show_formaltype Det Acc ft ^ " " ^ v ^ " := " ^ show_expr e ^ ". ");
                                 found := true
                               with _ -> output_tmp "<<< Cannot read valid expression from this input. Try again! >>>"
                             done;
              ) exists
  in

  let assume preds = let l = List.length preds in
                     if l > 0 then
                       begin
                         let h = if l = 1 then "gilt" else "gelten" in
                         output ("Angenommen, es " ^ h ^ " " ^ String.concat ", " (List.map show_predicate preds) ^ ". ")
                       end
  in
  
  let check pred = output ("Es gilt " ^ show_predicate pred ^ ", denn ... ");
                   output_tmp ("<<< Fügen Sie hier manuell einen Beweis dieser Aussage ein! >>> ")
  in

  let read_predicates preds =
    let parser = get_parser Pred in
    List.map (fun c -> let lexbuf = Lexing.from_string c in
                       match parser Lexer.token lexbuf !state.alphabet with
                         SomePredicate p -> p
                       | _ -> failwith ("Proofscript.run_proof_script: cannot parse predicate from `" ^ c ^ "´!"))
      preds
  in
                                     
  let run = function State(s) -> output s
                   | ShowProof -> output_tmp ("\n\nNochmal die Beweisskizze von Anfang bis hierher:\n\n" ^ !proof ^ "QED\n")
                   | Bind((ft,v),o) -> bind ft v o 
                   | Introduce univs -> introduce univs
                   | Specify(exists,conds) -> let cs = read_predicates conds in
                                              specify exists cs;
                                              List.iter check cs
                   | Assume preds -> let ps = read_predicates preds in
                                     assume ps
                   | Check preds -> let ps = read_predicates preds in
                                    List.iter check ps
                   | ClearOutput -> proof := ""
  in
  List.map run script

let set_alphabet s =
  try
    let lexbuf = Lexing.from_string s in
    let sigma = Parser.alphabet Lexer.token lexbuf in  
    !state.alphabet <- sigma;
    output ("Das Alphabet ist " ^ show_alphabet sigma ^ ". ")
  with _ -> failwith ("Proofscript.set_alphabet: cannot parse alphabet from `" ^ s ^ "´!")

let prove_nonregular l = run_proof_script
                           [ Bind(language "L", l);
                             ClearOutput;
                             State("Beweis durch Widerspruch. Angenommen, L sei regulär. ");
                             Introduce [number "n"];
                             Specify ([word "w"], ["|w| >= n"; "w: L"]);
                             Introduce [word "x"; word "y"; word "z"];                        
                             Assume ["xyz = w"; "|y| >= 1"; "|xy| <= n"];
                             Specify ([number "i"], ["-(xy^iz: L)"]);
                             State("Widerspruch zur Vorhersage des Pumping-Lemmas. Damit kann L nicht regulär sein. ");
                             ShowProof
                           ]

