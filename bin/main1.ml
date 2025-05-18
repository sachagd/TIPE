open Ast

let print i = 
  print_int i;
  print_newline ()

let binopc op = (*changer pour les autres binopc*)
  match op with 
  |BinOpC(_,_,i) -> 
    match i with
    |Num(i) -> i 
    |_ -> failwith "error binopc c'est pas un int malotru"

let hash_loop h i e (n, t1) =
  if Hashtbl.mem h e then 
    let _, t2 = Hashtbl.find h e in 
    for j = 0 to n do
      let (k1, k2) = t1.(j) in 
      let (k3, k4) = t2.(j) in 
      print k3;
      print k4; 
      print k1;
      print k2;
      t2.(j) <- (k3 + i * (k1 - k3), k4 + i * (k2 - k4))
    done;
  else
    Hashtbl.add h e (n, t1);
  ()

let get_var_name dec = 
  match dec with
  |InitDeclaration(name, _,  _) -> name
  |_ -> failwith "c'est suffisant" 

let hash_if h e (n ,t1) = 
  if Hashtbl.mem h e then
    let _, t2 = Hashtbl.find h e in
    for j = 0 to n do 
      let (k1, k2) = t1.(j) in 
      let (k3, k4) = t2.(j) in 
      t2.(j) <- (min k1 k3, max k2 k4);
    done;
  else
    Hashtbl.add h e (n, t1);
  ()
  
let rec bound_expr h e = 
  match e with
  |Num(i) -> i,i 
  |Var(x) -> 
    let (_, t) = Hashtbl.find h x in 
    t.(0)
  |BinOpE(op,e1,e2) ->
    let b1, b2 = bound_expr h e1 
    and b3, b4 = bound_expr h e2 in
    (match op with
    |Add -> (b1 + b3, b2 + b4)
    |Sub -> (b1 - b4, b2 - b3)
    |Mul -> (b1 * b3, b2 * b4)
    |Div -> (b1 / b4, b2 / b3)
    |Mod -> failwith "error operator, utilise pas mod sale fou")
  (* |Array() *)
  |_ -> failwith "error expression, je sais même pas comment t'as fait"

(* On suppose les arrays remplies de int *)
let rec arraydeclaration t l i n mi ma =
  match l with 
  |[] -> 
    t.(n) <- (mi,ma);
    t
  |hd::tl ->
    match hd with
    |Num(e) ->
      t.(i) <- e,e;
      arraydeclaration t tl (i+1) n (min mi e) (max ma e)
    |_ -> failwith "error on a dit que des ints"
    
let rec stmt h a = 
  match a with
  |InitDeclaration(nom, var_type, expr) -> 
    if var_type = IntType then 
      Hashtbl.replace h nom (0, [|bound_expr h expr|]);
  |ArrayDeclaration(nom, n, _, l) -> 
    let t = Array.make (n + 1) (0, 0) in
    Hashtbl.add h nom (n , arraydeclaration t l 0 n max_int min_int);
    print_int (snd (snd (Hashtbl.find h nom)).(n))
  |Assignment(nom, expr) ->
    Hashtbl.replace h nom (0, [|bound_expr h expr|])
  |ArrayAssign(_,ind,_) ->
    let ind_lbound, ind = bound_expr h ind in 
    ()
  |ForLoop(ind,c,_,l) ->    
    let h1 = Hashtbl.copy h in 
    let loop_index_bound = binopc c in 
    let loop_index_name = get_var_name ind in
    Hashtbl.add h1 loop_index_name (0, [|(0, loop_index_bound)|]);
    List.iter (stmt h1) l;
    Hashtbl.iter (hash_loop h loop_index_bound) h1;
    Hashtbl.clear h1;
  |If(l) ->
    let h1 = Hashtbl.copy h in 
    List.iter (stmt h1) l; 
    Hashtbl.iter (hash_if h) h1
  |Return(_) -> ()
  |_ -> failwith "t'as cru tu pouvais coder ce que tu voulais en c"

let func h = function 
  |Function(_, contenu) -> List.iter (stmt h) contenu 

let program h = function
  |Program(l) -> List.iter (func h) l

let print_hashtable e (n, t) =
  let k1,k2 = t.(n) in 
  print_string e;
  print_string "  ";
  print_int k1;
  print_string ", ";
  print_int k2;
  print_newline ();
  for i = 0 to n - 1 do 
    let k1,k2 = t.(i) in
    print_string "   ";
    print_int i;
    print_string "  ";
    print_int k1;
    print_string ", ";
    print_int k2;
    print_newline ()
  done;
  print_newline ()

let bound_to_type e (n ,t) =
  let (k1, k2) = t.(n) in 
  let s = ref "" in
  if -128 <= k1 && k2 <= 127 then 
    s := "int_8"
  else 
    if -32768 <= k1 && k2 <= 32767 then 
      s := "int_16"
    else
      s := "int_32";
  print_string e;
  print_string " : ";
  print_string !s;
  print_newline ()

let find_bound a = 
  let h = Hashtbl.create 0 in 
  program h a;
  Hashtbl.iter print_hashtable h;
  print_newline();
  Hashtbl.iter bound_to_type h

let () =
  let filename = "main.c" in
  let channel = open_in filename in
  let lexbuf = Lexing.from_channel channel in
  let ast = Parser.program Lexer.tokenize lexbuf in
  find_bound ast