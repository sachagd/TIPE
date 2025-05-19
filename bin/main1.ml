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

let hash_loop h n var (loop_min, loop_max) =
  if Hashtbl.mem h var then 
    begin
    let (old_min, old_max) = Hashtbl.find h var in 
    Hashtbl.replace h var (old_min + n * (loop_min - old_min), old_max + n * (loop_max - old_max));
    end
  else
    Hashtbl.add h var (loop_min, loop_max);
  ()

let get_var_name dec = 
  match dec with
  |InitDeclaration(name, _) -> name
  |_ -> failwith "c'est suffisant" 

let hash_if h var (if_min, if_max) = 
  if Hashtbl.mem h var then
    begin
    let (old_min, old_max) = Hashtbl.find h var in
    Hashtbl.replace h var (min old_min if_min, max old_max if_max)
    end
  else
    Hashtbl.add h var (if_min, if_max);
  ()
  
let rec bound_expr h expr = 
  match expr with
  |Num(i) -> i,i 
  |Var(var) |Array(var) -> Hashtbl.find h var
  |BinOpE(op, expr1, expr2) ->
    let min1, max1 = bound_expr h expr1 
    and min2, max2 = bound_expr h expr2 in
    (match op with
    |Add -> (min1 + min2, max1 + max2)
    |Sub -> (min1 - max2, max2 - min1)
    |Mul -> (min1 * min2, max1 * max2)
    |Div -> (min1 / max2, max2 / min1)
    |Mod -> failwith "error operator, utilise pas mod sale fou")
  |_ -> failwith "error expression, je sais même pas comment t'as fait"

(* On suppose les arrays remplies de int *)
let rec arraydeclaration l mi ma =
  match l with 
  |[] -> 
    mi, ma
  |Num(e)::tl ->
    arraydeclaration tl (min mi e) (max ma e)
  |_ -> failwith "error on a dit que des ints"
    
let rec stmt h a = 
  match a with
  |InitDeclaration(var, expr) -> 
    Hashtbl.add h var (bound_expr h expr);
  |ArrayDeclaration(var, exprs) -> 
    Hashtbl.add h var (arraydeclaration exprs max_int min_int);
  |Assignment(nom, expr) |ArrayAssign(nom, expr) ->
    let old_min, old_max = Hashtbl.find h nom in
    let expr_min, expr_max = bound_expr h expr in
    Hashtbl.replace h nom (min old_min expr_min, max old_max expr_max)
  |ForLoop(ind, c, _, l) ->    
    let h1 = Hashtbl.copy h in 
    let loop_index_bound = binopc c in 
    let loop_index_name = get_var_name ind in
    Hashtbl.add h1 loop_index_name (0, loop_index_bound);
    List.iter (stmt h1) l;
    Hashtbl.iter (hash_loop h loop_index_bound) h1;
    Hashtbl.clear h1;
  |If(l) ->
    List.iter (stmt h) l; 
  |Return(_) -> ()
  |_ -> failwith "t'as cru tu pouvais coder ce que tu voulais en c"

let func h = function 
  |Function(_, contenu) -> List.iter (stmt h) contenu 

let program h = function
  |Program(l) -> List.iter (func h) l

let print_hashtable var (var_min, var_max) =
  print_string var;
  print_string "  ";
  print_int var_min;
  print_string ", ";
  print_int var_max;
  print_newline ()

let bound_to_type var (var_min, var_max) =
  print_string var;
  print_string " : ";
  if -128 <= var_min && var_max <= 127 then 
    print_string "int_8"
  else 
    if -32768 <= var_min && var_max <= 32767 then 
      print_string "int_16"
    else
      print_string "int_32";
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