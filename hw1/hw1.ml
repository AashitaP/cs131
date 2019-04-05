(*function to check if element is in list*)

let rec contains a lis = 
  match lis with 
  [] -> false
  | head :: rst -> if a = head then true else contains a rst;;

(*check if every element of a is in b - so create a helper function and apply that function to every element of list a*)
let rec subset a b = 
  match a with 
  [] -> true (*empty set is subset of all sets*)
  | head::rst -> if contains head b then subset rst b else false;;

(*to check is represented sets are equal, a is a subset of b and b is a subset of a*)
let equal_sets a b = 
  subset a b && subset b a;;

(* idea is to concatenate each element of a into b if it is not already in it -- recursively *)
let rec set_union a b = 
  match a with 
  [] -> b
  | head::rst -> if contains head b then set_union rst b else set_union rst (head::b);;

(*each element of a compared with b, if not in it then continue if in it concatenate *)

let rec set_intersection a b = 
  match a with 
  [] -> []
  | head::rst -> if contains head b then head::set_intersection rst b else set_intersection rst b;;

(*go through each element of a, if it is in b check rest against b else concatenate with check rest against b - like opposite of set_intersection*)

let rec set_diff a b = 
  match a with 
  [] -> []
  | head::rst -> if contains head b then set_diff rst b else head::set_diff rst b;;

(*simple recursion*)

let rec computed_fixed_point eq f x = 
  if eq (f x) x then x else computed_fixed_point eq f (f x);;


type ('nonterminal, 'terminal) symbol = 
  | N of 'nonterminal
  | T of 'terminal

(*check against one element of the list to see if it matches the symbol*)
let exist_non_term_symbol symb rule_rhs = 
  match rule_rhs with 
  T _ -> false 
  | N a -> if symb = a then true else false;;

(*for each element in the right hand side list, check*)
let rec exist_non_term_symbol_list symb rules_rhs = 
  match rules_rhs with
  [] -> false
  | rule::rest -> if exist_non_term_symbol symb rule then true else exist_non_term_symbol_list symb rest;;


(*for entire list of rules, split into each row and split row into lhs and rhs rule*)

let rec check_contains_symbol symb rules = 
  match rules with 
  [] -> false
  | head::rst -> let top = (head) in 
      if (exist_non_term_symbol_list symb (snd top)) then true else (check_contains_symbol symb rst);;

(* go through entire list of rules and check if there are any matches with the start symbol (top) or with the rhs of the list of rules already added *)
let rec create_rules_helper list_rules return_list top = 
  match list_rules with 
  [] -> return_list
  | head::rst -> if ((check_contains_symbol (fst head) return_list) || (fst head) = top) 
  then (create_rules_helper rst (head::return_list) top) else (create_rules_helper rst return_list top);; 

 (* keep calling helper function until the return list no longer expands i.e. every time return list includes an additional rule *) 
let create_rules list_rules top = 
  let rules_parse_first = create_rules_helper list_rules [] top in 
    computed_fixed_point (equal_sets) (fun x -> (create_rules_helper list_rules x top)) rules_parse_first


(*filter to mantain order *)

let rec filter_reachable_helper list_rules top = 
  let create_rules_var = (create_rules list_rules top) in
    List.filter (fun x -> List.mem x (create_rules_var)) (list_rules) 

(*final return format*)
let rec filter_reachable g = 
  (fst g, filter_reachable_helper (snd g) (fst g));;