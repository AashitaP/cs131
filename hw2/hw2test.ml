type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal
 
type ('nonterminal, 'terminal) symbol =
 | N of 'nonterminal
 | T of 'terminal
   
 (*test cases for make_matcher.*)

let accept_all string = Some string
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x

let accept_i_suffix = function 
  | "I"::x -> Some x
  | a -> None

let accept_love_suffix = function
  | "!"::x -> Some x
  | a -> None

let accept_sushi_suffix = function
  | ["Sushi"] -> Some ["Sushi"]
  | a -> None


type grammar_nonterminals =
  | Pizza | Ramen | Rice | Taco | Pho

let food_grammar =
  (Pizza,
   function
     | Pizza ->
         [[N Ramen; N Rice; N Taco];
          [N Pho];
          [T"Pizza"]]
     | Pho ->
	 [[N Ramen];
	  [N Rice];
	  [N Rice; N Ramen];
    [T"I"; N Rice; N Taco];
	  [T"Pho"]]
     | Rice ->
   [[N Taco];
   [T"Love"]]
     | Taco ->
	 [[N Ramen]]
     | Ramen ->
   [[T"Ramen"];
    [T"!"]])

  let make_matcher_test = ((make_matcher food_grammar accept_all ["Sushi"]) = None) (* None of the prefix matches so the matcher returns None regardless of acceptors - next few cases*)
  let make_matcher_test1 = ((make_matcher food_grammar accept_sushi_suffix ["Sushi"]) = None) 
  let make_matcher_test2 = ((make_matcher food_grammar accept_empty_suffix ["Sushi"]) = None) 
  let make_matcher_test3 = ((make_matcher food_grammar accept_all ["!"]) = Some [])
  let make_matcher_test4 = ((make_matcher food_grammar accept_all ["I"; "Love"; "Ramen"]) = Some [])
  let make_matcher_test5 = ((make_matcher food_grammar accept_i_suffix ["I"; "Love"; "Ramen"]) = None) (*empty fragment is passed into acceptor*)
  let make_matcher_test6 = ((make_matcher food_grammar accept_empty_suffix ["I"; "Love"; "Ramen"]) = Some []) (*empty fragment, all matches*)
  let make_matcher_test7 = ((make_matcher food_grammar accept_all ["I"; "Love"; "Ramen"]) = Some []) (*empty fragment, all matches*)
  let make_matcher_test8 = ((make_matcher food_grammar accept_all ["Love"; "!"]) = Some ["!"]) (*checks that grammar is evaluated in order*)
  let make_matcher_test9 = ((make_matcher food_grammar accept_love_suffix ["Love"; "!"]) = Some []) (*checks that grammar is evaluated in order*)


(*test cases for make_parser.*)

let make_parser_test = ((make_parser food_grammar ["Sushi"]) = None) (*no tree for sushi*)
let make_parser_test1 = (*following test cases check that make_parser & parse_tree_leaves are reverse*)
    match make_parser food_grammar ["!"] with
      | Some tree -> parse_tree_leaves tree = ["!"]
      | _ -> false
let make_parser_test2 = 
    match make_parser food_grammar ["Love"; "!"] with
      | Some tree -> parse_tree_leaves tree = ["Love"; "!"]
      | _ -> false
let make_parser_test3 = 
    match make_parser food_grammar ["I"; "Love"; "Ramen"] with
      | Some tree -> parse_tree_leaves tree = ["I"; "Love"; "Ramen"]
      | _ -> false

let make_parser_test4= ((make_parser food_grammar []) = None) (*empty should return None*)
let make_parser_test5= ((make_parser food_grammar ["I"; "Love"]) = None) (*no tree*)

  
  