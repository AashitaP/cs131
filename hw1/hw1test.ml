let subset_test0 = subset [] [1;2;3]
let subset_test1 = subset [3;1;3] [1;2;3]
let subset_test2 = not (subset [1;3;7] [4;1;3])
let subset_test3 = subset [] []
let subset_test4 = not (subset [1;2] [])
let subset_test5 = subset [1;1;1;1;1;1;1;1;1] [1]
let subset_test6 = not (subset [1] [2])


let equal_sets_test0 = equal_sets [1;3] [3;1;3]
let equal_sets_test1 = not (equal_sets [1;3;4] [3;1;3])
let equal_sets_test2 = equal_sets [] []
let equal_sets_test3 = not (equal_sets [] [1])
let equal_sets_test4 = not (equal_sets [1] [])
let equal_sets_test5 = equal_sets [1;1;1;1;1;1] [1]
let equal_sets_test6 = equal_sets [1] [1;1;1;1;1;1]
let equal_sets_test1 = equal_sets [5;6;7;8;9;8;7;6;5] [9;9;8;8;7;7;6;6;5;5]


let set_union_test0 = equal_sets (set_union [] [1;2;3]) [1;2;3]
let set_union_test1 = equal_sets (set_union [3;1;3] [1;2;3]) [1;2;3]
let set_union_test2 = equal_sets (set_union [] []) []
let set_union_test3 = equal_sets (set_union [1;2;3;4] []) [1;2;3;4]
let set_union_test4 = equal_sets (set_union [1;1;1;1;1;1;1] []) [1]
let set_union_test5 = equal_sets (set_union [1;1;1;1;1;1;1] [2;2;2;2;3;4;5;6;7]) [1;2;3;4;5;6;7]
let set_union_test6 = equal_sets (set_union [1;1;1;1;1;1;1] [2;2;2;2;3;4;5;6;7]) [7;6;5;4;3;2;1]


let set_intersection_test0 =
  equal_sets (set_intersection [] [1;2;3]) []
let set_intersection_test1 =
  equal_sets (set_intersection [3;1;3] [1;2;3]) [1;3]
let set_intersection_test2 =
  equal_sets (set_intersection [1;2;3;4] [3;1;2;4]) [4;3;2;1]
let set_intersection_test3 =
  equal_sets (set_intersection [1;2;3] []) []
let set_intersection_test4 =
  equal_sets (set_intersection [1;2;3] [2;2;2;2;2;2;2]) [2]
let set_intersection_test5 =
  equal_sets (set_intersection [] []) []


let set_diff_test0 = equal_sets (set_diff [1;3] [1;4;3;1]) []
let set_diff_test1 = equal_sets (set_diff [4;3;1;1;3] [1;3]) [4]
let set_diff_test2 = equal_sets (set_diff [4;3;1] []) [1;3;4]
let set_diff_test3 = equal_sets (set_diff [] [4;3;1]) []
let set_diff_test4 = equal_sets (set_diff [] []) []
let set_diff_test5 = equal_sets (set_diff [1;1;1;1;1;1;1] []) [1]
let set_diff_test6 = equal_sets (set_diff [2;2;2;2;2;2] [2]) []
let set_diff_test7 = equal_sets (set_diff [2] [2;2;2;2;2;2]) []

let computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x / 2) 1000000000 = 0
let computed_fixed_point_test1 =
  computed_fixed_point (=) (fun x -> x *. 2.) 1. = infinity
let computed_fixed_point_test2 =
  computed_fixed_point (=) sqrt 10. = 1.
let computed_fixed_point_test3 =
  ((computed_fixed_point (fun x y -> abs_float (x -. y) < 1.)
			 (fun x -> x /. 2.)
			 10.)
   = 1.25)
let computed_fixed_point_test4 =
  computed_fixed_point (=) (fun x -> x) 1 = 1
let computed_fixed_point_test5 =
  computed_fixed_point (=) (fun x -> x *. x) 2. = infinity
let computed_fixed_point_test6 =
  computed_fixed_point (=) (fun x -> x *. x) 0.25 = 0.

(* An example grammar for a small subset of Awk.  *)

type awksub_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num

let awksub_rules =
   [Expr, [T"("; N Expr; T")"];
    Expr, [N Num];
    Expr, [N Expr; N Binop; N Expr];
    Expr, [N Lvalue];
    Expr, [N Incrop; N Lvalue];
    Expr, [N Lvalue; N Incrop];
    Lvalue, [T"$"; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
    Num, [T"0"];
    Num, [T"1"];
    Num, [T"2"];
    Num, [T"3"];
    Num, [T"4"];
    Num, [T"5"];
    Num, [T"6"];
    Num, [T"7"];
    Num, [T"8"];
    Num, [T"9"]]

let awksub_grammar = Expr, awksub_rules

let awksub_test0 =
  filter_reachable awksub_grammar = awksub_grammar

let awksub_test1 =
  filter_reachable (Expr, List.tl awksub_rules) = (Expr, List.tl awksub_rules)

let awksub_test2 =
  filter_reachable (Lvalue, awksub_rules) = (Lvalue, awksub_rules)

let awksub_test3 =
  filter_reachable (Expr, List.tl (List.tl awksub_rules)) =
    (Expr,
     [Expr, [N Expr; N Binop; N Expr];
      Expr, [N Lvalue];
      Expr, [N Incrop; N Lvalue];
      Expr, [N Lvalue; N Incrop];
      Lvalue, [T "$"; N Expr];
      Incrop, [T "++"];
      Incrop, [T "--"];
      Binop, [T "+"];
      Binop, [T "-"]])

let awksub_test4 =
  filter_reachable (Expr, List.tl (List.tl (List.tl awksub_rules))) =
    (Expr,
     [Expr, [N Lvalue];
      Expr, [N Incrop; N Lvalue];
      Expr, [N Lvalue; N Incrop];
      Lvalue, [T "$"; N Expr];
      Incrop, [T "++"];
      Incrop, [T "--"]])

let awksub_test5 = 
  filter_reachable (Expr,List.tl (List.tl awksub_rules)) =  
      (Expr, 
        [Expr, [N Expr; N Binop; N Expr];
        Expr, [N Lvalue];
        Expr, [N Incrop; N Lvalue];
        Expr, [N Lvalue; N Incrop];
        Lvalue, [T"$"; N Expr];
        Incrop, [T"++"];
        Incrop, [T"--"];
        Binop, [T"+"];
        Binop, [T"-"]])

type giant_nonterminals =
  | Conversation | Sentence | Grunt | Snore | Shout | Quiet | Scream

let giant_grammar =
  Conversation,
  [Snore, [T"ZZZ"];
   Quiet, [];
   Grunt, [T"khrgh"];
   Shout, [T"aooogah!"];
   Sentence, [N Quiet];
   Sentence, [N Grunt];
   Sentence, [N Shout];
   Conversation, [N Snore];
   Conversation, [N Sentence; T","; N Conversation]]

let giant_test0 =
  filter_reachable giant_grammar = giant_grammar

let giant_test1 =
  filter_reachable (Sentence, List.tl (snd giant_grammar)) =
    (Sentence,
     [Quiet, []; Grunt, [T "khrgh"]; Shout, [T "aooogah!"];
      Sentence, [N Quiet]; Sentence, [N Grunt]; Sentence, [N Shout]])

let giant_test2 =
  filter_reachable (Quiet, snd giant_grammar) = (Quiet, [Quiet, []])

let giant_test3 = 
  filter_reachable (Scream, (snd giant_grammar)) = (Scream, []) (*start does not exist*)

let giant_test4 =
  filter_reachable (Grunt, snd giant_grammar) = (Grunt, [Grunt, [T"khrgh"]])

let giant_test5 =
  filter_reachable (Grunt, [Shout, [T"aooogah!"];
  Sentence, [N Quiet];
  Sentence, [N Grunt];
  Sentence, [N Shout];
  Conversation, [N Snore];
  Conversation, [N Sentence; T","; N Conversation]]) = (Grunt, [])
  
let dwarf_grammar =
    Conversation,
    [] (*grammar with no rules*)
let dwarf_test0 = filter_reachable dwarf_grammar = dwarf_grammar