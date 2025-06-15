use "hw2.sml";

(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = all_except_option ("string", ["string"]) = SOME []
val testfoo = all_except_option ("string", ["string", "string2"])

val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val testbar = get_substitutions1 ([[],[]], "foo") = []
val testbar1 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
"Fred")
val testbar2 = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
"Fred")

val test3 = get_substitutions2 ([["foo"],["there"]], "foo") = []

val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test5 = card_color (Clubs, Num 2) = Black
(* val test51 = card_color2 (Clubs, Num 2) = Black *)

val test6 = card_value (Clubs, Num 2) = 2

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
(* val test71 = remove_card2 ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = [] *)
val test72 = remove_card ([(Hearts, Ace), (Spades, Jack), (Hearts, Ace)], (Hearts, Ace), IllegalMove) = [(Spades, Jack), (Hearts, Ace)]
val test73 = remove_card ([(Hearts, Ace), (Spades, Jack), (Hearts, Ace)], (Diamonds, Ace), IllegalMove)
(* val test74 = remove_card2 ([(Spades, Jack), (Hearts, Ace), (Hearts, Ace)], (Hearts, Ace), IllegalMove) = [(Spades, Jack), (Hearts, Ace)]
val test75 = remove_card2 ([(Hearts, Ace), (Spades, Jack), (Hearts, Ace)], (Diamonds, Ace), IllegalMove) *)

val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val test81 = all_same_color [(Hearts, Ace), (Hearts, Ace), (Diamonds, Num 4)] = true
val test82 = all_same_color [(Hearts, Ace), (Spades, Ace), (Diamonds, Num 4)] = false

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
val test91 = sum_cards [(Clubs, Num 2)] = 2

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val test101 = score ([(Hearts, Num 2),(Diamonds, Num 4)],9) = 1

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false)
              handle IllegalMove => true)



