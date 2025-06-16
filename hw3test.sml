(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test11 = only_capitals ["a","Bb","C"] = ["Bb","C"]

val test2 = longest_string1 ["A","bc","C"] = "bc"
val test21 = longest_string1 ["A","bc","cc","C"] = "bc"

val test3 = longest_string2 ["A","bc","C"] = "bc"
val test31 = longest_string2 ["A","bb","bc","C"] = "bc"

val test4a = longest_string3 ["A","bc","C"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A"
val test51 = longest_capitalized ["A","bc","BBBB","C"] = "BBBB"
val test52 = longest_capitalized ["A","bc","BB","CC","C"] = "BB"

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test72 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test81 = all_answers (fn x => if x = 2 then SOME [x] else NONE) [2,2,2] = SOME [2,2,2]
val test82 = all_answers (fn x => if x = 2 then SOME [x] else NONE) [] = SOME []

val test9a = count_wildcards Wildcard = 1
val test91a = count_wildcards (TupleP [Wildcard, Wildcard]) = 2
val test92a = count_wildcards (ConstructorP("foo", Wildcard)) = 1
val test93a = count_wildcards (ConstructorP("foo", TupleP[Wildcard, Wildcard])) = 2

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val test91b = count_wild_and_variable_lengths (Variable("aaa")) = 3
val test92b = count_wild_and_variable_lengths (TupleP [Wildcard, Variable "abc"]) = 4
val test93b = count_wild_and_variable_lengths (ConstructorP("Some", TupleP [Variable "xy", Wildcard, ConstP 5])) = 3

val test9c = count_some_var ("x", Variable("x")) = 1
val test91c = count_some_var ("foo", (TupleP [Wildcard, Variable "foo"])) = 1
val test92c = count_some_var ("foo", (ConstructorP("Some", TupleP [Variable "foo", Wildcard, ConstP 5]))) = 1
val test93c = count_some_var ("foo", TupleP [
  Variable "foo",
  Variable "y",
  ConstructorP("C", Variable "foo"),
  Wildcard
]) = 2

val test10 = check_pat (Variable("x")) = true
val p1 = TupleP [
  Variable "foo",
  Variable "y",
  ConstructorP("C", Variable "foo"),
  Wildcard
]
val test101 = check_pat p1 = false
val p2 = TupleP [
  Variable "foo",
  Variable "bar",
  ConstructorP("C", Variable "baz"),
  Wildcard
]
val test102 = check_pat p2 = true

val test11 = match (Const(1), UnitP) = NONE
val test112 = match (Const 1, Wildcard) = SOME []
val test113 = match (Const 1, Variable "test113") = SOME [("test113", Const 1)]
val test114 = match (Unit, UnitP) = SOME []
val test115 = match (Const 17, ConstP 17) = SOME []
val test116 =
  match (Tuple[Const 1, Const 2, Unit], TupleP[ConstP 1, Variable "a", Variable "b"]) = SOME [("b", Unit),("a", Const 2)]
val test117 =
  match (Constructor("same", Const 1), ConstructorP("same", Variable "test117")) = SOME[("test117", Const 1)]

val test12 = first_match Unit [UnitP] = SOME []
val test121 = first_match Unit [] = NONE
val test122 = first_match (Tuple[Const 1, Const 2]) [UnitP, ConstP 5, TupleP[ConstP 1, ConstP 2]] = SOME []
val test123 = first_match (Constructor("same", Const 1)) [UnitP, ConstructorP("same", Variable "test123")] = SOME[("test123", Const 1)]