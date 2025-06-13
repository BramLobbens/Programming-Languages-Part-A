use "module4_extra_practice_problems.sml"

val passing_grade: final_grade= { id=1, grade=SOME(81) }
val failing_grade = { id=2, grade=SOME(60) }
val absent_grade = { id=3, grade=NONE }
val t00 = pass_or_fail (passing_grade) = pass
val t01 = pass_or_fail (failing_grade) = fail
val t02 = pass_or_fail (absent_grade) = fail

val t10 = has_passed (passing_grade) = true
val t11 = has_passed (failing_grade) = false
val t12 = has_passed (absent_grade) = false

val t20 = number_passed ([passing_grade]) = 1
val t21 = number_passed ([failing_grade]) = 0
val t22 = number_passed ([passing_grade, passing_grade]) = 2
val t23 = number_passed ([failing_grade, passing_grade]) = 1
val t24 = number_passed ([]) = 0

val t30 = number_misgraded ([]) = 0
val t31 = number_misgraded ([(pass, passing_grade)]) = 0
val t32 = number_misgraded ([(pass, failing_grade)]) = 1
val t33 = number_misgraded ([(fail, failing_grade)]) = 0
val t34 = number_misgraded ([(pass, failing_grade), (pass, passing_grade), (fail, passing_grade)]) = 2