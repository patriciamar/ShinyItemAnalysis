#' Dichotomous dataset of learning to learn test
#'
#' `LearningToLearn` is a real longitudinal dataset used in Martinkova et al
#' (2020) study, demonstrating differential item functioning in change (DIF-C)
#' on Learning to Learn (LtL) test. Among other variables, it primarily contains
#' binary-coded responses of 782 subjects to (mostly) multiple-choice test
#' consisting of 41 items within 7 subscales (see **Format** for details). Each
#' respondent was tested twice in total -- the first time in Grade 6 and the
#' second time in Grade 9. Most importantly, school track (variable `track_01`
#' or `track`) is available, with 391 students attending basic school (BS) and
#' 391 pursuing selective academic school (AS). This dataset was created using
#' propensity score matching algorithm to achieve similar characteristics in
#' both tracks (see **References** for details). To further simplify the work
#' with `LtL` dataset, we provide computed total scores as well as 7 subscores,
#' both for Grade 6 and Grade 9. The dataset also includes *change* variables
#' for each item (see **Format** for details) for more detailed DIF-C analysis
#' using multinomial regression model.
#'
#' @source Martinkova, P., Hladka, A., & Potuznikova, E. (2020). Is academic
#'   tracking related to gains in learning competence? Using propensity score
#'   matching and differential item change functioning analysis for better
#'   understanding of tracking implications. *Learning and Instruction*, *66*,
#'   101286. \doi{10.1016/j.learninstruc.2019.101286}
#'
#' @keywords datasets
#'
#' @format A `LearningToLearn` data frame consists of 782 observations on the following 141 variables:
#' \describe{
#' \item{track_01}{Dichotomously scored school track, where `"1"` denotes the selective academic school one. }
#' \item{track}{School track, where `"AS"` represents the selective academic school track, and `"BS"` stands
#' for basic school track. }
#' \item{score_6 & score_9}{Total test score value obtained by summing all 41 items of `LtL`, the number denotes
#' the Grade which the respondent was taking at the time of testing. }
#' \item{score_6_subtest1--score_6_subtest7}{Scores of respective cognitive subtest (1--7) of `LtL` in Grade 6. }
#' \item{score_9_subtest1--score_9_subtest7}{Scores of respective cognitive subtest (1--7) of `LtL` in Grade 9. }
#' \item{Item1A_6--Item7F_6}{Dichotomously coded 41 individual items obtained at Grade 6, `"1"` represents
#' the correct answer to the particular item. }
#' \item{Item1A_9--Item7F_9}{Dichotomously coded 41 individual items obtained at Grade 9, `"1"` represents
#' the correct answer to the particular item. }
#' \item{Item1A_changes--Item7F_changes}{Change patterns with those possible values:
#' * a student responded correctly in neither Grade 6 nor in Grade 9 (did not improve, `"00"`)
#' * a student responded correctly in Grade 6 but not in Grade 9 (deteriorated, `"10"`)
#' * a student did not respond correctly in Grade 6 but responded correctly in Grade 9 (improved, `"01"`), and
#' * a student responded correctly in both grades (did not deteriorate, `"11"`)}
#' }
"LearningToLearn"
