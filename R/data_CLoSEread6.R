#' Czech Longitudinal Study in Education (CLoSE) - reading in 6th grade
#'
#' `CLoSEread6` dataset consists of the dichotomously scored responses of 2,634 students
#' (1,324 boys, 1,310 girls) on 19 multiple-choice items in a test of reading
#' skills, version B, taken in the 6th grade. Item responses were dichotomized: 1
#' point was awarded only if the answer was fully correct and 0 if it was not
#' (Greger, Straková, & Martinková, 2022; Martinková, Hladká, & Potužníková, 2020;
#' Hladká, Martinková, & Magis, 2023)
#'
#' @references
#' Greger, D., Straková, J., & Martinková, P. (2022). Extending the ILSA study
#'    design to a longitudinal design. TIMSS & PIRLS extension in the Czech Republic:
#'    CLoSE study. In T. Nilsen, A. Stancel-Piatak, & J.-E. Gustafsson (Eds.),
#'    *Springer international handbooks of education. International handbook of comparative
#'    large-scale studies in education: Perspectives, methods and findings*. Springer.
#'    \doi{10.1007/978-3-030-38298-8_31-1}
#'
#' Martinková, P., Hladká, A., & Potužníková, E. (2020). Is academic
#'   tracking related to gains in learning competence? Using propensity score
#'   matching and differential item change functioning analysis for better
#'   understanding of tracking implications. *Learning and Instruction*, *66*,
#'   101286. \doi{10.1016/j.learninstruc.2019.101286}
#'
#' @source
#' Hladká, A., Martinková, P., & Magis, D. (2023). Combining item purification and
#'   multiple comparison adjustment methods in detection of differential item
#'   functioning. *Multivariate Behavioral Research*, In Press.
#'
#' @keywords datasets
#'
#' @format `CLoSEread6` is a `data.frame` consisting of 2,634 observations on
#'   the 20 variables.
#'   \describe{
#'   \item{Q6B_1-Q6B_19}{Dichotomously scored items of the test on reading skills. }
#'   \item{gender}{Gender membership, `"0"` boys, `"1"` girls. }
#' }
"CLoSEread6"
