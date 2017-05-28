#' Dichotomous Data Set of Admission Test to Medical School
#'
#' @docType data
#' @name dataMedical
#' @description The \code{dataMedical} data set consists of the responses of 2,392 subjects
#' (750 males, 1,633 females and 9 subjects without gender specification) to admission test to
#' a medical school. It contains 100 items. A correct answer is coded as 1 and incorrect
#' answer as 0. Missing answers were evaluated as incorrect, i.e. 0.
#'
#' @usage data(dataMedical)
#'
#' @author
#' Cestmir Stuka \cr
#' First Faculty of Medicine, Charles University in Prague \cr
#'
#' Martin Vejrazka \cr
#' First Faculty of Medicine, Charles University in Prague \cr
#'
#' Patricia Martinkova \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' martinkova@cs.cas.cz \cr
#'
#' Jakub Houdek \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' The Faculty of Informatics and Statistics, University of Economics, Prague \cr
#' houdek.james@gmail.com \cr
#'
#' Adela Drabinova \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' Faculty of Mathematics and Physics, Charles University \cr
#' adela.drabinova@gmail.com \cr
#'
#' @references
#' Stuka, C. Vejrazka, M., Martinkova, P. Komenda, M. and Stepanek, L. (2016). The Use of  Test and Item Analisis for Improvement of Tests. Workshop held at conference MEFANET, 2016, Brno, Czech Republic.
#'
#' @keywords datasets
#'
#' @seealso \code{\link{dataMedicaltest}}, \code{\link{dataMedicalkey}}
#'
#' @format A \code{dataMedical} data frame consists of 2,392 observations on
#' the following 101 variables. The first 100 columns represent dichotomously scored
#' items of the test. The 101st column is vector of gender membership; values 0 and 1
#' refer to males and females.
"dataMedical"

