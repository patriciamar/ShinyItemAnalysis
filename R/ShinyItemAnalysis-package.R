#' ShinyItemAnalysis: Test and Item Analysis via shiny
#'
#' @description The ShinyItemAnalysis package contains an interactive shiny
#'   application for analysis of educational tests and their items which can be
#'   accessed using function \code{startShinyItemAnalysis()}. The shiny
#'   application covers a broad range of psychometric methods and offers data
#'   examples, model equations, parameter estimates, interpretation of results,
#'   together with a selected R code, and is therefore suitable for teaching
#'   psychometric concepts with R. It also allows the users to upload and
#'   analyze their own data and to automatically generate analysis reports in
#'   PDF or HTML.\cr
#'
#'   Besides, the package provides its own functions for test and item analysis
#'   within classical test theory framework (e.g., functions \code{gDiscrim()},
#'   \code{ItemAnalysis()}, \code{DistractorAnalysis()}, or \code{DDplot()}),
#'   using various regression models (e.g., \code{plotCumulative()},
#'   \code{plotAdjacent()}, \code{plotMultinomial()}, or
#'   \code{plotDIFLogistic()}), and under IRT framework (e.g.,
#'   \code{ggWrightMap()}, or \code{plotDIFirt()}).\cr
#'
#'   Package also contains several demonstration datasets including the
#'   \code{HCI} dataset from the reference paper by Martinkova and Drabinova
#'   (2018).\cr
#'
#' @aliases ShinyItemAnalysis-package
#'
#' @import difNLR difR shiny
#' @importFrom cowplot plot_grid
#' @importFrom deltaPlotR deltaPlot
#' @importFrom ggplot2 aes aes_string coord_flip element_blank element_line
#'   element_rect element_text geom_abline ggplot_build position_dodge
#'   geom_histogram geom_hline geom_line geom_point geom_ribbon geom_text ggplot
#'   ggsave ggtitle labs scale_color_manual scale_colour_manual
#'   scale_fill_manual scale_linetype_manual scale_shape_manual
#'   scale_size_continuous scale_x_continuous scale_x_discrete
#'   scale_y_continuous scale_y_reverse geom_col stat_function stat_summary
#'   theme theme_bw unit xlab xlim ylab ylim
#' @importFrom graphics lines plot plot.new
#' @importFrom grDevices dev.off hcl png rainbow recordPlot
#' @importFrom ltm ltm rasch tpm
#' @importFrom mirt fscores itemfit mirt
#' @importFrom nnet multinom
#' @importFrom psych alpha polychoric
#' @importFrom psychometric item.exam
#' @importFrom rmarkdown render
#' @importFrom shinyjs show hide useShinyjs
#' @importFrom stats aggregate coef coefficients complete.cases cor deriv deriv3
#'   deviance fitted glm median na.exclude na.omit nls p.adjust pnorm pchisq qnorm
#'   qchisq quantile relevel sd symnum vcov xtabs
#' @importFrom stringr str_sub
#' @importFrom utils capture.output data head packageVersion read.csv
#'
#' @section Functions: \itemize{
#' \item \code{\link{startShinyItemAnalysis}}
#' \item \code{\link{DDplot}}
#' \item \code{\link{DistractorAnalysis}}
#' \item \code{\link{plotDistractorAnalysis}}
#' \item \code{\link{gDiscrim}}
#' \item \code{\link{ggWrightMap}}
#' \item \code{\link{ItemAnalysis}}
#' \item \code{\link{plotAdjacent}}, \code{\link{plotCumulative}}, \code{\link{plotMultinomial}}
#' \item \code{\link{plotDIFirt}}, \code{\link{plotDIFLogistic}}
#' \item \code{\link{plot_corr}}
#' \item \code{\link{recode_nr}}
#' }
#'
#' @section Datasets:
#' \itemize{
#' \item \code{\link{dataMedical}}
#' \item \code{\link{dataMedicalgraded}}
#' \item \code{\link{dataMedicalkey}}
#' \item \code{\link{dataMedicaltest}}
#' \item \code{\link{HCI}}
#' \item \code{\link{HCIdata}}
#' \item \code{\link{HCIgrads}}
#' \item \code{\link{HCIkey}}
#' \item \code{\link{HCIprepost}}
#' \item \code{\link{HCItest}}
#' \item \code{\link{HCItestretest}}
#' \item \code{\link{LearningToLearn}} }
#'
#' @details Package: ShinyItemAnalysis \cr
#' Type: Package \cr
#' Version: 1.3.4-1 \cr
#' Date: 2020-12-15 \cr
#' Depends: R (>= 3.5.0) \cr
#' Imports: cowplot, data.table, deltaPlotR, difNLR (>= 1.3.2), difR (>= 5.0),
#' dplyr, DT, ggdendro, ggplot2, gridExtra, knitr, latticeExtra, ltm, magrittr,
#' mirt (>= 1.24), msm, nnet, plotly, psych, purrr, psychometric, rlang,
#' rmarkdown, rstudioapi, scales, shiny (>= 1.0.3), shinyBS, shinydashboard,
#' shinyjs (>= 0.9), stringr, tibble, tidyr, VGAM, xtable
#' License: GPL-3 \cr
#' BugReports: \url{https://github.com/patriciamar/ShinyItemAnalysis/issues} \cr
#' Website: \url{http://www.ShinyItemAnalysis.org/} \cr
#' Encoding: UTF-8 \cr
#'
#' @author Patricia Martinkova  \cr
#' Institute of Computer Science of the Czech Academy of Sciences  \cr
#' Faculty of Education, Charles University \cr
#' \email{martinkova@@cs.cas.cz} \cr
#'
#' Adela Hladka (nee Drabinova) \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' Faculty of Mathematics and Physics, Charles University \cr
#' \email{hladka@@cs.cas.cz} \cr
#'
#' @references Martinkova, P., & Drabinova, A. (2018). ShinyItemAnalysis for
#' Teaching Psychometrics and to Enforce Routine Analysis of Educational Tests.
#' The R Journal, 10(2), 503-515. \url{https://doi.org/10.32614/RJ-2018-074}
#'
#' @docType package
"_PACKAGE"
# > [1] "_PACKAGE"
