#' ShinyItemAnalysis: Test and Item Analysis via Shiny
#'
#' @description The `ShinyItemAnalysis` package contains an interactive Shiny
#' application for the psychometric analysis of educational tests, psychological
#' assessments, health-related and other types of multi-item measurements, or
#' ratings from multiple raters, which can be accessed using function
#' `startShinyItemAnalysis()`. The shiny application covers a broad range of
#' psychometric methods and offers data examples, model equations, parameter
#' estimates, interpretation of results, together with a selected R code, and is
#' therefore suitable for teaching psychometric concepts with R. It also allows
#' the users to upload and analyze their own data and to automatically generate
#' analysis reports in PDF or HTML.
#'
#' Besides, the package provides its own functions for test and item analysis
#' within classical test theory framework (e.g., functions `gDiscrim()`,
#' `ItemAnalysis()`, `DistractorAnalysis()`, or `DDplot()`), using various
#' regression models (e.g., `plotCumulative()`, `plotAdjacent()`,
#' `plotMultinomial()`, or `plotDIFLogistic()`), and under IRT framework (e.g.,
#' `ggWrightMap()`, or `plotDIFirt()`).
#'
#' Package also contains several demonstration datasets including the `HCI`
#' dataset from the book by Martinkova and Hladka (2023), and from paper
#' by Martinkova and Drabinova (2018).
#'
#'
#' @importFrom stats aggregate coef complete.cases cor deviance fitted glm
#'   median na.exclude na.omit p.adjust pnorm pchisq qnorm qchisq quantile
#'   relevel sd vcov xtabs
#' @importFrom utils capture.output data head packageVersion read.csv
#'
#' @section Functions:
#' \itemize{
#'   \item [startShinyItemAnalysis()]
#'   \item [DDplot()]
#'   \item [DistractorAnalysis()]
#'   \item [plotDistractorAnalysis()]
#'   \item [fa_parallel()]
#'   \item [gDiscrim()]
#'   \item [ggWrightMap()]
#'   \item [ICCrestricted()]
#'   \item [ItemAnalysis()]
#'   \item [plotAdjacent()], [plotCumulative()], [plotMultinomial()]
#'   \item [plotDIFirt()], [plotDIFLogistic()]
#'   \item [plot_corr()]
#'   \item [recode_nr()]
#' }
#'
#' @section Datasets:
#' \itemize{
#'   \item [AIBS()]
#'   \item [Anxiety()]
#'   \item [AttitudesExpulsion()]
#'   \item [BFI2()]
#'   \item [CZmatura()]
#'   \item [CZmaturaS()]
#'   \item [dataMedical()]
#'   \item [dataMedicalgraded()]
#'   \item [dataMedicalkey()]
#'   \item [dataMedicaltest()]
#'   \item [HCI()]
#'   \item [HCIdata()]
#'   \item [HCIgrads()]
#'   \item [HCIkey()]
#'   \item [HCIlong()]
#'   \item [HCIprepost()]
#'   \item [HCItest()]
#'   \item [HCItestretest()]
#'   \item [HeightInventory()]
#'   \item [LearningToLearn()]
#'   \item [MSATB()]
#'   \item [MSclinical()]
#'   \item [NIH()]
#'   \item [TestAnxietyCor()]
#' }
#'
#' @author Patricia Martinkova  \cr Institute of Computer Science of the Czech
#'   Academy of Sciences  \cr Faculty of Education, Charles University \cr
#'   \email{martinkova@@cs.cas.cz}
#'
#'   Adela Hladka (nee Drabinova) \cr Institute of Computer Science of the Czech
#'   Academy of Sciences
#'
#'   Jan Netik \cr Institute of Computer Science of the Czech Academy of
#'   Sciences \cr
#'
#' @references Martinkova, P., & Hladka, A. (2023). Computational Aspects of
#'   Psychometric Methods: With R. Chapman and Hall/CRC. \doi{10.1201/9781003054313}
#'
#'   Martinkova, P., & Drabinova, A. (2018). ShinyItemAnalysis for
#'   teaching psychometrics and to enforce routine analysis of educational
#'   tests. The R Journal, 10(2), 503--515, \doi{10.32614/RJ-2018-074}
#'
#' @docType package
"_PACKAGE"
