#' PROMIS Anxiety Scale Dataset
#'
#' The data contains responses from 766 people sampled from a general population
#' to the PROMIS Anxiety scale (http://www.nihpromis.org) composed of 29
#' Likert-type questions with a common rating scale (1 = Never, 2 = Rarely,
#' 3 = Sometimes, 4 = Often, and 5 = Always).
#'
#' @format
#' A data frame with 766 observations on the following 32 variables.
#' \describe{
#'   \item{\code{age}}{0 = younger than 65 and 1 = 65 and older}
#'   \item{\code{gender}}{0 = Male and 1 = Female}
#'   \item{\code{education}}{0 = some college or higher and 1 = high school or lower}
#'   \item{\code{R1}}{I felt fearful}
#'   \item{\code{R2}}{I felt frightened}
#'   \item{\code{R3}}{It scared me when I felt nervous}
#'   \item{\code{R4}}{I felt anxious}
#'   \item{\code{R5}}{I felt like I needed help for my anxiety}
#'   \item{\code{R6}}{I was concerned about my mental health}
#'   \item{\code{R7}}{I felt upset}
#'   \item{\code{R8}}{I had a racing or pounding heart}
#'   \item{\code{R9}}{I was anxious if my normal routine was disturbed}
#'   \item{\code{R10}}{I had sudden feelings of panic}
#'   \item{\code{R11}}{I was easily startled}
#'   \item{\code{R12}}{I had trouble paying attention}
#'   \item{\code{R13}}{I avoided public places or activities}
#'   \item{\code{R14}}{I felt fidgety}
#'   \item{\code{R15}}{I felt something awful would happen}
#'   \item{\code{R16}}{I felt worried}
#'   \item{\code{R17}}{I felt terrified}
#'   \item{\code{R18}}{I worried about other people's reactions to me}
#'   \item{\code{R19}}{I found it hard to focus on anything other than my anxiety}
#'   \item{\code{R20}}{My worries overwhelmed me}
#'   \item{\code{R21}}{I had twitching or trembling muscles}
#'   \item{\code{R22}}{I felt nervous}
#'   \item{\code{R23}}{I felt indecisive}
#'   \item{\code{R24}}{Many situations made me worry}
#'   \item{\code{R25}}{I had difficulty sleeping}
#'   \item{\code{R26}}{I had trouble relaxing}
#'   \item{\code{R27}}{I felt uneasy}
#'   \item{\code{R28}}{I felt tense}
#'   \item{\code{R29}}{I had difficulty calming down}
#'   \item{\code{score}}{Total score.}
#'   \item{\code{zscore}}{Standardized total score.}
#'   }
#'
#'
#' @source Reexport from `lordif` package; http://www.nihpromis.org
#'
#' @references PROMIS Cooperative Group. Unpublished Manual for the
#'   Patient-Reported Outcomes Measurement Information System (PROMIS) Version
#'   1.1. October, 2008: http://www.nihpromis.org
#'
#' @keywords datasets
#'
"Anxiety"
