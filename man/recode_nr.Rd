% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recode_nr.R
\name{recode_nr}
\alias{recode_nr}
\title{Recognize and recode not-reached responses}
\usage{
recode_nr(Data, nr_code = 99, df)
}
\arguments{
\item{Data}{matrix or data.frame: object to be recoded, must include only
items columns and no additional information}

\item{nr_code}{single character, integer or numeric: specifying how should be
recognized not-reached responses coded (default is \code{99})}

\item{df}{deprecated. Use argument \code{Data} instead.}
}
\value{
A \code{data.frame} object.
}
\description{
\code{recode_nr()} function recognizes and recodes not-reached
responses, i.e., missing responses to items such that all subsequent
items are missed as well by the respondent.
}
\examples{
HCImissed <- HCI[, 1:20]

# simulate skipped (missed) and not-reached items in HCI dataset
set.seed(4211)
for (i in 1:150) {
  # not-reached (minimum at 10th item, maximum at 20th)
  HCImissed[sample(1:nrow(HCImissed), 1), seq(sample(10:20, 1), 20)] <- NA

  # missed with random location
  HCImissed[sample(1:nrow(HCImissed), 1), sample(1:20, 1)] <- NA
}

summary(HCImissed)

HCImissedNR <- recode_nr(HCImissed, nr_code = 99)
head(HCImissedNR)
summary(HCImissedNR)
}
\seealso{
\code{\link[=ItemAnalysis]{ItemAnalysis()}}
}
\author{
Jan Netik \cr
Institute of Computer Science of the Czech Academy of Sciences \cr
\email{netik@cs.cas.cz}

Patricia Martinkova \cr
Institute of Computer Science of the Czech Academy of Sciences \cr
\email{martinkova@cs.cas.cz} \cr
}
