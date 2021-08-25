#' BFI2 Dataset
#'
#' `BFI2` dataset (Hřebíčková et al., 2020) consists of responses of
#' `r format(nrow(BFI2), big.mark = ",")`
#' respondents
#' (`r format(nrow(BFI2[BFI2$Gender == 0, ]), big.mark = ",")`
#' females,
#' `r format(nrow(BFI2[BFI2$Gender == 1, ]), big.mark = ",")`
#' males) to Big Five Inventory 2 (BFI-2). It contains 60 ordinal items, vector
#' of age, education, and vector of gender membership.
#'
#'
#' The items prefixed with `i` are item scores. Items are indicators of 5 latent
#' personality factors/dimensions/domains, which are further broken down into
#' so-called facets. The 5 personality domains are: N = Negative Emotionality, E
#' = Extraversion, O = Open-Mindedness, C = Consciousness and A = Agreeability.
#' These are further broken down into so-called facets, as shown in the
#' following table:
#'
#' | **Domain** | **Facet** | **Item numbers** |
#' | --- | --- | --- |
#' | E | Sociability (scb)| 1, 16, 31, 46 |
#' | E | Assertiveness (asr)| 6, 21, 36, 51 |
#' | E | Energy Level (enl)| 11, 26, 41, 56 |
#' | A | Compassion (cmp)| 2, 17, 32, 47 |
#' | A | Respectfulness (rsp)| 7, 22, 37, 52 |
#' | A | Trust (trs)| 12, 27, 42, 57 |
#' | C | Organization (org)| 3, 18, 33, 48 |
#' | C | Productiveness (prd)| 8, 23, 38, 53 |
#' | C | Responsibility (rsp) | 13, 28, 43, 58 |
#' | N | Anxiety (anx)| 4, 19, 34, 49 |
#' | N | Depression (dep)| 9, 24, 39, 54 |
#' | N | Emotional Volatility (emt)| 14, 29, 44, 59 |
#' | O | Intellectual Curiosity (int)| 10, 25, 40, 55 |
#' | O | Aesthetic Sensitivity (aes)| 5, 20, 35, 50 |
#' | O | Creative Imagination (crt)| 15, 30, 45, 60 |
#'
#' In the original instrument, some items are inversely oriented, i.e., the
#' higher score means the lower latent trait. This was the case of items number
#' 3, 4, 5, 8, 9, 11, 12, 16, 17, 22, 23, 24, 25, 26, 28, 29, 30, 31, 36, 37,
#' 42, 44, 45, 47, 48, 49, 50, 51, 55, and 58. These **items have been recoded**
#' for you, i.e., displayed is value of `6 - original score`.
#'
#' In the sample code, alternative item names are provided. These item names
#' can be used to decode the item domain, facet, item number, and
#' whether it was recoded or not. For example, `iCorg03r` stands for recoded 3rd
#' item (out of 60) from Consciousness domain and Organization facet.
#'
#' @usage data(BFI2)
#'
#' @examples
#' data(BFI2)
#' colnames(BFI2)[1:60] <- c("iEscb01", "iAcmp02", "iCorg03r", "iNanx04r", "iOaes05r", "iEasr06",
#' "iArsp07", "iCprd08r", "iNdep09r", "iOint10", "iEenl11r", "iAtrs12r", "iCrsp13", "iNemt14",
#' "iOcrt15", "iEscb16r", "iAcmp17r", "iCorg18", "iNanx19", "iOaes20", "iEasr21", "iArsp22r",
#' "iCprd23r", "iNdep24r", "iOint25r", "iEenl26r", "iAtrs27", "iCrsp28r", "iNemt29r",
#' "iOcrt30r", "iEscb31r", "iAcmp32", "iCorg33", "iNanx34", "iOaes35", "iEasr36r", "iArsp37r",
#' "iCprd38", "iNdep39", "iOint40", "iEenl41", "iAtrs42r", "iCrsp43", "iNemt44r", "iOcrt45r",
#' "iEscb46", "iAcmp47r", "iCorg48r", "iNanx49r", "iOaes50r", "iEasr51r", "iArsp52", "iCprd53",
#' "iNdep54", "iOint55r", "iEenl56", "iAtrs57", "iCrsp58r", "iNemt59", "iOcrt60")
#'
#' @references Hřebíčková, M., Jelínek, M., Květon,P., Benkovič, A., Botek, M.,
#'   Sudzina, F. Soto, Ch., John, O. (2020).  Big Five Inventory 2 (BFI-2):
#'   Hierarchický model s 15 subškálami \[Big Five Inventory 2 (BFI-2):
#'   Hierarchical model with 15 subscales, in Czech\]. *Československá
#'   psychologie, 64,* 437--460.
#'
#'   Soto, C. J., & John, O. P. (2017). The next Big Five Inventory (BFI-2):
#'   Developing and assessing a hierarchical model with 15 facets to enhance
#'   bandwidth, fidelity, and predictive power. *Journal of Personality and
#'   Social Psychology, 113,* 117--143.
#'
#' @note Thanks to Martina Hřebíčková for sharing this dataset.
#'
#' @keywords datasets
#' @md
#' @encoding UTF-8
#'
#' @format
#' \code{BFI2} is a \code{data.frame} consisting of
#' `r format(nrow(BFI2), big.mark = ",")`
#' observations on 64 variables.
#' \describe{
#'   \item{i1--i60}{The BFI items, scored on Likert scale where
#'   `1` = Disagree strongly,
#'   `2` = Disagree a little,
#'   `3` = Neutral; no opinion,
#'   `4` = Agree a little, and
#'   `5` = Agree strongly.
#'   Some items were recoded so that all items are scored in the same direction, see Details.}
#'   \item{Gender}{Gender membership, `0` = females, `1` = males.}
#'   \item{Age}{Age in years.}
#'   \item{Educ}{Education,
#'   `1` = Basic school,
#'   `2` = Secondary technical school,
#'   `3` = Secondary general school,
#'   `4` = Other secondary school,
#'   `5` = Tertiary professional school,
#'   `6` = Bachelor degree,
#'   `7` = Masters degree,
#'   `8` = PhD}
#' }
"BFI2"
