#' Compute traditional item analysis indices
#'
#' Computes various traditional item analysis indices including difficulty,
#' discrimination and item validity. For ordinal items, the function returns
#' scaled values for some of the indices. See the details below.
#'
#' For calculation of generalized ULI index, it is possible to specify a custom
#' number of groups `k`, and which two groups `l` and `u` are to be compared.
#'
#' In ordinal items, difficulty is calculated as difference of average score
#' divided by range (maximal possible score `maxscore` minus minimal possible
#' score `minscore`).
#'
#' If `cutscore` is provided, item analysis is conducted on binarized data;
#' values greater or equal to cut-score are set to `1`, other values are set to
#' `0`. Both the `minscore` and `maxscore` arguments are then ingored and set to
#' 0 and 1, respectively.
#'
#'
#' @param Data *matrix* or *data.frame* of items to be examined. Rows represent
#'   respondents, columns represent items.
#'
#' @param criterion vector of criterion values.
#'
#' @param minscore,maxscore *integer*, theoretical minimal/maximal score. If not
#'   provided, these are computed on observed data. Automatically recycled to
#'   the number of columns of the data.
#'
#' @param cutscore *integer* If provided, the input data are binarized
#'   accordingly. Automatically recycled to the number of columns of the data.
#'
#' @param k,l,u Arguments passed on to [gDiscrim()]. Provide these if you want to
#'   compute generalized upper-lower index along with a standard ULI (using `k`
#'   = 3, `l` = 1, `u` = 3), which is provided by default.
#'
#' @param bin *deprecated*, use `cutscore` instead. See the **Details**.
#'
#' @return A `data.frame` with following columns:
#'   \item{`Difficulty`}{average score of the item divided by its range.}
#'   \item{`Mean`}{average item score.}
#'   \item{`SD`}{standard deviation of the item score.}
#'   \item{`Cut.score`}{cut-score specified in `cutscore`.}
#'   \item{`obs.min`}{observed minimal score.}
#'   \item{`Min.score`}{minimal score specified in `minscore`; if not provided,
#'   observed minimal score.}
#'   \item{`obs.max`}{observed maximal score.}
#'   \item{`Max.score`}{maximal score specified in `maxscore`; if not provided,
#'   observed maximal score.}
#'   \item{`Prop.max.score`}{proportion of maximal scores.}
#'   \item{`RIT`}{item-total correlation (correlation between item score and
#'   overall test score).}
#'   \item{`RIR`}{item-rest correlation (correlation between item score and
#'   overall test score without the given item).}
#'   \item{`ULI`}{upper-lower index using the standard parameters (3 groups,
#'   comparing 1st and 3rd).}
#'   \item{`Corr.criterion`}{correlation between item score and criterion
#'   `criterion`.}
#'   \item{`gULI`}{generalized ULI. `NA` when the arguments `k`, `l`, and `u`
#'   were not provided.}
#'   \item{`Alpha.drop`}{Cronbach's alpha without given item.}
#'   \item{`Index.rel`}{Gulliksen's (1950) item reliability index.}
#'   \item{`Index.val`}{Gulliksen's (1950) item validity index.}
#'   \item{`Perc.miss`}{Percentage of missed responses on the particular item.}
#'   \item{`Perc.nr`}{Percentage of respondents that did not reached the item
#'   nor the subsequent ones, see [recode_nr()] for further details.}
#'
#' @author Patricia Martinkova \cr Institute of Computer Science of the Czech
#'   Academy of Sciences \cr \email{martinkova@@cs.cas.cz}
#'
#'   Jan Netik \cr Institute of Computer Science of the Czech Academy of
#'   Sciences \cr \email{netik@@cs.cas.cz}
#'
#'   Jana Vorlickova \cr Institute of Computer Science of the Czech Academy of
#'   Sciences
#'
#'   Adela Hladka \cr Institute of Computer Science of the Czech Academy of
#'   Sciences \cr \email{hladka@@cs.cas.cz}
#'
#' @references Martinkova, P., Stepanek, L., Drabinova, A., Houdek, J.,
#'   Vejrazka, M., & Stuka, C. (2017). Semi-real-time analyses of item
#'   characteristics for medical school admission tests. In: Proceedings of the
#'   2017 Federated Conference on Computer Science and Information Systems.
#'   https://doi.org/10.15439/2017F380
#'
#'   Gulliksen, H. (1950). *Theory of mental tests.* John Wiley & Sons Inc.
#'   https://doi.org/10.1037/13240-000
#'
#' @seealso [DDplot()], [gDiscrim()], [recode_nr()]
#'
#' @examples
#' \dontrun{
#' # binary dataset
#' dataBin <- dataMedical[, 1:100]
#' # ordinal dataset
#' dataOrd <- dataMedicalgraded[, 1:100]
#' # study success is the same for both data sets
#' StudySuccess <- dataMedical[, 102]
#'
#' # item analysis for binary data
#' head(ItemAnalysis(dataBin))
#' # item analysis for binary data using also study success
#' head(ItemAnalysis(dataBin, criterion = StudySuccess))
#'
#' # item analysis for binary data
#' head(ItemAnalysis(dataOrd))
#' # item analysis for binary data using also study success
#' head(ItemAnalysis(dataOrd, criterion = StudySuccess))
#' # including also item analysis for binarized data
#' head(ItemAnalysis(dataOrd,
#'   criterion = StudySuccess, k = 5, l = 4, u = 5,
#'   maxscore = 4, minscore = 0, cutscore = 4
#' ))
#' }
#'
#' @importFrom purrr modify2 map2_dbl
#' @importFrom tidyr drop_na
#'
#' @export
ItemAnalysis <- function(Data, minscore = NULL, maxscore = NULL,
                         cutscore = NULL, criterion = NULL,
                         k = NULL, l = NULL, u = NULL, bin = "deprecated") {
  if (!missing(bin)) stop("Argument `bin` is deprecated, the dataset will be binarized according to `cutscore` whenewer it is provided.", call. = FALSE)

  # if there is any cutscore provided, binarize the dataset accordingly
  if (!is.null(cutscore)) {
    Data <- modify2(Data, cutscore, ~ .x >= .y) # this is highly efficient

    # ignore user-supplied min- and maxscores, becauses if cutscore is provided,
    # those are always 0 and 1, respectively
    minscore <- 0L
    maxscore <- 1L
  } else {
    cutscore <- NA
  }

  # missed items (NAs)
  missed <- sapply(
    Data,
    function(x) {
      sum(is.na(x)) / length(x) # this is faster than mean, surprisingly
    }
  )

  # not-reached items (coded as 99) - this is the most expensive operation
  prop_nr <- sapply(
    recode_nr(Data),
    function(x) {
      sum(x == 99, na.rm = TRUE) / length(x)
    }
  )

  # get observed min- and maxscore and if these are not provided, used them in
  # the subsequent analyses
  obs_minscore <- sapply(Data, min, na.rm = TRUE)
  if (is.null(minscore)) {
    minscore <- obs_minscore
  }
  obs_maxscore <- sapply(Data, max, na.rm = TRUE)
  if (is.null(maxscore)) {
    maxscore <- obs_maxscore
  }
  mean <- colMeans(Data, na.rm = TRUE)

  diff <- (mean - minscore) / (maxscore - minscore)
  SD <- sapply(Data, sd, na.rm = TRUE)

  prop_max <- map2_dbl(Data, maxscore, ~ mean(.x == .y, na.rm = TRUE))

  # for ULI and gULI, we are running all the gDiscrim checks twice, unfortunately
  uli <- gDiscrim(Data, maxscore = maxscore, minscore = minscore)

  # compute gULI only when all of its parameters are provided
  guli <- NA # allocate object for data.frame
  if (!any(is.null(k), is.null(l), is.null(u))) {
    guli <- gDiscrim(
      Data = Data, k = k, l = l, u = u,
      maxscore = maxscore, minscore = minscore
    ) # more efficient variant for this concrete case?
  }

  ts <- rowSums(Data)
  rir <- diag(cor(ts - Data, Data, use = "pairwise.complete.obs"))
  rit <- cor(ts, Data, use = "pairwise.complete.obs")[1L, ] # subset makes a vector

  # criterion cor
  ric <- NA # allocate object for data.frame
  if (!is.null(criterion)) {
    ric <- cor(criterion, Data, use = "pairwise.complete.obs")[1L, ]
  }

  # Gulliksen item indices
  iri <- rit * SD

  ivi <- NA # allocate object for data.frame
  if (!is.null(criterion)) {
    ivi <- ric * SD
  }

  # precompute objects that alpha drop accesses
  n_items <- ncol(Data)
  items_var <- SD^2 # use SD from above
  data_var <- var(Data, na.rm = TRUE)

  alpha_drop <- sapply(
    seq_len(n_items),
    function(x) {
      (n_items - 1) / (n_items - 2) *
        (1 - sum(items_var[-x]) / sum(data_var[-x, -x]))
    }
  )


  # use hard-to-type-hard-to-read legacy names (despite that sentence case with
  # "." delimiter was superseded decades (!) ago in S/R because of S3 class
  # introduction
  data.frame(
    Difficulty = diff,
    Mean = mean,
    SD,
    Cut.score = cutscore,
    obs.min = obs_minscore,
    Min.score = minscore,
    obs.max = obs_maxscore,
    Max.score = maxscore,
    Prop.max.score = prop_max,
    RIR = rir,
    RIT = rit,
    Corr.criterion = ric,
    ULI = uli,
    gULI = guli,
    Alpha.drop = alpha_drop,
    Index.rel = iri,
    Index.val = ivi,
    Perc.miss = missed * 100,
    Perc.nr = prop_nr * 100
  )
}
