
#' Fit Baseline Logit Intercept-Slope (BLIS) model on nominal data
#'
#' `blis` fits the IRT Nominal Response Model to data from multiple-choice tests,
#' while accounting for the correct answer and treating this option as a baseline
#' in this baseline-category logit model. The intercept-slope parametrization in
#' BLIS can be converted to IRT (difficulty-discrimination) parametrization (BLIRT).
#'
#' For the details on `coef` method dispatched for fitted BLIS model, see
#' [coef,BlisClass-method]. To get more on the class, see [BlisClass-class].
#'
#' @param Data *data.frame* or *tibble* with all columns being factors. Support
#'   for *matrix* is limited and behavior not guaranteed.
#' @param key A single-column `data.frame`, (**not** matrix) `tibble` or -
#'   preferably - a factor vector of levels considered as correct responses.
#' @inheritDotParams mirt::mirt -data -model -itemtype -guess -upper -pars -key
#'
#' @return Fitted model of class [BlisClass-class] (extending standard `mirt`'s
#'   `SingleGroupClass`).
#'
#' @importFrom purrr map_dfr walk
#' @importFrom dplyr relocate mutate row_number
#' @importFrom tibble add_row tibble
#' @importFrom mirt mirt
#' @importFrom methods new
#'
#' @export
#'
#' @aliases blis
#' @family BLIS/BLIRT related
#'
#' @author Jan Netik \cr Institute of Computer Science of the Czech Academy of
#'   Sciences \cr \email{netik@@cs.cas.cz}
#'
#'   Patricia Martinkova \cr Institute of Computer Science of the Czech Academy
#'   of Sciences \cr \email{martinkova@@cs.cas.cz}
#'
#' @examples
#' fitted_blis <- fit_blis(HCItest[, 1:20], HCIkey, SE = TRUE)
#' coef(fitted_blis)
#' coef(fitted_blis)$`Item 12`
#' coef(fitted_blis, IRTpars = TRUE)
#' coef(fitted_blis, IRTpars = TRUE, CI = 0.90) # 90% CI instead of 95% CI
#' coef(fitted_blis, IRTpars = TRUE, printSE = TRUE) # SE instead of CI
fit_blis <- function(Data, key, ...) {

  # convert to integers, create original levels list with key stored
  int_data_lvls_key <- nominal_to_int(Data, key)

  Data <- int_data_lvls_key[["Data"]]
  orig_levels <- int_data_lvls_key[["orig_levels"]]

  # create item pars to be forwarded to mirt (as pars arg)
  # set a* to 1 and turn of the estimation
  # turn estimation of correct response off and assign 0 as its value
  # set initial values of distractors' slopes to -0.5 (have to be less than correct resp. value)
  item_pars <- map_dfr(orig_levels, ~ {
    lvls_int <- seq_along(.x) - 1L # get resp. indices
    key_int <- which(attr(.x, "key", exact = TRUE)) - 1L # get index of correct resp.

    list(
      name = c("a1", paste0("ak", lvls_int), paste0("d", lvls_int)),
      value = c(1, c(0, -.5)[(lvls_int != key_int) + 1L], rep(0, length(lvls_int))),
      est = c(FALSE, rep(lvls_int != key_int, 2L))
    )
  },
  .id = "item" # append item name to the result
  )

  # create entries for groups - we'll use only one
  group_pars <- tibble(
    group = "all", item = "GROUP", class = "GroupPars",
    name = c("MEAN_1", "COV_11"), value = c(0, 1),
    lbound = c(-Inf, 1e-04), ubound = Inf, est = FALSE,
    prior.type = "none", prior_1 = NaN, prior_2 = NaN
  )

  # bind group and item pars, set col order, append parnums
  pars <- item_pars %>%
    mutate(
      class = "nominal",
      group = "all", lbound = -Inf, ubound = Inf,
      prior.type = "none", prior_1 = NaN, prior_2 = NaN
    ) %>%
    add_row(group_pars) %>%
    mutate(parnum = row_number()) %>%
    relocate(
      .data$group, .data$item, .data$class, .data$name, .data$parnum,
      .data$value, .data$lbound, .data$ubound, .data$est,
      .data$prior.type, .data$prior_1, .data$prior_2
    ) %>%
    as.data.frame()


  # fit the model
  fit <- mirt(Data, model = 1, itemtype = "nominal", pars = pars, ...)

  # set our class to have a control of coef method
  return(new("BlisClass", fit, orig_levels = orig_levels))
}

# just an alias for fit_blis:
#' @rdname fit_blis
#' @export
blis <- function(Data, key, ...) {
  fit_blis(Data = Data, key = key, ...)
}


#' Turn nominal (factor) data to integers, keep original levels with a key of
#' correct responses alongside
#'
#' Convert a `data.frame` or `tibble` with factor variables (items) to integers,
#' keeping the original factor levels (i.e. response categories) and correct
#' answers (stored as an `key` attribute of each item) alongside.
#'
#' Fitting a nominal model using [mirt::mirt()] package requires the dataset to
#' consist only of integers, *arbitrarily* representing the response categories.
#' You can convert your dataset to integers on your own in that case.
#'
#' On the other hand, BLIS model (and thus also the BLIRT parametrization)
#' further requires the information of correct item response category. On top of
#' that, the same information is leveraged when fitting a `mirt` model that
#' conserves the "directionality" of estimated latent ability (using a model
#' definition from [obtain_nrm_def()]). In these cases, you are recommended to
#' use [nominal_to_int()] (note that [fit_blis()] and [blis()] does this
#' internally). Note also that fitted BLIS model (of class [BlisClass-class]) stores
#' the original levels with correct answer key in its `orig_levels` slot,
#' accessible by a user via [get_orig_levels()].
#'
#' @inheritParams fit_blis
#'
#' @importFrom purrr map2 modify walk
#' @family BLIS/BLIRT related
#'
#' @return *List* of original levels with logical attribute `key`, which stores
#'   the information on which response (level) is considered  correct. *Note
#'   that levels not used in the original data are dropped.*
#'
#' @export
nominal_to_int <- function(Data, key) {
  # assert factor columns
  walk(
    Data,
    ~ if (!inherits(.x, c("factor", "ordered"))) {
      stop("All columns have to be factors.", call. = FALSE)
    }
  )

  # assert key is factor
  key <- unlist(key)
  if (!inherits(key, c("factor", "ordered"))) {
    stop("The key have to be a factor.", call. = FALSE)
  }

  # assert length compatibility
  if (ncol(Data) != length(key)) {
    stop("The key and data dimensions have to be compatible.", call. = FALSE)
  }


  # drop unused levels in case Data have been modified in some way
  # -- prevent mismatch of level-int
  Data <- droplevels(Data)

  # turn everything to integers
  Data <- modify(Data, unclass)

  # for those who stores their key as some exotic class/type
  key <- unlist(key)

  # store original levels and key (+ this will be used for item pars creation)
  orig_levels <- map2(Data, key, ~ {
    lvls <- levels(.x)
    attr(lvls, "key") <- levels(.x) == .y
    lvls
  })

  list(Data = Data, orig_levels = orig_levels)
}


#' Obtain model definition for `mirt`'s nominal model taking in account the key
#' of correct answers
#'
#' @description Standard `mirt` model with `itemtype = "nominal"` puts the
#'   identification constrains on the item response category slopes such as
#'   \eqn{ak_0 = 0} and \eqn{ak_{(K-1)} = (K - 1)}, freely estimating the rest.
#'
#'   While nominal item responses are unordered by definition, it is often the
#'   case that one of the item response categories is correct and the
#'   respondents endorsing this category "naturally" possess a higher latent
#'   ability. Use this function to obtain model definition where the correct
#'   response category \eqn{k_c} for item \eqn{i} with \eqn{K} possible response
#'   categories translates to constrains \eqn{ak_{k_c} = (K - 1)} and
#'   \eqn{ak_{k_{d1}} = 0}, with \eqn{k_{d1}} being the first incorrect response
#'   category (i.e. the first distractor).
#'
#'
#' @param data_with_key The output of `nominal_to_int()`.
#' @param ... arguments passed onto `mirt::mirt()`. No practical use for now.
#'
#' @return A `data.frame` with the starting values, parameter numbers,
#'   estimation constrains etc. Pass it as `pars` argument of [mirt::mirt()].
#' @export
#'
#' @importFrom purrr map
#'
#' @family BLIS/BLIRT related
#'
#' @examples
#' library(mirt)
#'
#' # convert nominal data to integers and the original labels with correct answers
#' data_with_key <- nominal_to_int(HCItest[, 1:20], HCIkey)
#'
#' # build model definition for {mirt} using the returned list from above
#' nrm_def <- obtain_nrm_def(data_with_key)
#'
#' # fit the nominal model using the obtained model definition in `pars` argument
#' fit <- mirt(data_with_key$Data, 1, "nominal", pars = nrm_def)
obtain_nrm_def <- function(data_with_key, ...) {
  sv <- mirt(data_with_key$Data, 1, "nominal", pars = "values", ...)

  sv_new <- data_with_key$orig_levels %>% map(~ {

    # get the original key, side-assign as est
    est <- key <- attr(.x, "key")
    k <- length(key)

    # set TRUE to first non-correct response (this will be "lowest" category)
    est[!est][1L] <- TRUE

    # write as est attribute, this we'll use as est in starting values
    attr(.x, "est") <- !est

    # allocate vector for values with .5 for all
    value <- rep_len(.5, k)

    # for correct response, set k - 1
    value[key & est] <- k - 1

    # for arbitrary low category (first disctractor), set 0
    value[!key & est] <- 0

    # write resulting values vector as an attribute
    attr(.x, "value") <- value

    .x
  })

  # set est and values to our new constrains
  sv$est[grepl("ak", sv$name)] <- sv_new %>%
    map(attr, "est") %>%
    unlist(use.names = FALSE)

  sv$value[grepl("ak", sv$name)] <- sv_new %>%
    map(attr, "value") %>%
    unlist(use.names = FALSE)

  sv
}



#' BLIS S4 class
#'
#' Extends `mirt`'s `SingleGroupClass` directly (meaning all `mirt` methods that
#' work with that class will work with [BlisClass-class] too; make sure `mirt` is
#' loaded).
#'
#' The purpose of the class is to have a custom `coef` method  (see
#' [coef,BlisClass-method]) dispatched and the original levels with correct
#' response (as a `key` attribute) stored in the resulting fitted model.
#'
#' @slot orig_levels *list* of original levels with logical attribute `key`,
#'   which stores the information on which response (level) has been considered
#'   as correct. Note that levels not used in the original data are dropped.
#'
#' @importFrom methods setClass
#'
#' @family BLIS/BLIRT related
#'
#' @export
setClass("BlisClass",
  contains = "SingleGroupClass",
  slots = c(orig_levels = "list")
)


#' Get Original Levels from a Fitted BLIS model
#'
#' Just a simple accessor to original levels and correct key stored in fitted
#' BLIS model.
#'
#' @param object *object of class [BlisClass-class]*, model fitted via `fit_blis()` or
#'   `blis()`.
#'
#' @export
#' @importFrom methods is
#'
#' @family BLIS/BLIRT related
#' @return *list* of the original levels and correct key. Key is stored as an
#'   attribute `key` for every individual item.
#'
#' @examples
#' fit <- fit_blis(HCItest[, 1:20], HCIkey)
#' get_orig_levels(fit)
get_orig_levels <- function(object) {
  if (!is(object, "BlisClass")) stop("Object provided is not of class [BlisClass-class].")
  object@orig_levels
}

#' Get Coefficients from a fitted BLIS model
#'
#' @param object *object of class [BlisClass-class]*, model fitted via `fit_blis`() or
#'   `blis()`.
#' @param ... Additional arguments. Not utilized at the moment.
#' @param CI *numeric*, a width of the confidence intervals.
#' @param printSE *logical*, print standard errors instead of CI? Defaults to
#'   `FALSE`.
#' @param IRTpars *logical*, convert slope intercept parameters into IRT
#'   parameters (i.e. BLIRT)? Defaults to `FALSE`.
#' @param simplify *logical*, return coefficients as a matrix, instead of list?
#'   Defaults to `FALSE`. *Not implemented yet.*
#' @param labels *logical*, if `TRUE`, show response labels (e.g. "A", "B", "C")
#'   instead of response numeric indices (e.g. 0, 1, 2). Defaults to `FALSE`.
#' @param mark_correct *logical*, mark the correct response with an asterisk
#'   symbol. Applicable only if `labels` is `TRUE` (in which case,
#'   `mark_correct` defaults to `TRUE`).
#'
#' @return coefs
#'
#' @examples "todo"
#' @importFrom methods setMethod
#' @importFrom purrr map map2
#'
#' @family BLIS/BLIRT related
#'
#' @export
setMethod(
  "coef", "BlisClass",
  function(object, ..., CI = .95, printSE = FALSE, IRTpars = FALSE,
           simplify = FALSE, labels = FALSE,
           mark_correct = labels) {

    # not-implemented args warnings
    if (!missing(simplify)) {
      warning(
        "`simplify` has not been implemented yet and will have no effect.",
        call. = FALSE
      )
    }

    # assert CI is in proper bounds
    if (CI >= 1 || CI <= 0) {
      stop("CI must lie between 0 and 1.", call. = FALSE)
    }

    # friendly message
    if (!labels && mark_correct) {
      warning("You have to opt for labels printing to `mark_correct` take any effect.",
        call. = FALSE
      )
    }

    # reparametrize fitted BLIS if IRT parametrization is requested
    if (IRTpars) object <- blis2blirt(object)

    item_names <- colnames(object@Data$data)
    pars <- object@ParObjects$pars[seq_along(item_names)] # parameters list from fitted object
    has_ses <- length(pars[[1L]]@SEpar) # evaluates to TRUE if SEs are available
    nms <- "par" # initial items list rowname, appends SE or LCI and UCI if needed below

    # TODO: refactor this
    out <- if (has_ses) {
      # print SEs or CIs?
      if (printSE) {
        # print SEs
        nms <- c(nms, "SE")
        map(
          pars,
          ~ matrix(
            c(.x@par[-1L], .x@SEpar[-1L]), # [-1L] strips out the overall slope parameter a*
            ncol = length(.x@par[-1L]), byrow = TRUE,
            dimnames = list(nms, .x@parnames[-1L])
          )
        )
      } else {
        # print CIs
        p <- (1 - CI) / 2
        nms <- c(nms, paste0("CI_", c(p, p + CI) * 100))
        z <- abs(qnorm(p))

        map(
          pars,
          ~ matrix(
            c(
              .x@par[-1L],
              c(
                .x@par[-1L] - z * .x@SEpar[-1L],
                .x@par[-1L] + z * .x@SEpar[-1L]
              )
            ),
            ncol = length(.x@par[-1L]), byrow = TRUE,
            dimnames = list(nms, .x@parnames[-1L])
          )
        )
      }
    } else {
      # print pars only
      map(
        pars,
        ~ matrix(
          .x@par[-1L],
          nrow = 1, byrow = TRUE,
          dimnames = list(nms, .x@parnames[-1L])
        )
      )
    }

    names(out) <- item_names

    if (labels) {
      out <- map2(
        out, object@orig_levels,
        ~ {
          if (mark_correct) {
            corr_resp <- attr(.y, "key", exact = TRUE)
            .y[corr_resp] <- paste0(.y[corr_resp], "*")
          }
          colnames(.x) <- c(paste0("a_", .y), paste0("d_", .y))
          .x
        }
      )
    }

    # GroupPars (mean, cov) are not included,
    # as we don't support multigroup nor multidimensional models so far

    structure(out, class = "blis_coefs") # for print method
  }
)


#' Print method for BLIS coefficients
#'
#' @param x result of `coef()`.
#' @param ... Additional arguments passed on to `print()`.
#' @param digits *integer*, number of digits to show in the output. Note that
#'   printed object are still an original list, which does not round any value
#'   (it is returned invisibly).
#'
#' @importFrom purrr modify
#'
#' @family BLIS/BLIRT related
#'
#' @export
print.blis_coefs <- function(x, digits = 3, ...) {
  x <- modify(x, ~ round(.x, digits))
  class(x) <- "list" # do not stuck in infinite loop of printing blis coefs
  print(x, ...)
  invisible(x) # still, return raw object invisibly
}


#' Reparametrize fitted BLIS model to IRT
#'
#' @param fitted_model fitted BLIS model (object of class [BlisClass-class])
#'
#' @return IRT parametrization of BLIS model
#'
#' @keywords internal
#'
#' @importFrom purrr modify_if map map2_dbl
#' @importFrom stats setNames
#'
blis2blirt <- function(fitted_model) {
  has_ses <- length(fitted_model@ParObjects$pars[[1L]]@SEpar) # evaluates to TRUE if SEs are available
  if (has_ses) vcov <- fitted_model@vcov

  fitted_model@ParObjects$pars <- modify_if(
    fitted_model@ParObjects$pars, ~ inherits(.x, "nominal"),
    function(it) {
      if (has_ses) {
        # get pars that were actually estimated (estimated == present in vcov matrix)
        est_pars <- it@par[it@est]

        # get parnums and parname of estimated pars
        est_parnums <- it@parnum[it@est]
        est_parnames <- it@parnames[it@est]

        # compose names that the vcov matrix uses, for later subsetting
        vcov_nms <- paste(est_parnames, est_parnums, sep = ".")

        first_half_idx <- seq_len(it@ncat - 1) # indices for first half of the par vector

        grads <- map(first_half_idx, ~ {
          # get intercept and slope for each response
          int_slope <- est_pars[c(.x + it@ncat - 1, .x)]
          # get partial derivatives / gradients
          matrix(
            c(
              -(1 / int_slope[2L]),
              int_slope[1L] / int_slope[2L]^2
            ),
            nrow = 1
          )
        })

        # get vcov subsets for each response
        vcov_subs <- map(first_half_idx, ~ {
          idx <- vcov_nms[c(.x + it@ncat - 1, .x)]
          vcov[idx, idx]
        })

        # compute new SEs
        new_ses <- map2_dbl(
          grads, vcov_subs,
          ~ sqrt(diag(.x %*% .y %*% t(.x)))
        )

        # name them, so we can replace the original SEs by name
        names(new_ses) <- est_parnames[it@ncat:(2 * (it@ncat - 1))]

        # name original SES
        it@SEpar <- setNames(it@SEpar, it@parnames)

        # replace with new ones
        it@SEpar[names(new_ses)] <- new_ses
      }

      # transform intercepts to IRT
      it@par[1 + (it@ncat + 1):(2 * it@ncat)] <- -it@par[1 + (it@ncat + 1):(2 * it@ncat)] / it@par[1:it@ncat + 1]

      # replace NaN introduced by zero division with 0
      it@par[is.nan(it@par)] <- 0

      # use IRT nomenclature
      it@parnames <- c(
        "a*",
        paste0("a", seq_len(it@ncat)), paste0("b", seq_len(it@ncat))
      )

      return(it)
    }
  )
  return(fitted_model)
}
