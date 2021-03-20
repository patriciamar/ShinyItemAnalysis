#' Conduct Parallel Analysis
#'
#' Computes the eigenvalues of the sample correlation matrix and the eigenvalues
#' obtained from a random correlation matrix for which no factors are assumed.
#' By default, the function utilizes a modified Horn's (1965) method, which --
#' instead of mean -- uses 95th percentile of each item eigenvalues sampling
#' distribution as a threshold to find the optimal number of factors.
#'
#' Horn proposed a solution to the problem of optimal factor number
#' identification using an approach based on a Monte Carlo simulation.
#'
#' First, several (20 by default) zero-factor \code{p}-variate normal
#' distributions (where \code{p} is the number of columns) are obtained, and
#' \code{p} × \code{p} correlation matrices are computed for them. Eigenvalues
#' of each matrix is then calculated in order to get an eigenvalues sampling
#' distribution for each simulated variable.
#'
#' Traditionally, Horn obtains an average of each sampling distribution and
#' these averages are used as a threshold which is compared with eigenvalues of
#' the original, real data. However, \emph{usage of the mean was later disputed}
#' by Buja & Eyuboglu (1992), and 95th percentile of eigenvalues sampling
#' distribution was suggested as a more accurate threshold. This, more recent
#' method is used by default in the function.
#'
#' @param Data \emph{data.frame} or \emph{matrix}, dataset where rows are
#'   observations and columns items.
#' @param cor \emph{character}, how to calculate the correlation matrix of the
#'   real data. Can be either "pearson" (default), "tetrachoric" or
#'   "polychoric". Unambiguous abbreviations accepted.
#' @param method \emph{character}, whether to use traditionall Horn's method or
#'   more recent and well-performing quantile one. Either \code{mean} or
#'   \code{quantile} (default). Can be abbreviated.
#' @param p \emph{numeric} (0--1), probability for which the sample quantile is
#'   produced. Defaults to \code{.95}. Ignored if \code{method = "mean"}.
#' @param n_iter \emph{integer}, number of iterations, i.e. the number of
#'   zero-factor multivariate normal distributions to sample. Defaults to
#'   \code{20}.
#' @param fm \emph{character}, factoring method. See \code{\link[psych]{fa}}
#'   from the package \code{\link[psych]{psych}}.
#' @inheritParams stats::cor
#' @inheritDotParams psych::polychoric -x -y -na.rm -polycor -std.err
#'
#' @return An object of class \code{sia_parallel}. Can be coerced to
#'   \code{data.frame} or plotted with \code{plot()}.
#'
#' @examples
#' data("bfi", package = "psych")
#' items <- bfi[, 1:25]
#'
#' fa_parallel(items)
#' \dontrun{
#' fa_parallel(items, method = "mean") # traditional Horn's method, emulates psych
#'
#' fa_parallel(items) %>% plot()
#' fa_parallel(items) %>% as.data.frame()
#' }
#'
#' @author Jan Netik \cr Institute of Computer Science of the Czech Academy of
#'   Sciences
#'
#'   Patricia Martinkova \cr Institute of Computer Science of the Czech Academy
#'   of Sciences \cr \email{martinkova@@cs.cas.cz}
#'
#' @references Horn, J. L. (1965). A rationale and test for the number of
#'   factors in factor analysis. Psychometrika, 30, 179--185.
#'   http://dx.doi.org/10.1007/BF02289447
#'
#'   Buja, A., & Eyuboglu, N. (1992). Remarks on parallel analysis. Multivariate
#'   Behavioral Research, 27, 509--540. http://dx.doi.org/10.1207/
#'   s15327906mbr2704_2
#'
#' @encoding UTF-8
#' @importFrom stats rnorm
#' @importFrom psych fa tetrachoric polychoric
#' @importFrom parallel mclapply
#' @export
fa_parallel <- function(Data, cor = "pearson", method = "quantile", p = .95, n_iter = 20,
                        fm = "minres", use = "pairwise", ...) {
  n_subj <- nrow(Data)
  n_vars <- ncol(Data)

  if (p < 0 || p > 1) {
    stop("Probability must be between 0 and 1!", call. = FALSE)
  }

  cor <- match.arg(cor, c("pearson", "tetrachoric", "polychoric"))

  # simulated part
  sim_eigen_list <- mclapply(seq_len(n_iter), function(XX) {
    sim_data <- matrix(rnorm(n_subj * n_vars), nrow = n_subj, ncol = n_vars)

    sim_corr <- cor(sim_data, use = "everything")

    fa(sim_corr, fm = fm, rotate = "none", warnings = FALSE)$values
  })

  sim_eigen_df <- t(matrix(unlist(sim_eigen_list), ncol = n_iter))
  sim_eigen <- switch(
    match.arg(method, c("mean", "quantile")),
    mean = apply(sim_eigen_df, 2, mean),
    quantile = apply(sim_eigen_df, 2, function(x) {
      quantile(x, p)
    })
  )

  # real part
  data_corr <- switch(cor,
    "pearson" = cor(Data, use = use),
    "polychoric" = tryCatch(polychoric(Data, na.rm = TRUE, ...)$rho,
      error = function(e) {
        stop(
          "Your items have more than 8 response categories, use of polychoric corr. is discouraged.\n",
          "Choose different correlation or stick with polychoric by specifying `max.cat = n`,\n",
          "where `n` is greater than the number of response categories of your items.",
          call. = FALSE
        )
      }
    ),
    "tetrachoric" = tryCatch(tetrachoric(Data, na.rm = TRUE, ...)$rho,
      error = function(e) {
        if (max(Data, na.rm = TRUE) > 1) {
          stop("Tetrachoric correlation requires dichotomous data.",
            call. = FALSE
          )
        } else {
          stop(e)
        }
      }
    )
  )

  data_eigen <- fa(data_corr, fm = fm, warnings = FALSE)$values

  out <- list()
  out[["factor"]] <- c(seq_along(data_eigen), seq_along(sim_eigen))
  out[["data"]] <- factor(c(
    rep("real", length(data_eigen)), rep("simulated", length(sim_eigen))
  ))
  out[["eigenvalue"]] <- c(data_eigen, sim_eigen)

  attr(out, "class") <- "sia_parallel"

  out
}


#' @importFrom rlang inform
#' @export
print.sia_parallel <- function(x, ...) {
  real_idx <- which(x$data == "real")
  simulated_idx <- which(x$data == "simulated")

  real_eigenvalues <- x$eigenvalue[real_idx]
  simulated_eigenvalues <- x$eigenvalue[simulated_idx]

  factors_below_thr <- which(!(real_eigenvalues > simulated_eigenvalues))

  n_factors <- max(factors_below_thr[1] - 1, 1)

  kaiser <- sum(real_eigenvalues >= 1)

  cat(
    "According to the parallel analysis, the optimal number of factors is ",
    n_factors, ".\n",
    "Following the Kaiser rule, ", kaiser,
    ifelse(kaiser > 1, " factors", " factor"), " is recommended.",
    sep = ""
  )

  inform(paste0(
    "\n\nFor the plot, pass the result of 'fa_paralell()' to 'plot()'.\n",
    "To see the underlying data, use 'as.data.frame()'."
  ), .frequency = "regularly", .frequency_id = "si_parallel_hint")
}


#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_hline annotate
#'   scale_x_continuous theme_bw theme expansion element_line element_rect
#'   element_blank element_text coord_cartesian
#' @importFrom rlang .data
#' @export
plot.sia_parallel <- function(x, y, ...) {
  max_fact <- max(x[["factor"]])
  max_eigenvalue <- max(x[["eigenvalue"]])

  y_lim_max <- ifelse(max_eigenvalue < 1.5, 1.5, max_eigenvalue)

  x %>%
    as.data.frame() %>%
    ggplot(aes(.data$factor, .data$eigenvalue, col = .data$data)) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 1, linetype = "dashed", alpha = .5) +
    annotate("text",
      x = max_fact, y = 1, label = "Kaiser boundary", hjust = 1, vjust = 1.5,
      alpha = .5
    ) +
    scale_x_continuous(
      breaks = function(x) seq(1, x[2], 3),
      expand = expansion(add = .75)
    ) +
    theme_bw(
      base_size = 15, base_family = ""
    ) +
    coord_cartesian(ylim = c(NA, y_lim_max)) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.major = element_line(size = .4),
      panel.grid.minor = element_line(size = .25),
      panel.border = element_rect(),
      legend.position = c(1, 1),
      legend.justification = c(1.2, 1.25),
      legend.title = element_blank(),
      legend.background = element_blank(),
      legend.key = element_blank()
    )
}


#' @export
as.data.frame.sia_parallel <- function(x, row.names = NULL, optional = FALSE, ...) {
  x <- list(
    factor = x[["factor"]],
    data = x[["data"]],
    eigenvalue = x[["eigenvalue"]]
  )

  attr(x, "row.names") <- seq_len(length(x[[1]]))
  class(x) <- "data.frame"

  x
}
