#' Conduct Parallel Analysis
#'
#' Computes the eigenvalues of the sample correlation matrix and the eigenvalues
#' obtained from a random correlation matrix for which no factors/components are
#' assumed. By default, the function utilizes a modified Horn's (1965) method,
#' which -- instead of mean -- uses 95th percentile of each item eigenvalues
#' sampling distribution as a threshold to find the optimal number of
#' factors/components.
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
#' @param Data \emph{data.frame} or \emph{matrix}, dataset (where rows are
#'   observations and columns items) or correlation matrix (recognized
#'   automatically).
#' @param plot \emph{logical}, if \code{TRUE} (the default), show the plot along
#'   with the function results. To create the plot from the resulting object
#'   afterwards, call \code{plot()}.
#' @param method \emph{character}, either \code{fa}, \code{pca}, or \code{both}
#'   (the default). Which method to use for the eigenvalues simulation and
#'   computation.
#' @param cor \emph{character}, how to calculate the correlation matrix of the
#'   real data. Can be either \code{pearson} (default), \code{tetrachoric} or
#'   \code{polychoric}. Unambiguous abbreviations accepted.
#' @param n_obs \emph{integer}, in case you provided the correlation matrix
#'   directly as the input, you have to provide the number of observations in
#'   the original dataset.
#' @param threshold \emph{character}, whether to use traditionall Horn's method
#'   or more recent and well-performing quantile one. Either \code{mean} or
#'   \code{quantile} (default). Can be abbreviated.
#' @param p \emph{numeric} (0--1), probability for which the sample quantile is
#'   produced. Defaults to \code{.95}. Ignored if \code{threshold = "mean"}.
#' @param n_iter \emph{integer}, number of iterations, i.e. the number of
#'   zero-factor multivariate normal distributions to sample. Defaults to
#'   \code{20}.
#' @param fm \emph{character}, factoring method. See \code{\link[psych]{fa}}
#'   from the package \code{\link[psych]{psych}}.
#' @param show_kaiser \emph{logical}, whether to show Kaiser boundary in the
#'   plot (the default) or not.
#' @inheritParams stats::cor
#' @inheritDotParams psych::polychoric -x -y -na.rm -polycor -std.err
#'
#' @return An object of class \code{data.frame} and \code{sia_parallel}. Can be
#'   plotted using \code{plot()}.
#'
#' @examples
#' data("TestAnxietyCor", package = "ShinyItemAnalysis")
#' fa_parallel(TestAnxietyCor, n_obs = 335, method = "pca")
#'
#' \dontrun{
#' data("bfi", package = "psych")
#' items <- bfi[, 1:25]
#'
#' fa_parallel(items)
#' fa_parallel(items, threshold = "mean") # traditional Horn's method
#' }
#'
#' @author
#' Jan Netik \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{netik@@cs.cas.cz}
#'
#' Patricia Martinkova \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{martinkova@@cs.cas.cz}
#'
#' @references
#' Horn, J. L. (1965). A rationale and test for the number of factors in factor
#' analysis. Psychometrika, 30, 179--185. \doi{10.1007/BF02289447}
#'
#' Buja, A., & Eyuboglu, N. (1992). Remarks on parallel analysis. Multivariate
#' Behavioral Research, 27, 509--540. \doi{10.1207/s15327906mbr2704_2}
#'
#' @encoding UTF-8
#'
#' @importFrom stats rnorm
#' @importFrom psych fa tetrachoric polychoric
#' @importFrom parallel mclapply
#'
#' @export
#'
fa_parallel <- function(Data, cor = "pearson", n_obs = NULL,
                        method = "pca",
                        threshold = "quantile", p = .95, n_iter = 20,
                        plot = TRUE, show_kaiser = TRUE,
                        fm = "minres", use = "pairwise", ...) {
  method <- match.arg(method, c("fa", "pca", "both"))
  method_vec <- if (method == "both") c("fa", "pca") else method

  # dims
  n_subj <- nrow(Data)
  n_vars <- ncol(Data)


  if (p < 0 || p > 1) {
    stop("Probability must lie between 0 and 1!", call. = FALSE)
  }


  # circumvent the correlation estimation when Data is recognized as a corr. already
  if (is_corr(Data)) {
    if (is.null(n_obs)) {
      stop(c(
        "You have to specify the number of observations of the original ",
        "dataset when using a correlation matrix directly as the input."
      ), call. = FALSE)
    }

    message("The input was recognized as a correlation matrix.\nAssuming ", n_obs, " observations in the original data.")

    data_corr <- Data

    n_subj <- n_obs # needed for random-valued matrix simulation
  } else {

    # real data part
    data_corr <- switch(match.arg(cor, c("pearson", "tetrachoric", "polychoric")),
      "pearson" = cor(Data, use = use),
      "polychoric" = tryCatch(polychoric(Data, na.rm = TRUE, ...)$rho,
        error = function(e) {
          stop(paste(
            "Calculation of polychoric correlations returned an error:\n", e
          ),
          call. = FALSE
          )
        }
      ),
      "tetrachoric" = tryCatch(tetrachoric(Data, na.rm = TRUE, ...)$rho,
        error = function(e) {
          if (max(Data, na.rm = TRUE) > 1) {
            stop("Tetrachoric correlation requires dichotomous data.", # typo in original psych error
              call. = FALSE
            )
          } else {
            stop(e)
          }
        }
      )
    )
  }

  if ("fa" %in% method_vec) {
    data_eigen_fa <- fa(data_corr, fm = fm, warnings = FALSE)$values
  }

  if ("pca" %in% method_vec) {
    data_eigen_pca <- eigen(data_corr, symmetric = TRUE, only.values = TRUE)$values
  }


  # simulated part
  # corr matrices from p-variate normal random matrices - for both pca and fa
  sim_corr <- mclapply(seq_len(n_iter), function(x) {
    sim_data <- matrix(rnorm(n_subj * n_vars), nrow = n_subj, ncol = n_vars)
    cor(sim_data, use = "everything")
  })

  if ("fa" %in% method_vec) {
    sim_eigen_list_fa <- mclapply(sim_corr, function(x) {
      fa(x, rotate = "none", warnings = FALSE)$values
    })
  }

  if ("pca" %in% method_vec) {
    sim_eigen_list_pca <- mclapply(sim_corr, function(x) {
      eigen(x, symmetric = TRUE, only.values = TRUE)$values
    })
  }

  #  get eigenvals function
  get_eigenvals <- function(sim_eigen_list, threshold, n_iter, p) {
    sim_eigen_df <- t(matrix(unlist(sim_eigen_list), ncol = n_iter))

    switch(match.arg(threshold, c("mean", "quantile")),
      mean = apply(sim_eigen_df, 2, mean),
      quantile = apply(sim_eigen_df, 2, function(x) quantile(x, p))
    )
  }

  if ("fa" %in% method_vec) {
    sim_eigen_fa <- get_eigenvals(sim_eigen_list_fa, threshold, n_iter, p)
  }

  if ("pca" %in% method_vec) {
    sim_eigen_pca <- get_eigenvals(sim_eigen_list_pca, threshold, n_iter, p)
  }


  out <- expand.grid(
    fact_or_comp = seq_len(n_vars),
    data_type = c("real", "simulated"),
    method = method_vec
  )

  out[["eigenvalue"]] <- switch(method,
    "fa" = c(data_eigen_fa, sim_eigen_fa),
    "pca" = c(data_eigen_pca, sim_eigen_pca),
    "both" = c(data_eigen_fa, sim_eigen_fa, data_eigen_pca, sim_eigen_pca)
  )

  attr(out, "row.names") <- seq_len(nrow(out))
  attr(out, "class") <- c("sia_parallel", "data.frame")


  if ("fa" %in% method_vec) {
    factors_below_thr <- which(!(data_eigen_fa > sim_eigen_fa))
    n_factors <- max(factors_below_thr[1] - 1, 1)

    kaiser_fa <- sum(data_eigen_fa >= 0)
  }

  if ("pca" %in% method_vec) {
    comp_below_thr <- which(!(data_eigen_pca > sim_eigen_pca))
    n_comp <- max(comp_below_thr[1] - 1, 1)

    kaiser_pca <- sum(data_eigen_pca >= 1)
  }

  if (plot) {
    print(plot(out, show_kaiser = show_kaiser))
  }

  cat(
    "According to the parallel analysis, the optimal number of",
    switch(method,
      "fa" = paste0("factors is ", n_factors, "."),
      "pca" = paste0("principal components is ", n_comp, "."),
      "both" = paste0(
        "factors is ", n_factors,
        " and the optimal number of principal components is ", n_comp, "."
      )
    ), "\nFollowing the Kaiser rule,",
    switch(method,
      "fa" = paste0(
        kaiser_fa, ifelse(kaiser_fa > 1, " factors are", " factor is"),
        " recommended."
      ),
      "pca" = paste0(
        kaiser_pca, ifelse(kaiser_pca > 1, " components are", " component is"),
        " recommended."
      ),
      "both" = paste0(
        kaiser_fa, ifelse(kaiser_fa > 1, " factors", " factor"),
        " and ",
        kaiser_pca, ifelse(kaiser_pca > 1, " components are", " component is"),
        " recommended."
      )
    )
  )

  invisible(out)
}


#' Plot Method for Parallel Analysis Output
#'
#' You can call this method to plot an existing object resulting from
#' \code{fa_paralell()} function, which behaves as a standard \code{data.frame},
#' but can be automatically recognized and processed with a dedicated plot
#' method. Also, you can \emph{post-hoc} disable the Kaiser boundaries shown by
#' default.
#'
#' @param x object of class \code{sia_parallel} to plot.
#' @param y \emph{ignored}
#' @param ... additional argument:
#'   \describe{\item{\code{show_kaiser}}{\emph{logical}, whether to show
#'   horizonal lines denoting Kaiser boundaries (eigenvalue 0 and/or 1 for FA
#'   and/or PCA, respectively). Defaults to \code{TRUE}.}}
#'
#' @examples
#' \dontrun{
#' fa_parallel_result <- BFI2[, 1:60] %>% fa_parallel(plot = FALSE) # without plot
#' fa_parallel_result %>% plot # generate plot from "fitted" object
#' fa_parallel_result %>% plot(show_kaiser = FALSE) # hide Kaiser boundaries
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_hline annotate
#'   scale_x_continuous scale_color_manual theme_bw theme expansion element_line
#'   element_rect scale_alpha_manual xlab element_blank element_text
#'   coord_cartesian
#' @importFrom rlang .data
#'
#' @export
plot.sia_parallel <- function(x, y, ...) {
  max_fact <- max(x[["fact_or_comp"]])
  max_eigenvalue <- max(x[["eigenvalue"]])
  method <- if (nlevels(x[["method"]]) == 2) "both" else levels(x[["method"]])
  x[["method"]] <- toupper(x[["method"]])
  y_lim_max <- ifelse(max_eigenvalue < 1.5, 1.5, max_eigenvalue)


  kaiser_boundary <- function(method, show_kaiser = TRUE) {
    if (show_kaiser) {
      positions <- switch(method,
        "fa" = 0,
        "pca" = 1,
        "both" = c(0, 1)
      )
      labels <- switch(method,
        "fa" = "Kaiser boundary",
        "pca" = "Kaiser boundary",
        "both" = c("Kaiser boundary for FA", "Kaiser boundary for PCA")
      )

      list(
        geom_hline(yintercept = positions, linetype = "dashed", alpha = .4),
        annotate("text",
          x = max_fact, y = positions, label = labels, hjust = 1, vjust = -.75,
          alpha = .4
        )
      )
    }
  }

  x %>%
    ggplot(aes(.data$fact_or_comp, .data$eigenvalue,
      col = .data$method, alpha = .data$data_type
    )) +
    kaiser_boundary(method, ...) +
    geom_line() +
    geom_point() +
    scale_x_continuous(
      breaks = function(x) seq(1, x[2], 3),
      expand = expansion(add = .75)
    ) +
    scale_color_manual(
      values = c("#00BFC4", "#F8766D"),
      guide = ifelse(method == "both", "legend", "none")
    ) +
    scale_alpha_manual(values = c(1, .25)) +
    xlab(switch(method,
      "fa" = "factor number",
      "pca" = "component number",
      "both" = "factor/component number"
    )) +
    coord_cartesian(ylim = c(NA, y_lim_max)) +
    theme_bw(
      base_size = 15, base_family = ""
    ) +
    theme(
      panel.grid.major = element_blank(), # element_line(size = .4),
      panel.grid.minor = element_blank(), # element_line(size = .25),
      panel.border = element_rect(),
      legend.position = c(1, 1),
      legend.justification = c(1.2, 1.25),
      legend.box = "horizontal",
      legend.title = element_blank(),
      legend.background = element_blank()
    )
}
