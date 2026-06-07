# ============================================================================ #
# IRT – Graded Response Model (GRM)                                            #
# ============================================================================ #

# DATA -----------------------------------------------------------------------

IRT_grm_data <- reactive({
  ordinal() |> modify(as.integer) # ordinal data as integers for mirt
})


# PARAMETRIZATION ------------------------------------------------------------

# Map UI choice to IRTpars argument for mirt::coef()
IRT_grm_is_irt <- reactive({
  input$IRT_grm_parametrization == "irt"
})


# MODEL FIT ------------------------------------------------------------------

IRT_grm_fit <- reactive({
  suppressMessages(mirt(
    IRT_grm_data(),
    model = 1,
    itemtype = "graded",
    SE = TRUE,
    verbose = FALSE,
    technical = list(NCYCLES = input$ncycles)
  ))
}) |>
  bindCache(IRT_grm_data(), input$ncycles) |>
  bindEvent(IRT_grm_data(), input$ncycles)


# COEFFICIENT TABLE ----------------------------------------------------------
# Helper: extract coefficients with valid SEs for GRM, GPCM, PCM, and RSM.
#
# mirt stores SEs in the estimation space (intercept-slope for GRM/GPCM/PCM;
# a native IRT-like space for RSM).  Depending on the model, we either:
#   - Transform d-parameters to IRT b-parameters via delta method (GRM/GPCM/PCM)
#   - Use the raw mirt output directly, computing threshold SEs via delta method
#     from b1 and c_k (RSM)
#
# RSM raw output columns:  a1 (fixed=1), b1 (item location), c2, c3, … (offsets)
#   Absolute thresholds:  tau_k = b1 + c_k  (c1 = 0 fixed, so tau_1 = b1)
#   SE(tau_1) = SE(b1)
#   SE(tau_k) = sqrt( SE(b1)^2 + SE(c_k)^2 )   [b1 and c_k independent]
#
# GRM/GPCM/PCM raw output columns:  a1, d1, d2, …  (d0 = 0 fixed, dropped)
#   b_k = (d_{k-1} - d_k) / a1  (d_0 = 0),  equals crossover of adjacent ICCs
#   SE(b_k) via delta method on consecutive d-parameter differences
#   When a1 is fixed (PCM), SE(a1) = 0 → simplified SE formula
.ordinalIRT_coef_with_se <- function(fit, irt_pars) {
  raw <- coef(fit, printSE = TRUE, IRTpars = FALSE)
  raw[["GroupPars"]] <- NULL

  # Detect RSM by presence of a "b1" column (item location) in raw output
  is_rsm <- "b1" %in% colnames(raw[[1L]])

  # ---- RSM branch ----------------------------------------------------------
  if (is_rsm) {
    # Raw already contains IRT-meaningful parameters: a1 (fixed), b1, c2, c3, …
    # We compute absolute threshold parameters tau_k = b1 + c_k for display.
    # SE(tau_k) via delta method (b1 and c_k are independent model parameters):
    #   SE(tau_1) = SE(b1)
    #   SE(tau_k) = sqrt( SE(b1)^2 + SE(c_k)^2 ),  k >= 2

    lapply(raw, function(mat) {
      # Extract rows as plain named vectors — avoids matrix-subsetting quirks
      se_row <- mat["SE", , drop = TRUE]
      par_row <- mat["par", , drop = TRUE]

      # mirt RSM convention (verified empirically):
      #   b1, b2, … = free threshold offsets lambda_1, lambda_2, …
      #               (lambda_0 = 0 is fixed and absent from raw output)
      #   c, c2, …  = item-specific locations b_i
      #               (first item fixed to 0, SE = NA)
      #
      # Display order: item locations b_i first, then lambda_k

      # --- item locations (c columns) -----------------------------------------
      # mirt stores c = -b_i (intercept convention), so b_i = -c
      rsm_c_cols <- grep("^c\\d*$", names(par_row), value = TRUE)
      rsm_c_vals <- unname(par_row[rsm_c_cols]) # raw c = -b_i
      rsm_b_vals <- -rsm_c_vals # b_i = -c
      rsm_se_b <- unname(se_row[rsm_c_cols]) # SE same sign-invariant
      rsm_n_b <- length(rsm_c_cols)

      # --- threshold offsets (b columns) --------------------------------------
      # Keep the digit suffix to use as lambda index (b1 -> lambda_1, etc.)
      rsm_b_cols <- grep("^b\\d+$", names(par_row), value = TRUE)
      rsm_lam_idx <- as.integer(sub("^b", "", rsm_b_cols)) # 1, 2, …
      rsm_lam_vals <- unname(par_row[rsm_b_cols])
      rsm_se_lam <- unname(se_row[rsm_b_cols])

      if (irt_pars) {
        # IRT: b_i (item locations, negated from c) | lambda_k (threshold offsets)
        # Crossover for item i at category k: lambda_k + b_i
        matrix(
          c(rsm_b_vals, rsm_lam_vals, rsm_se_b, rsm_se_lam),
          nrow = 2,
          byrow = TRUE,
          dimnames = list(
            c("par", "SE"),
            c(paste0("b", seq_len(rsm_n_b)), paste0("lambda", rsm_lam_idx))
          )
        )
      } else {
        # Intercept-slope: beta_{i0} = -b_i = c | beta_{tk} = -lambda_k
        matrix(
          c(rsm_c_vals, -rsm_lam_vals, rsm_se_b, rsm_se_lam),
          nrow = 2,
          byrow = TRUE,
          dimnames = list(
            c("par", "SE"),
            c(
              paste0("betab", seq_len(rsm_n_b)),
              paste0("betalambda", rsm_lam_idx)
            )
          )
        )
      }
    })

    # ---- GRM / GPCM / PCM branch ---------------------------------------------
  } else {
    # Drop fixed zero-constraint d columns (par == 0 & SE is NA), e.g. d0
    drop_fixed_d <- function(mat) {
      d_cols <- grep("^d\\d+$", colnames(mat), value = TRUE)
      is_fixed <- vapply(
        d_cols,
        function(cn) {
          isTRUE(as.numeric(mat["par", cn]) == 0) && is.na(mat["SE", cn])
        },
        logical(1L)
      )
      mat[, c("a1", d_cols[!is_fixed]), drop = FALSE]
    }

    if (!irt_pars) {
      return(lapply(raw, drop_fixed_d))
    }

    # Use mirt's IRTpars=TRUE for correct step parameter point estimates.
    # The naive formula b_k = -d_k/a1 is wrong for k > 1.
    # Correct formula: b_k = (d_{k-1} - d_k) / a1, d_0 = 0.
    # These equal the crossover points of adjacent ICC curves.
    irt_raw <- coef(fit, printSE = FALSE, IRTpars = TRUE)
    irt_raw[["GroupPars"]] <- NULL

    mapply(
      function(mat_raw, mat_irt) {
        mat_raw <- drop_fixed_d(mat_raw)

        a1 <- mat_raw["par", "a1"]
        se_a1_est <- if (is.na(mat_raw["SE", "a1"])) 0 else mat_raw["SE", "a1"]
        se_a1_out <- if (se_a1_est == 0) NA_real_ else se_a1_est

        d_cols <- grep("^d\\d+$", colnames(mat_raw), value = TRUE)
        d_vals <- mat_raw["par", d_cols]
        se_d <- mat_raw["SE", d_cols]

        # Point estimates from IRTpars
        b_cols <- grep("^b\\d+$", colnames(mat_irt), value = TRUE)
        b_vals <- mat_irt["par", b_cols]

        # Delta-method SEs for b_k = (d_{k-1} - d_k) / a1, d_0 = 0
        d_all <- c(0, unname(d_vals))
        se_all <- c(0, unname(se_d))
        se_b <- vapply(
          seq_along(b_cols),
          function(k) {
            dk_prev <- d_all[k]
            dk <- d_all[k + 1L]
            se_prev <- se_all[k]
            se_k <- se_all[k + 1L]
            sqrt(
              (se_prev / a1)^2 +
                (se_k / a1)^2 +
                ((dk_prev - dk) * se_a1_est / a1^2)^2
            )
          },
          numeric(1L)
        )

        b_names <- paste0("b", seq_along(b_cols))
        matrix(
          c(a1, unname(b_vals), se_a1_out, se_b),
          nrow = 2,
          byrow = TRUE,
          dimnames = list(c("par", "SE"), c("a1", b_names))
        )
      },
      raw,
      irt_raw,
      SIMPLIFY = FALSE
    )
  }
}


IRT_grm_summary_coef <- reactive({
  is_irt <- IRT_grm_is_irt()

  coefs <- .ordinalIRT_coef_with_se(IRT_grm_fit(), irt_pars = is_irt)

  # Build tidy tibble --------------------------------------------------------
  coef_tbl <- coefs |>
    map_dfr(~ as_tibble(.x, rownames = "type"), .id = "item")

  # Rename columns to display-friendly math names ----------------------------
  # intercept-slope: a1 → beta_1,  d1 d2 … → beta_0_1 beta_0_2 …
  # IRT:             a1 → a,       b1 b2 … → b_1 b_2 …
  par_cols <- setdiff(colnames(coef_tbl), c("item", "type"))

  if (is_irt) {
    # a_i, b_{i1}, b_{i2}, …
    renamed <- par_cols |>
      str_replace("^a1$", "ai") |>
      str_replace("^b(\\d+)$", "bi\\1")
  } else {
    # beta_{1i}, beta_{0i1}, beta_{0i2}, …
    renamed <- par_cols |>
      str_replace("^a1$", "beta1i") |>
      str_replace("^d(\\d+)$", "beta0i\\1")
  }
  names(coef_tbl)[names(coef_tbl) %in% par_cols] <- renamed
  par_cols_new <- renamed

  # Pivot to wide "zig-zag" (par columns then SE columns) -------------------
  coef_wide <- coef_tbl |>
    pivot_wider(
      id_cols = item,
      names_from = type,
      values_from = all_of(par_cols_new),
      names_glue = "{if_else(type == 'par', '', type)}_{.value}"
    ) |>
    column_to_rownames("item")

  # Pretty-print column headers with LaTeX math ------------------------------
  # Internal token -> LaTeX:
  #   IRT:          ai -> a_i,  bi1 -> b_{i1},  bi2 -> b_{i2}, …
  #   int-slope:    beta1i -> beta_{1i},  beta0i1 -> beta_{0i1}, …
  colnames(coef_wide) <- colnames(coef_wide) |>
    map_chr(
      ~ {
        col <- .x
        is_se <- startsWith(col, "SE_")
        inner <- sub("^(SE_|_)", "", col)
        lx <- if (inner == "ai") {
          "\\(\\mathit{a_i}\\)"
        } else if (startsWith(inner, "bi")) {
          k <- sub("^bi", "", inner)
          paste0("\\(\\mathit{b_{i", k, "}}\\)")
        } else if (inner == "beta1i") {
          "\\(\\mathit{\\beta_{1i}}\\)"
        } else if (startsWith(inner, "beta0i")) {
          k <- sub("^beta0i", "", inner)
          paste0("\\(\\mathit{\\beta_{0i", k, "}}\\)")
        } else {
          paste0("\\(\\mathit{", inner, "}\\)") # fallback
        }
        if (is_se) paste0("SE(", lx, ")") else lx
      }
    )

  # S-X2 item fit statistics — only when no missing data
  if (!anyNA(IRT_grm_data())) {
    fit_stats <- tryCatch(
      suppressMessages(itemfit(
        IRT_grm_fit(),
        fit.statistics = "S_X2",
        na.rm = TRUE
      )),
      error = function(e) NULL
    )
    if (!is.null(fit_stats)) {
      sx2_tbl <- tibble(
        item = item_names(),
        SX2 = round(fit_stats$S_X2, 3),
        df = fit_stats$df.S_X2,
        p = round(fit_stats$p.S_X2, 3)
      ) |>
        column_to_rownames("item")
      colnames(sx2_tbl) <- c(
        "\\(S\\text{-}X^2\\)",
        "df",
        "\\(p\\)-value"
      )
      coef_wide <- bind_cols(coef_wide, sx2_tbl)
    }
  }

  coef_wide
}) |>
  bindCache(IRT_grm_data(), input$IRT_grm_parametrization, input$ncycles) |>
  bindEvent(IRT_grm_data(), input$IRT_grm_parametrization, input$ncycles)


# SUMMARY – IIC plot ---------------------------------------------------------

IRT_grm_summary_iic <- reactive({
  fit <- IRT_grm_fit()
  nms <- colnames(fit@Data$data)
  thetas <- IRT_thetas_for_plots()

  d <- map2_dfr(
    nms,
    item_names(),
    ~ tibble(
      Ability = thetas,
      Information = iteminfo(extract.item(fit, .x), thetas),
      Item = .y,
      label = paste0(
        "Ability = ",
        round(Ability, 3),
        "\n",
        "Information = ",
        round(Information, 3),
        "\n",
        "Item = ",
        Item
      )
    )
  )

  d |>
    ggplot(aes(Ability, Information, color = Item, group = Item)) +
    suppressWarnings(geom_line(aes(text = label))) +
    theme_app()
}) |>
  bindCache(IRT_grm_fit()) |>
  bindEvent(IRT_grm_fit())

IRT_grm_summary_iic_plotly <- reactive({
  g <- IRT_grm_summary_iic()
  p <- ggplotly(g, tooltip = "text")
  p$elementId <- NULL
  p |> plotly::config(displayModeBar = FALSE)
}) |>
  bindCache(IRT_grm_fit()) |>
  bindEvent(IRT_grm_fit())

output$IRT_grm_summary_iic <- renderPlotly({
  IRT_grm_summary_iic_plotly()
})

output$IRT_grm_summary_iic_download <- downloadHandler(
  filename = function() "fig_IRT_grm_IIC.png",
  content = function(file) {
    ggsave(
      file,
      plot = IRT_grm_summary_iic() +
        theme(
          text = element_text(size = setting_figures$text_size),
          legend.position = "right",
          legend.key.size = unit(0.8, "lines")
        ),
      device = "png",
      height = setting_figures$height,
      width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)


# SUMMARY – TIC plot ---------------------------------------------------------

IRT_grm_summary_tic <- reactive({
  fit <- IRT_grm_fit()
  thetas <- IRT_thetas_for_plots()

  tibble(
    Ability = thetas,
    Information = testinfo(fit, thetas),
    SE = 1 / sqrt(Information)
  ) |>
    ggplot(aes(x = Ability)) +
    geom_line(aes(y = Information, col = "info")) +
    geom_line(aes(y = SE, col = "se")) +
    scale_color_manual(
      values = c("blue", "pink"),
      labels = c("Information", "SE")
    ) +
    scale_y_continuous("Information", sec.axis = sec_axis(~., name = "SE")) +
    theme(axis.title.y.right = element_text(color = "pink")) +
    theme_app()
}) |>
  bindCache(IRT_grm_fit()) |>
  bindEvent(IRT_grm_fit())

IRT_grm_summary_tic_plotly <- reactive({
  g <- IRT_grm_summary_tic()
  p <- ggplotly(g)
  p$x$data[[1]]$text <- gsub("<br />colour: info", "", p$x$data[[1]]$text)
  p$x$data[[2]]$text <- gsub("<br />colour: se", "", p$x$data[[2]]$text)
  p$elementId <- NULL
  p |> plotly::config(displayModeBar = FALSE)
}) |>
  bindCache(IRT_grm_fit()) |>
  bindEvent(IRT_grm_fit())

output$IRT_grm_summary_tic <- renderPlotly({
  IRT_grm_summary_tic_plotly()
})

output$IRT_grm_summary_tic_download <- downloadHandler(
  filename = function() "fig_IRT_grm_TIC.png",
  content = function(file) {
    ggsave(
      file,
      plot = IRT_grm_summary_tic() +
        theme(
          text = element_text(size = setting_figures$text_size),
          legend.position = "right",
          legend.key.size = unit(0.8, "lines")
        ),
      device = "png",
      height = setting_figures$height,
      width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)


# SUMMARY – coefficient table ------------------------------------------------

output$IRT_grm_summary_coef <- renderTable(
  IRT_grm_summary_coef(),
  rownames = TRUE,
  striped = TRUE,
  na = ""
)

output$IRT_grm_summary_coef_download <- downloadHandler(
  filename = function() "IRT_grm_coefs.csv",
  content = function(file) {
    tab <- IRT_grm_summary_coef()
    names(tab) <- names(tab) |>
      str_remove_all("\\\\\\(\\\\mathit\\{\\\\") |>
      str_remove_all("\\\\\\)") |>
      str_remove_all("[{}}]")
    write.csv(tab, file)
  }
)


# SUMMARY – ability estimates ------------------------------------------------

IRT_grm_summary_fscores <- reactive({
  fit <- IRT_grm_fit()
  fs <- fscores(fit, full.scores.SE = TRUE)
  colnames(fs) <- c("F-score", "SE(F-score)")
  tab <- data.frame(
    `Total score` = total_score(),
    `Z-score` = z_score(),
    `T-score` = t_score(),
    fs,
    check.names = FALSE
  )
  rownames(tab) <- paste("Respondent", seq_len(nrow(tab)))
  tab
}) |>
  bindCache(IRT_grm_fit()) |>
  bindEvent(IRT_grm_fit())

output$IRT_grm_summary_ability <- renderTable(
  head(IRT_grm_summary_fscores(), 6),
  rownames = TRUE
)

output$IRT_grm_summary_ability_download <- downloadHandler(
  filename = function() "IRT_grm_abilities.csv",
  content = function(file) write.csv(IRT_grm_summary_fscores(), file)
)

IRT_grm_ability_cor <- reactive({
  tab <- IRT_grm_summary_fscores()
  cor(tab[["F-score"]], tab[["Z-score"]], use = "pairwise.complete.obs")
}) |>
  bindCache(IRT_grm_fit()) |>
  bindEvent(IRT_grm_fit())

output$IRT_grm_summary_ability_correlation_text <- renderText({
  paste0(
    "This scatterplot shows the relationship between the standardized total ",
    "score (Z-score) and the factor score estimated by the IRT model. The ",
    "Pearson correlation coefficient between these two scores is ",
    sprintf("%.3f", IRT_grm_ability_cor()),
    "."
  )
})

IRT_grm_summary_ability_plot <- reactive({
  df <- IRT_grm_summary_fscores()
  ggplot(df, aes(`Z-score`, `F-score`)) +
    geom_point(size = 3) +
    labs(x = "Standardized total score", y = "Factor score") +
    theme_app()
}) |>
  bindCache(IRT_grm_fit()) |>
  bindEvent(IRT_grm_fit())

IRT_grm_summary_ability_plotly <- reactive({
  g <- IRT_grm_summary_ability_plot()
  p <- ggplotly(g)
  p$elementId <- NULL
  p |> plotly::config(displayModeBar = FALSE)
}) |>
  bindCache(IRT_grm_fit()) |>
  bindEvent(IRT_grm_fit())

output$IRT_grm_summary_ability_plot <- renderPlotly({
  IRT_grm_summary_ability_plotly()
})

output$IRT_grm_summary_ability_plot_download <- downloadHandler(
  filename = function() "fig_IRT_grm_abilities.png",
  content = function(file) {
    ggsave(
      file,
      plot = IRT_grm_summary_ability_plot() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height,
      width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)


# ITEMS – slider update ------------------------------------------------------

observe({
  updateSliderInput(
    session = session,
    inputId = "IRT_grm_items",
    max = ncol(IRT_grm_data())
  )
})


# ITEMS – shared colour palette ----------------------------------------------
# Max categories across all items; category k always gets the same colour.
# Consistent across items and between ICC and CPC plots.

IRT_grm_max_cats <- reactive({
  fit <- IRT_grm_fit()
  max(sapply(seq_len(fit@Data$nitems), function(i) extract.item(fit, i)@ncat))
})

IRT_grm_cat_colours <- reactive({
  gg_color_hue(IRT_grm_max_cats())
})


# ITEMS – ICC ----------------------------------------------------------------

IRT_grm_items_icc <- reactive({
  item       <- input$IRT_grm_items
  fit        <- IRT_grm_fit()
  thetas     <- IRT_thetas_for_plots()
  n_cats     <- extract.item(fit, item)@ncat
  colours    <- IRT_grm_cat_colours()[seq_len(n_cats)]
  cat_labels <- paste0("Category ", seq_len(n_cats) - 1L)

  probs <- as_tibble(probtrace(extract.item(fit, item), thetas))
  names(probs) <- cat_labels

  probs |>
    bind_cols(theta = thetas) |>
    pivot_longer(-theta, names_to = "Category", values_to = "Probability") |>
    mutate(
      Category = factor(Category, levels = cat_labels),
      label = paste0(
        "Ability = ",
        round(theta, 3),
        "\n",
        "Probability = ",
        round(Probability, 3),
        "\n",
        Category
      )
    ) |>
    ggplot(aes(theta, Probability, color = Category, group = Category)) +
    suppressWarnings(geom_line(aes(text = label))) +
    scale_color_manual(values = setNames(colours, cat_labels)) +
    labs(
      x = "Ability",
      y = "Category probability",
      title = item_names()[item],
      color = "Category"
    ) +
    coord_cartesian(ylim = c(0, 1)) +
    theme_app()
})

output$IRT_grm_items_icc <- renderPlotly({
  g <- IRT_grm_items_icc()
  p <- ggplotly(g, tooltip = "text")
  p$elementId <- NULL
  p |> plotly::config(displayModeBar = FALSE)
})

output$IRT_grm_items_icc_download <- downloadHandler(
  filename = function() {
    paste0("fig_IRT_grm_ICC_", item_names()[input$IRT_grm_items], ".png")
  },
  content = function(file) {
    ggsave(
      file,
      plot = IRT_grm_items_icc() +
        theme(
          text = element_text(size = setting_figures$text_size),
          legend.position = "right",
          legend.key.size = unit(0.8, "lines")
        ),
      device = "png",
      height = setting_figures$height,
      width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)


# ITEMS – IIC ----------------------------------------------------------------

IRT_grm_items_iic <- reactive({
  item <- input$IRT_grm_items
  fit <- IRT_grm_fit()
  thetas <- IRT_thetas_for_plots()

  tibble(
    Ability = thetas,
    Information = iteminfo(extract.item(fit, item), thetas)
  ) |>
    ggplot(aes(Ability, Information)) +
    geom_line() +
    ggtitle(item_names()[item]) +
    theme_app()
})

output$IRT_grm_items_iic <- renderPlotly({
  g <- IRT_grm_items_iic()
  p <- ggplotly(g)
  p$elementId <- NULL
  p |> plotly::config(displayModeBar = FALSE)
})

output$IRT_grm_items_iic_download <- downloadHandler(
  filename = function() {
    paste0("fig_IRT_grm_IIC_", item_names()[input$IRT_grm_items], ".png")
  },
  content = function(file) {
    ggsave(
      file,
      plot = IRT_grm_items_iic() +
        theme(
          text = element_text(size = setting_figures$text_size),
          legend.position = "right",
          legend.key.size = unit(0.8, "lines")
        ),
      device = "png",
      height = setting_figures$height,
      width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)


# ITEMS – coefficient table --------------------------------------------------

IRT_grm_items_coef <- reactive({
  item <- input$IRT_grm_items
  IRT_grm_summary_coef()[item, , drop = FALSE]
})

output$IRT_grm_items_coef <- renderTable(
  IRT_grm_items_coef(),
  rownames = FALSE,
  na = ""
)

# ITEMS – expected score curve -----------------------------------------------

IRT_grm_items_esc <- reactive({
  item <- input$IRT_grm_items
  fit <- IRT_grm_fit()
  thetas <- IRT_thetas_for_plots()
  n_cats <- extract.item(fit, item)@ncat

  probs <- probtrace(extract.item(fit, item), thetas)
  exp_score <- as.numeric(probs %*% seq(0L, n_cats - 1L))

  tibble(
    Ability = thetas,
    ExpectedScore = exp_score,
    label = paste0(
      "Ability = ",
      round(thetas, 3),
      "\n",
      "Expected score = ",
      round(exp_score, 3)
    )
  ) |>
    ggplot(aes(x = Ability, y = ExpectedScore, group = 1)) +
    suppressWarnings(geom_line(aes(text = label))) +
    scale_y_continuous(
      name = "Expected score",
      limits = c(0, n_cats - 1L)
    ) +
    ggtitle(item_names()[item]) +
    theme_app()
})

output$IRT_grm_items_esc <- renderPlotly({
  g <- IRT_grm_items_esc()
  p <- ggplotly(g, tooltip = "text")
  p$elementId <- NULL
  p |> plotly::config(displayModeBar = FALSE)
})

output$IRT_grm_items_esc_download <- downloadHandler(
  filename = function() {
    paste0("fig_IRT_grm_ESC_", item_names()[input$IRT_grm_items], ".png")
  },
  content = function(file) {
    ggsave(
      file,
      plot = IRT_grm_items_esc() +
        theme(
          text = element_text(size = setting_figures$text_size),
          legend.position = "right",
          legend.key.size = unit(0.8, "lines")
        ),
      device = "png",
      height = setting_figures$height,
      width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ITEMS – cumulative probability curves --------------------------------------
# GRM-specific: shows P(Y >= k | theta) for k = 1, ..., K-1
# Colours match ICC: category k uses colour k+1 from the shared palette,
# so P(Y>=1) gets the same colour as "Category 1" in the ICC.

IRT_grm_items_cpc <- reactive({
  item   <- input$IRT_grm_items
  fit    <- IRT_grm_fit()
  thetas <- IRT_thetas_for_plots()
  n_cats <- extract.item(fit, item)@ncat
  # Colours for k=1,...,K-1 (skip index 1 = category 0, not shown in CPC)
  colours    <- IRT_grm_cat_colours()[seq(2L, n_cats)]
  cum_labels <- paste0("P(Y \u2265 ", seq_len(n_cats - 1L), ")")

  cat_probs <- probtrace(extract.item(fit, item), thetas)

  # P(Y >= k) for k = 1, ..., K-1
  cum_probs <- apply(cat_probs, 1, function(row) {
    rev(cumsum(rev(row)))[-1L]
  })
  if (!is.matrix(cum_probs)) {
    cum_probs <- matrix(cum_probs, ncol = 1L)
  } else {
    cum_probs <- t(cum_probs)
  }
  colnames(cum_probs) <- cum_labels

  as_tibble(cum_probs) |>
    bind_cols(theta = thetas) |>
    pivot_longer(-theta, names_to = "Category", values_to = "Probability") |>
    mutate(
      Category = factor(Category, levels = cum_labels),
      label = paste0(
        "Ability = ",     round(theta,       3), "\n",
        "Probability = ", round(Probability, 3), "\n",
        Category
      )
    ) |>
    ggplot(aes(theta, Probability, color = Category, group = Category)) +
    suppressWarnings(geom_line(aes(text = label))) +
    scale_color_manual(values = setNames(colours, cum_labels)) +
    labs(
      x     = "Ability",
      y     = "Cumulative probability",
      title = item_names()[item],
      color = "Category"
    ) +
    coord_cartesian(ylim = c(0, 1)) +
    theme_app()
})

output$IRT_grm_items_cpc <- renderPlotly({
  g <- IRT_grm_items_cpc()
  p <- suppressWarnings(ggplotly(g, tooltip = "text"))
  p$elementId <- NULL
  p |> plotly::config(displayModeBar = FALSE)
})

output$IRT_grm_items_cpc_download <- downloadHandler(
  filename = function() {
    paste0("fig_IRT_grm_CPC_", item_names()[input$IRT_grm_items], ".png")
  },
  content = function(file) {
    ggsave(
      file,
      plot   = IRT_grm_items_cpc() +
        theme(
          text             = element_text(size = setting_figures$text_size),
          legend.position  = "right",
          legend.key.size  = unit(0.8, "lines")
        ),
      device = "png",
      height = setting_figures$height,
      width  = setting_figures$width,
      dpi    = setting_figures$dpi
    )
  }
)
