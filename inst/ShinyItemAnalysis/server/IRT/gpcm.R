# ============================================================================ #
# IRT – Generalized Partial Credit Model / PCM / RSM                          #
# ============================================================================ #

# DATA -----------------------------------------------------------------------

IRT_gpcm_data <- reactive({
  ordinal() |> modify(as.integer)
})


# MODEL CHOICE & PARAMETRIZATION ---------------------------------------------

# "gpcm" | "pcm" | "rsm"  – the mirt itemtype (PCM = gpcm with a=1)
IRT_gpcm_model <- reactive({
  switch(
    input$IRT_gpcm_model,
    gpcm = "gpcm",
    pcm  = "gpcm",   # fitted as gpcm but slopes constrained via pars below
    rsm  = "rsm"
  )
})

IRT_gpcm_is_irt <- reactive({
  input$IRT_gpcm_parametrization == "irt"
})


# MODEL FIT ------------------------------------------------------------------

# internal helper: fit one model (pure function, no caching)
.gpcm_fit_one <- function(model, data, ncycles) {
  if (model == "pcm") {
    pars <- suppressMessages(mirt(data, 1, itemtype = "gpcm", pars = "values"))
    pars$value[pars$name == "a1"] <- 1
    pars$est[pars$name   == "a1"] <- FALSE
    suppressMessages(mirt(
      data, model = 1, itemtype = "gpcm",
      SE = TRUE, verbose = FALSE, pars = pars,
      technical = list(NCYCLES = ncycles)
    ))
  } else {
    suppressMessages(mirt(
      data, model = 1, itemtype = model,
      SE = TRUE, verbose = FALSE,
      technical = list(NCYCLES = ncycles)
    ))
  }
}

# helper: do all items share the same number of response categories? (RSM req.)
.gpcm_equal_cats <- function(data) {
  length(unique(vapply(data, function(x) length(unique(x)), integer(1L)))) == 1L
}

# One cached fit per model, via Shiny's native cache. The cache key is the set
# of lightweight inputs that fully determine the fit — the data, ncycles and a
# model discriminator — NOT the heavy mirt object, so the key is cheap and
# stable to hash (keying on a serialized mirt fit is slow and was a source of
# flaky cache hits before). bindCache keeps all previous values, so switching
# model / data / ncycles and back is a cache hit with no refit. The
# model-comparison sub-tab reuses these very reactives, so each model is fit at
# most once per (data, ncycles) and there is no separate "fit all" path to drift
# out of sync.
IRT_gpcm_fit_gpcm <- reactive({
  .gpcm_fit_one("gpcm", IRT_gpcm_data(), input$ncycles)
}) |>
  bindCache(IRT_gpcm_data(), input$ncycles, "gpcm") |>
  bindEvent(IRT_gpcm_data(), input$ncycles)

IRT_gpcm_fit_pcm <- reactive({
  .gpcm_fit_one("pcm", IRT_gpcm_data(), input$ncycles)
}) |>
  bindCache(IRT_gpcm_data(), input$ncycles, "pcm") |>
  bindEvent(IRT_gpcm_data(), input$ncycles)

IRT_gpcm_fit_rsm <- reactive({
  .gpcm_fit_one("rsm", IRT_gpcm_data(), input$ncycles)
}) |>
  bindCache(IRT_gpcm_data(), input$ncycles, "rsm") |>
  bindEvent(IRT_gpcm_data(), input$ncycles)

# Selected fit for the single-model summary / items views. Thin, uncached
# router that also carries the RSM equal-categories validation message.
IRT_gpcm_fit <- reactive({
  model <- input$IRT_gpcm_model
  if (model == "rsm") {
    validate(need(
      .gpcm_equal_cats(IRT_gpcm_data()),
      "RSM requires all items to have the same number of response categories."
    ))
  }
  switch(
    model,
    gpcm = IRT_gpcm_fit_gpcm(),
    pcm  = IRT_gpcm_fit_pcm(),
    rsm  = IRT_gpcm_fit_rsm()
  )
})


# COEFFICIENT TABLE ----------------------------------------------------------

# Note: .ordinalIRT_coef_with_se() is defined in server_grm.R and shared here.
# The delta-method SE transformation is identical for GPCM/PCM (b_k = -d_k/a1).
# For RSM, mirt uses a different internal parametrization but the same
# intercept-slope → IRT relationship holds column-wise, so the helper applies.

IRT_gpcm_summary_coef <- reactive({
  validate(
    need(
      input$IRT_gpcm_model != "rsm" ||
        length(unique(sapply(IRT_gpcm_data(), function(x) length(unique(x))))) == 1L,
      "RSM requires all items to have the same number of response categories."
    )
  )
  is_irt <- IRT_gpcm_is_irt()

  coefs <- .ordinalIRT_coef_with_se(IRT_gpcm_fit(), irt_pars = is_irt)

  coef_tbl <- coefs |>
    map_dfr(~ as_tibble(.x, rownames = "type"), .id = "item")

  par_cols <- setdiff(colnames(coef_tbl), c("item", "type"))
  is_rsm   <- input$IRT_gpcm_model == "rsm"

  # ------ Rename internal column names to display-ready LaTeX tokens --------
  # The helper outputs different column names per model/parametrization:
  #   GPCM/PCM IRT:       a1, b1, b2, …
  #   GPCM/PCM int-slope: a1, d1, d2, …
  #   RSM IRT:            b,  lambda1, lambda2, …
  #   RSM int-slope:      betai0, betat1, betat2, …

  if (!is_rsm) {
    if (is_irt) {
      # a_i, b_{i1}, b_{i2}, …
      renamed <- par_cols |>
        str_replace("^a1$",      "ai")     |>
        str_replace("^b(\\d+)$","bi\\1")
    } else {
      # beta_{1i}, beta_{0i1}, beta_{0i2}, …
      renamed <- par_cols |>
        str_replace("^a1$",      "beta1i")      |>
        str_replace("^d(\\d+)$","beta0i\\1")
    }
    names(coef_tbl)[names(coef_tbl) %in% par_cols] <- renamed
    par_cols_new <- renamed

    coef_wide <- coef_tbl |>
      pivot_wider(
        id_cols     = item,
        names_from  = type,
        values_from = all_of(par_cols_new),
        names_glue  = "{if_else(type == 'par', '', type)}_{.value}"
      ) |>
      column_to_rownames("item")

    # LaTeX formatter matching tokens: ai, bi1, bi2, beta1i, beta0i1, …
    colnames(coef_wide) <- colnames(coef_wide) |>
      map_chr(~ {
        col   <- .x
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
          paste0("\\(\\mathit{", inner, "}\\)")
        }
        if (is_se) paste0("SE(", lx, ")") else lx
      })

  } else {
    # RSM: dedicated LaTeX header builder — column names from helper are
    # semantic tokens (b, lambda1, …  or  betai0, betat1, …)
    par_cols_new <- par_cols

    coef_wide <- coef_tbl |>
      pivot_wider(
        id_cols     = item,
        names_from  = type,
        values_from = all_of(par_cols_new),
        names_glue  = "{if_else(type == 'par', '', type)}_{.value}"
      ) |>
      column_to_rownames("item")

    rsm_col_latex <- function(col) {
      is_se <- startsWith(col, "SE_")
      inner <- sub("^(SE_|_)", "", col)   # strip leading "SE_" or "_"
      lx <- if (is_irt) {
        # IRT columns: "b1", "b2", … (item locations) -> b_i
        #              "lambda1", "lambda2", … (common thresholds) -> lambda_1, lambda_2, …
        if (startsWith(inner, "lambda")) {
          idx <- sub("^lambda", "", inner)
          paste0("\\(\\mathit{\\lambda_{", idx, "}}\\)")
        } else {
          "\\(\\mathit{b_i}\\)"
        }
      } else {
        # Intercept-slope: "betab1", "betab2", … -> beta_{i0}
        #                  "betalambda1", … -> beta_{t1}, beta_{t2}, …
        if (startsWith(inner, "betalambda")) {
          idx <- sub("^betalambda", "", inner)
          paste0("\\(\\mathit{\\beta_{t", idx, "}}\\)")
        } else {
          "\\(\\mathit{\\beta_{i0}}\\)"
        }
      }
      if (is_se) paste0("SE(", lx, ")") else lx
    }

    colnames(coef_wide) <- vapply(colnames(coef_wide), rsm_col_latex, character(1L))
  }

  # S-X2 item fit statistics — only when no missing data
  if (!anyNA(IRT_gpcm_data())) {
    fit_stats <- tryCatch(
      suppressMessages(itemfit(IRT_gpcm_fit(), fit.statistics = "S_X2", na.rm = TRUE)),
      error = function(e) NULL
    )
    if (!is.null(fit_stats)) {
      sx2_tbl <- tibble(
        item  = item_names(),
        SX2   = round(fit_stats$S_X2,  3),
        df    = fit_stats$df.S_X2,
        p     = round(fit_stats$p.S_X2, 3)
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
  bindCache(IRT_gpcm_data(), input$IRT_gpcm_model, input$IRT_gpcm_parametrization, input$ncycles) |>
  bindEvent(IRT_gpcm_data(), input$IRT_gpcm_model, input$IRT_gpcm_parametrization, input$ncycles)


# SUMMARY – IIC plot ---------------------------------------------------------

IRT_gpcm_summary_iic <- reactive({
  validate(
    need(
      input$IRT_gpcm_model != "rsm" || .gpcm_equal_cats(IRT_gpcm_data()),
      "RSM requires all items to have the same number of response categories."
    )
  )
  fit    <- IRT_gpcm_fit()
  nms    <- colnames(fit@Data$data)
  thetas <- IRT_thetas_for_plots()

  map2_dfr(
    nms, item_names(),
    ~ tibble(
      Ability     = thetas,
      Information = iteminfo(extract.item(fit, .x), thetas),
      Item        = .y,
      label       = paste0(
        "Ability = ",     round(Ability,     3), "\n",
        "Information = ", round(Information, 3), "\n",
        "Item = ",        Item
      )
    )
  ) |>
    ggplot(aes(Ability, Information, color = Item, group = Item)) +
    suppressWarnings(geom_line(aes(text = label))) +
    theme_app()
}) |>
  bindCache(IRT_gpcm_data(), input$IRT_gpcm_model, input$ncycles) |>
  bindEvent(IRT_gpcm_data(), input$IRT_gpcm_model, input$ncycles)

IRT_gpcm_summary_iic_plotly <- reactive({
  validate(
    need(
      input$IRT_gpcm_model != "rsm" || .gpcm_equal_cats(IRT_gpcm_data()),
      "RSM requires all items to have the same number of response categories."
    )
  )
  g <- IRT_gpcm_summary_iic()
  p <- ggplotly(g, tooltip = "text")
  p$elementId <- NULL
  p |> plotly::config(displayModeBar = FALSE)
}) |>
  bindCache(IRT_gpcm_data(), input$IRT_gpcm_model, input$ncycles) |>
  bindEvent(IRT_gpcm_data(), input$IRT_gpcm_model, input$ncycles)

output$IRT_gpcm_summary_iic <- renderPlotly({
  IRT_gpcm_summary_iic_plotly()
})

output$IRT_gpcm_summary_iic_download <- downloadHandler(
  filename = function() "fig_IRT_gpcm_IIC.png",
  content  = function(file) {
    ggsave(
      file,
      plot   = IRT_gpcm_summary_iic() +
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


# SUMMARY – TIC plot ---------------------------------------------------------

IRT_gpcm_summary_tic <- reactive({
  validate(
    need(
      input$IRT_gpcm_model != "rsm" || .gpcm_equal_cats(IRT_gpcm_data()),
      "RSM requires all items to have the same number of response categories."
    )
  )
  fit    <- IRT_gpcm_fit()
  thetas <- IRT_thetas_for_plots()

  tibble(
    Ability     = thetas,
    Information = testinfo(fit, thetas),
    SE          = 1 / sqrt(Information)
  ) |>
    ggplot(aes(x = Ability)) +
    geom_line(aes(y = Information, col = "info")) +
    geom_line(aes(y = SE,          col = "se"))   +
    scale_color_manual(
      values = c("blue", "pink"),
      labels = c("Information", "SE")
    ) +
    scale_y_continuous("Information", sec.axis = sec_axis(~., name = "SE")) +
    theme(axis.title.y.right = element_text(color = "pink")) +
    theme_app()
}) |>
  bindCache(IRT_gpcm_data(), input$IRT_gpcm_model, input$ncycles) |>
  bindEvent(IRT_gpcm_data(), input$IRT_gpcm_model, input$ncycles)

IRT_gpcm_summary_tic_plotly <- reactive({
  validate(
    need(
      input$IRT_gpcm_model != "rsm" || .gpcm_equal_cats(IRT_gpcm_data()),
      "RSM requires all items to have the same number of response categories."
    )
  )
  g <- IRT_gpcm_summary_tic()
  p <- ggplotly(g)
  p$x$data[[1]]$text <- gsub("<br />colour: info", "", p$x$data[[1]]$text)
  p$x$data[[2]]$text <- gsub("<br />colour: se",   "", p$x$data[[2]]$text)
  p$elementId <- NULL
  p |> plotly::config(displayModeBar = FALSE)
}) |>
  bindCache(IRT_gpcm_data(), input$IRT_gpcm_model, input$ncycles) |>
  bindEvent(IRT_gpcm_data(), input$IRT_gpcm_model, input$ncycles)

output$IRT_gpcm_summary_tic <- renderPlotly({
  IRT_gpcm_summary_tic_plotly()
})

output$IRT_gpcm_summary_tic_download <- downloadHandler(
  filename = function() "fig_IRT_gpcm_TIC.png",
  content  = function(file) {
    ggsave(
      file,
      plot   = IRT_gpcm_summary_tic() +
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


# SUMMARY – coefficient table ------------------------------------------------

output$IRT_gpcm_summary_coef <- renderTable(
  IRT_gpcm_summary_coef(),
  rownames = TRUE,
  striped  = TRUE,
  na       = ""
)

output$IRT_gpcm_summary_coef_download <- downloadHandler(
  filename = function() "IRT_gpcm_coefs.csv",
  content  = function(file) {
    tab <- IRT_gpcm_summary_coef()
    names(tab) <- names(tab) |>
      str_remove_all("\\\\\\(\\\\mathit\\{\\\\") |>
      str_remove_all("\\\\\\)") |>
      str_remove_all("[{}}]")
    write.csv(tab, file)
  }
)


# SUMMARY – ability estimates ------------------------------------------------

IRT_gpcm_summary_fscores <- reactive({
  validate(
    need(
      input$IRT_gpcm_model != "rsm" || .gpcm_equal_cats(IRT_gpcm_data()),
      "RSM requires all items to have the same number of response categories."
    )
  )
  fit          <- IRT_gpcm_fit()
  fs           <- fscores(fit, full.scores.SE = TRUE)
  colnames(fs) <- c("F-score", "SE(F-score)")
  tab <- data.frame(
    `Total score` = total_score(),
    `Z-score`     = z_score(),
    `T-score`     = t_score(),
    fs,
    check.names   = FALSE
  )
  rownames(tab) <- paste("Respondent", seq_len(nrow(tab)))
  tab
}) |>
  bindCache(IRT_gpcm_data(), input$IRT_gpcm_model, input$ncycles) |>
  bindEvent(IRT_gpcm_data(), input$IRT_gpcm_model, input$ncycles)

output$IRT_gpcm_summary_ability <- renderTable({
  validate(
    need(
      input$IRT_gpcm_model != "rsm" ||
        length(unique(sapply(IRT_gpcm_data(), function(x) length(unique(x))))) == 1L,
      "RSM requires all items to have the same number of response categories."
    )
  )
  head(IRT_gpcm_summary_fscores(), 6)
}, rownames = TRUE)

output$IRT_gpcm_summary_ability_download <- downloadHandler(
  filename = function() "IRT_gpcm_abilities.csv",
  content  = function(file) write.csv(IRT_gpcm_summary_fscores(), file)
)

IRT_gpcm_ability_cor <- reactive({
  validate(
    need(
      input$IRT_gpcm_model != "rsm" || .gpcm_equal_cats(IRT_gpcm_data()),
      "RSM requires all items to have the same number of response categories."
    )
  )
  tab <- IRT_gpcm_summary_fscores()
  cor(tab[["F-score"]], tab[["Z-score"]], use = "pairwise.complete.obs")
}) |>
  bindCache(IRT_gpcm_data(), input$IRT_gpcm_model, input$ncycles) |>
  bindEvent(IRT_gpcm_data(), input$IRT_gpcm_model, input$ncycles)

output$IRT_gpcm_summary_ability_correlation_text <- renderText({
  validate(
    need(
      input$IRT_gpcm_model != "rsm" ||
        length(unique(sapply(IRT_gpcm_data(), function(x) length(unique(x))))) == 1L,
      "RSM requires all items to have the same number of response categories."
    )
  )
  paste0(
    "This scatterplot shows the relationship between the standardized total ",
    "score (Z-score) and the factor score estimated by the IRT model. The ",
    "Pearson correlation coefficient between these two scores is ",
    sprintf("%.3f", IRT_gpcm_ability_cor()), "."
  )
})

IRT_gpcm_summary_ability_plot <- reactive({
  validate(
    need(
      input$IRT_gpcm_model != "rsm" || .gpcm_equal_cats(IRT_gpcm_data()),
      "RSM requires all items to have the same number of response categories."
    )
  )
  df <- IRT_gpcm_summary_fscores()
  ggplot(df, aes(`Z-score`, `F-score`)) +
    geom_point(size = 3) +
    labs(x = "Standardized total score", y = "Factor score") +
    theme_app()
}) |>
  bindCache(IRT_gpcm_data(), input$IRT_gpcm_model, input$ncycles) |>
  bindEvent(IRT_gpcm_data(), input$IRT_gpcm_model, input$ncycles)

IRT_gpcm_summary_ability_plotly <- reactive({
  validate(
    need(
      input$IRT_gpcm_model != "rsm" || .gpcm_equal_cats(IRT_gpcm_data()),
      "RSM requires all items to have the same number of response categories."
    )
  )
  g <- IRT_gpcm_summary_ability_plot()
  p <- ggplotly(g)
  p$elementId <- NULL
  p |> plotly::config(displayModeBar = FALSE)
}) |>
  bindCache(IRT_gpcm_data(), input$IRT_gpcm_model, input$ncycles) |>
  bindEvent(IRT_gpcm_data(), input$IRT_gpcm_model, input$ncycles)

output$IRT_gpcm_summary_ability_plot <- renderPlotly({
  IRT_gpcm_summary_ability_plotly()
})

output$IRT_gpcm_summary_ability_plot_download <- downloadHandler(
  filename = function() "fig_IRT_gpcm_abilities.png",
  content  = function(file) {
    ggsave(
      file,
      plot   = IRT_gpcm_summary_ability_plot() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height,
      width  = setting_figures$width,
      dpi    = setting_figures$dpi
    )
  }
)


# ITEMS – slider update ------------------------------------------------------

observe({
  updateSliderInput(
    session = session,
    inputId = "IRT_gpcm_items",
    max     = ncol(IRT_gpcm_data())
  )
})


# ITEMS – shared colour palette ---------------------------------------------

IRT_gpcm_max_cats <- reactive({
  validate(
    need(
      input$IRT_gpcm_model != "rsm" ||
        length(unique(sapply(IRT_gpcm_data(), function(x) length(unique(x))))) == 1L,
      "RSM requires all items to have the same number of response categories."
    )
  )
  fit <- IRT_gpcm_fit()
  max(sapply(seq_len(fit@Data$nitems), function(i) extract.item(fit, i)@ncat))
})

IRT_gpcm_cat_colours <- reactive({
  validate(
    need(
      input$IRT_gpcm_model != "rsm" ||
        length(unique(sapply(IRT_gpcm_data(), function(x) length(unique(x))))) == 1L,
      "RSM requires all items to have the same number of response categories."
    )
  )
  gg_color_hue(IRT_gpcm_max_cats())
})


# ITEMS – ICC ----------------------------------------------------------------

IRT_gpcm_items_icc <- reactive({
  validate(
    need(
      input$IRT_gpcm_model != "rsm" ||
        length(unique(sapply(IRT_gpcm_data(), function(x) length(unique(x))))) == 1L,
      "RSM requires all items to have the same number of response categories."
    )
  )
  item       <- input$IRT_gpcm_items
  fit        <- IRT_gpcm_fit()
  thetas     <- IRT_thetas_for_plots()
  n_cats     <- extract.item(fit, item)@ncat
  colours    <- IRT_gpcm_cat_colours()[seq_len(n_cats)]
  cat_labels <- paste0("Category ", seq_len(n_cats) - 1L)

  probs <- as_tibble(probtrace(extract.item(fit, item), thetas))
  names(probs) <- cat_labels

  probs |>
    bind_cols(theta = thetas) |>
    pivot_longer(-theta, names_to = "Category", values_to = "Probability") |>
    mutate(
      Category = factor(Category, levels = cat_labels),
      label = paste0(
        "Ability = ",     round(theta,       3), "\n",
        "Probability = ", round(Probability, 3), "\n",
        Category
      )
    ) |>
    ggplot(aes(theta, Probability, color = Category, group = Category)) +
    suppressWarnings(geom_line(aes(text = label))) +
    scale_color_manual(values = setNames(colours, cat_labels)) +
    labs(
      x     = "Ability",
      y     = "Category probability",
      title = item_names()[item],
      color = "Category"
    ) +
    coord_cartesian(ylim = c(0, 1)) +
    theme_app()
})

output$IRT_gpcm_items_icc <- renderPlotly({
  g <- IRT_gpcm_items_icc()
  p <- ggplotly(g, tooltip = "text")
  p$elementId <- NULL
  p |> plotly::config(displayModeBar = FALSE)
})

output$IRT_gpcm_items_icc_download <- downloadHandler(
  filename = function() {
    paste0("fig_IRT_gpcm_ICC_", item_names()[input$IRT_gpcm_items], ".png")
  },
  content = function(file) {
    ggsave(
      file,
      plot   = IRT_gpcm_items_icc() +
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


# ITEMS – IIC ----------------------------------------------------------------

IRT_gpcm_items_iic <- reactive({
  validate(
    need(
      input$IRT_gpcm_model != "rsm" ||
        length(unique(sapply(IRT_gpcm_data(), function(x) length(unique(x))))) == 1L,
      "RSM requires all items to have the same number of response categories."
    )
  )
  item   <- input$IRT_gpcm_items
  fit    <- IRT_gpcm_fit()
  thetas <- IRT_thetas_for_plots()

  tibble(
    Ability     = thetas,
    Information = iteminfo(extract.item(fit, item), thetas)
  ) |>
    ggplot(aes(Ability, Information)) +
    geom_line() +
    ggtitle(item_names()[item]) +
    theme_app()
})

output$IRT_gpcm_items_iic <- renderPlotly({
  g <- IRT_gpcm_items_iic()
  p <- ggplotly(g)
  p$elementId <- NULL
  p |> plotly::config(displayModeBar = FALSE)
})

output$IRT_gpcm_items_iic_download <- downloadHandler(
  filename = function() {
    paste0("fig_IRT_gpcm_IIC_", item_names()[input$IRT_gpcm_items], ".png")
  },
  content = function(file) {
    ggsave(
      file,
      plot   = IRT_gpcm_items_iic() +
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


# ITEMS – coefficient table --------------------------------------------------

IRT_gpcm_items_coef <- reactive({
  validate(
    need(
      input$IRT_gpcm_model != "rsm" ||
        length(unique(sapply(IRT_gpcm_data(), function(x) length(unique(x))))) == 1L,
      "RSM requires all items to have the same number of response categories."
    )
  )
  item <- input$IRT_gpcm_items
  IRT_gpcm_summary_coef()[item, , drop = FALSE]
})

output$IRT_gpcm_items_coef <- renderTable(
  IRT_gpcm_items_coef(),
  rownames = FALSE,
  na       = ""
)


# ITEMS – expected score curve -----------------------------------------------

IRT_gpcm_items_esc <- reactive({
  validate(
    need(
      input$IRT_gpcm_model != "rsm" ||
        length(unique(sapply(IRT_gpcm_data(), function(x) length(unique(x))))) == 1L,
      "RSM requires all items to have the same number of response categories."
    )
  )
  item   <- input$IRT_gpcm_items
  fit    <- IRT_gpcm_fit()
  thetas <- IRT_thetas_for_plots()
  n_cats <- extract.item(fit, item)@ncat

  probs     <- probtrace(extract.item(fit, item), thetas)
  exp_score <- as.numeric(probs %*% seq(0L, n_cats - 1L))

  tibble(
    Ability       = thetas,
    ExpectedScore = exp_score,
    label         = paste0(
      "Ability = ",        round(thetas,    3), "\n",
      "Expected score = ", round(exp_score, 3)
    )
  ) |>
    ggplot(aes(x = Ability, y = ExpectedScore, group = 1)) +
    suppressWarnings(geom_line(aes(text = label))) +
    scale_y_continuous(
      name   = "Expected score",
      limits = c(0, n_cats - 1L)
    ) +
    ggtitle(item_names()[item]) +
    theme_app()
})

output$IRT_gpcm_items_esc <- renderPlotly({
  g <- IRT_gpcm_items_esc()
  p <- ggplotly(g, tooltip = "text")
  p$elementId <- NULL
  p |> plotly::config(displayModeBar = FALSE)
})

output$IRT_gpcm_items_esc_download <- downloadHandler(
  filename = function() {
    paste0("fig_IRT_gpcm_ESC_", item_names()[input$IRT_gpcm_items], ".png")
  },
  content = function(file) {
    ggsave(
      file,
      plot   = IRT_gpcm_items_esc() +
        theme(
          text            = element_text(size = setting_figures$text_size),
          legend.position = "right",
          legend.key.size = unit(0.8, "lines")
        ),
      device = "png",
      height = setting_figures$height,
      width  = setting_figures$width,
      dpi    = setting_figures$dpi
    )
  }
)


# MODEL COMPARISON -----------------------------------------------------------

# Displays AIC/BIC/logLik + LRT for GPCM, PCM and RSM in a single table — same
# structure as IRT_binary_comparison. The three fits come straight from the
# per-model cached reactives above, so visiting the comparison tab reuses any
# model the user already viewed and fits the rest once (caching them for the
# single-model views too). RSM is skipped gracefully when items have differing
# numbers of response categories.

IRT_gpcm_comparison <- reactive({
  req(input$gpcm_tabs == "gpcm_comparison")

  rsm_ok <- .gpcm_equal_cats(IRT_gpcm_data())

  fits <- Filter(Negate(is.null), list(
    RSM  = if (rsm_ok) IRT_gpcm_fit_rsm() else NULL,
    PCM  = IRT_gpcm_fit_pcm(),
    GPCM = IRT_gpcm_fit_gpcm()
  ))

  validate(need(length(fits) >= 1L, "No models could be fitted."))
  nms <- names(fits)

  # Information criteria
  ic <- do.call(rbind, lapply(fits, function(f) {
    data.frame(
      AIC    = round(mirt::extract.mirt(f, "AIC"),    3),
      BIC    = round(mirt::extract.mirt(f, "BIC"),    3),
      logLik = round(mirt::extract.mirt(f, "logLik"), 3)
    )
  }))
  rownames(ic) <- nms

  # LRT columns — NA for GPCM (most general, nothing to compare against)
  lrt_df <- data.frame(
    `LR statistic` = rep(NA_real_, length(nms)),
    df             = rep(NA_integer_, length(nms)),
    `p-value`      = rep(NA_real_, length(nms)),
    check.names    = FALSE,
    row.names      = nms
  )

  if (length(fits) >= 2L) {
    # Pairwise anova: RSM vs PCM, PCM vs GPCM (from most to least restricted)
    lrt_list <- lapply(seq_len(length(fits) - 1L), function(i) {
      suppressMessages(anova(fits[[i]], fits[[i + 1L]]))
    })

    for (i in seq_along(lrt_list)) {
      lrt_row <- lrt_list[[i]]

      # Find columns defensively — mirt version differences
      # mirt anova() columns: X2, df, p (row 2 has the test result)
      x2_col <- intersect(c("X2", "Chisq", "LRT"),      colnames(lrt_row))[1]
      df_col <- intersect(c("df", "Df"),                 colnames(lrt_row))[1]
      p_col  <- intersect(c("p", "Pr(>Chisq)", "p.X2"), colnames(lrt_row))[1]

      if (!is.na(x2_col) && !is.na(df_col) && !is.na(p_col)) {
        lrt_df[i, "LR statistic"] <- round(lrt_row[2L, x2_col], 3)
        lrt_df[i, "df"]           <- lrt_row[2L, df_col]
        lrt_df[i, "p-value"]      <- round(lrt_row[2L, p_col],  3)
      }
    }
  }

  # Convert to character for uniform rbind with BEST row
  combined <- cbind(ic, lrt_df)
  combined[] <- lapply(combined, function(x) ifelse(is.na(x), "", as.character(x)))

  # Determine best model by LRT: the least restricted model that provides
  # a significant improvement (p < 0.05). Models ordered RSM < PCM < GPCM.
  # If no significant LRT, prefer most restricted (RSM if available, else PCM).
  lrt_best <- if (length(fits) >= 2L && any(!is.na(lrt_df[["p-value"]]))) {
    p_vals <- suppressWarnings(as.numeric(lrt_df[["p-value"]]))
    sig     <- which(!is.na(p_vals) & p_vals < 0.05)
    if (length(sig) > 0L) {
      # Last significant comparison wins — pick the less restricted model
      nms[sig[length(sig)] + 1L]
    } else {
      # No significant improvement — most restricted model is best
      nms[1L]
    }
  } else if (length(fits) == 1L) {
    nms[1L]
  } else {
    ""
  }

  best <- data.frame(
    AIC            = nms[which.min(ic$AIC)],
    BIC            = nms[which.min(ic$BIC)],
    logLik         = "",
    `LR statistic` = "",
    df             = "",
    `p-value`      = lrt_best,
    check.names    = FALSE,
    stringsAsFactors = FALSE
  )
  rownames(best) <- "BEST"
  rbind(combined, best)
})

output$IRT_gpcm_comparison <- DT::renderDataTable({
  df <- IRT_gpcm_comparison()
  n  <- nrow(df)
  DT::datatable(
    df,
    rownames  = TRUE,
    options   = list(
      dom        = "t",       # table only — no search/pagination
      ordering   = FALSE,
      pageLength = n
    )
  ) |>
    DT::formatStyle(
      columns    = 0,          # row names column
      target     = "row",
      rows       = n,          # last row = BEST
      fontWeight = DT::styleEqual(n, "bold")
    ) |>
    DT::formatStyle(
      columns    = seq_len(ncol(df)),
      target     = "row",
      fontWeight = DT::styleRow(n, "bold")
    )
}, server = FALSE)


output$IRT_gpcm_comparison_download <- downloadHandler(
  filename = function() "IRT_gpcm_model_comparison.csv",
  content  = function(file) write.csv(IRT_gpcm_comparison(), file, na = "")
)
