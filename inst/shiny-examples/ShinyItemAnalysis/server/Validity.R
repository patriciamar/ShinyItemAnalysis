# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# VALIDITY ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * CORRELATION STRUCTURE ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# ** Updating number of clusters slider ####
observe({
  item_count <- ncol(ordinal())
  updateSliderInput(
    session = session,
    inputId = "corr_plot_clust",
    value = 0,
    max = item_count
  )
})

# ** Polychoric correlation matrix ####
corr_structure <- reactive({
  data <- ordinal()

  # calculate correlations depending on selected method
  if (input$type_of_corr == "spearman") {
    corP <- cor(data, method = "spearman", use = "pairwise.complete.obs")
  } else if (input$type_of_corr == "pearson") {
    corP <- cor(data, method = "pearson", use = "pairwise.complete.obs")
  } else if (input$type_of_corr == "polychoric") {
    corP <- polychoric(data, na.rm = TRUE)
    corP <- corP$rho
  }
  corP
})

# ** Correlation plot ####
corr_plot_Input <- reactive({
  plot_corr(corr_structure(),
    cor = "none", clust_method = input$corr_plot_clustmethod,
    n_clust = input$corr_plot_clust, labels = input$show_corr, labels_size = input$corr_plot_labs_size
  )
})

# ** Updating number of clusters slider for reports ####
observe({
  item_count <- ncol(ordinal())
  updateSliderInput(
    session = session,
    inputId = "corr_plot_clust_report",
    value = 0,
    max = item_count
  )
})

# ** Correlation plot for reports ####
corr_plot_Input_report <- reactive({
  if (input$type_of_corr == input$corr_plot_type_of_corr_report) {
    corP <- corr_structure()
  } else {
    data <- ordinal()
    if (input$corr_plot_type_of_corr_report == "spearman") {
      corP <- cor(data, method = "spearman", use = "pairwise.complete.obs")
    } else if (input$corr_plot_type_of_corr_report == "pearson") {
      corP <- cor(data, method = "pearson", use = "pairwise.complete.obs")
    } else if (input$corr_plot_type_of_corr_report == "polychoric") {
      corP <- polychoric(data, na.rm = T)
      corP <- corP$rho
    }
  }

  plot_corr(corP,
    cor = "none", clust_method = input$corr_plot_clustmethod_report,
    n_clust = input$corr_plot_clust_report
  )
})

# ** Output correlation plot ####
output$corr_plot <- renderPlotly({
  plt <- corr_plot_Input() %>%
    ggplotly(tooltip = c("x", "y", "label")) %>%
    layout(
      xaxis = list(
        constrain = "domain",
        side = "top",
        tickangle = -90
      ), # fix asp. ratio
      yaxis = list(
        constrain = "domain",
        # scaleanchor = 'x',
        scaleratio = 1
      )
    ) %>%
    plotly::config(displayModeBar = FALSE)

  # editing legend appearance
  colorbar_ind <- which(sapply(plt$x$data, function(x) any(grepl("marker", names(x)))))
  colorbar_ind2 <- which(sapply(plt$x$data[colorbar_ind], function(x) any(grepl("colorbar", names(x$marker)))))
  plt$x$data[[colorbar_ind[colorbar_ind2]]]$marker$colorbar$outlinewidth <- 0

  # disable hoverinfo on clusters
  cluster_ind <- which(sapply(plt$x$data, function(x) any(grepl("fill", names(x)))))
  if (length(cluster_ind)) {
    plt$x$data[[cluster_ind]]$hoverinfo <- "skip"
  }

  plt
})

# ** DB correlation plot ####
output$DB_corr_plot <- downloadHandler(
  filename = function() {
    "fig_CorrelationPlot.png"
  },
  content = function(file) {
    plt <- plot_corr(corr_structure(),
      cor = "none",
      clust_method = input$corr_plot_clustmethod,
      n_clust = input$corr_plot_clust,
      labels = input$show_corr,
      labels_size = setting_figures$text_size * 0.25 # size in geom is in mm, settings is in pt
    ) +
      theme(text = element_text(size = setting_figures$text_size))
    ggsave(file,
      plot = plt,
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)


# ** DB correlation matrix ####
output$corr_matrix <- downloadHandler(
  filename = function() {
    paste("Correlation_matrix", ".csv", sep = "")
  },
  content = function(file) {
    corP <- corr_structure()
    write.csv(corP, file)
  }
)

# ** Dendrogram ####
dendrogram_plot_Input <- reactive({
  corP <- corr_structure()
  dist <- as.dist(1 - corP)
  clustmethod <- input$corr_plot_clustmethod
  if (clustmethod == "none") {
    return(ggplot() +
      geom_blank())
  }

  numclust <- input$corr_plot_clust

  hc <- hclust(dist, method = clustmethod)

  if (numclust <= 1) {
    order <- hc$order
    label <- if (!input$data_csvdata_keep_itemnames) item_names()[hc$order] else hc$label[hc$order]
    times <- length(label)
    cluster <- rep(paste("Cluster 1"), times)
  } else {
    plot(hc)
    rhc <- rect.hclust(hc, k = numclust)
    order <- unlist(rhc)
    label <- if (!input$data_csvdata_keep_itemnames) item_names()[order] else names(order)
    times <- sapply(rhc, length)
    cluster <- rep(paste("Cluster", 1:numclust), times)
  }

  df <- data.frame(
    label = label,
    num = order,
    cluster = cluster
  )
  dendr <- dendro_data(hc, type = "rectangle")
  if (!input$data_csvdata_keep_itemnames) {
    dendr$labels$label <- label
  }

  dfd <- merge(dendr$labels, df, by = "label")

  ggplot() +
    geom_segment(aes(y, x, xend = yend, yend = xend), data = segment(dendr)) +
    geom_text(aes(y, x, label = label, color = cluster), hjust = 0, data = dfd) +
    scale_x_reverse(expand = c(0, .05, 0, .15)) +
    ylab("Height") +
    theme_app() +
    theme(
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.title.x = element_blank()
    )
})

# ** Output dendrogram ####
output$dendrogram_plot <- renderPlotly({
  g <- dendrogram_plot_Input()
  p <- ggplotly(g, tooltip = c("label", "cluster"))

  p$elementId <- NULL
  p %>%
    plotly::config(displayModeBar = FALSE) %>%
    style(textposition = "right") %>%
    layout(showlegend = TRUE)
})

# ** DB dendrogram ####
output$DB_dendrogram <- downloadHandler(
  filename = function() {
    paste("fig_Dendrogram.png", sep = "")
  },
  content = function(file) {
    ggsave(file,
      plot = dendrogram_plot_Input() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * FACTOR ANALYSIS ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# validation in case any var(item) == 0
validity_factor_nonzero_vars_check <- reactive({
  validate(
    need(ordinal(), "No data in this dataset."),
    need(
      !nzchar(data_check_binary_all01_text()),
      {
        zero_var_items <- names(which(sapply(ordinal(), var) == 0))
        single <- length(zero_var_items) == 1

        paste(
          ifelse(single, "Item", "Items"),
          paste(zero_var_items, collapse = ", "),
          ifelse(single, "has", "have"),
          "zero variance.",
          "\nParallel and factor analyses both require that all items have nonzero variances.",
          "\nYou can remove the problematic",
          ifelse(single, "item", "items"), "in the Data tab."
        )
      }
    ),
    errorClass = "validation-error"
  )
})

# ** Parallel analysis ####
validity_factor_parallel_analysis <- reactive({
  validity_factor_nonzero_vars_check()

  text_out <- capture.output(data_out <- tryCatch(
    {
      fa_parallel(ordinal(),
        cor = input$validity_factor_pa_cor,
        method = input$validity_factor_pa_method,
        n_iter = 20, plot = FALSE
      )
    },
    error = function(e) e
  ))

  # check for any exceptions and hand them to the user
  validate(need(
    !inherits(data_out, c("simpleError", "error", "condition")),
    paste0(
      "Error returned:\n",
      data_out$message
    )
  ),
  errorClass = "validation-error"
  )

  list(data = data_out, text = text_out)
})

# ** Output scree plot ####
output$validity_factor_screeplot <- renderPlotly({
  sia_parallel_out <- validity_factor_parallel_analysis()[["data"]]
  method <- if (nlevels(sia_parallel_out[["method"]]) == 2) {
    "both"
  } else {
    levels(sia_parallel_out[["method"]])
  }

  plt <- sia_parallel_out %>%
    plot() %>%
    ggplotly() %>%
    style(textposition = "left") %>%
    layout(legend = list(x = .95, y = .95, xanchor = "right", orientation = "h")) %>%
    config(displayModeBar = FALSE)

  plt$x$data[[3]]$text <- plt$x$data[[3]]$text %>%
    str_replace("-?\\d{1}\\.\\d{5,}", function(x) round(as.numeric(x), 3))
  plt$x$data[[3]]$name <- str_replace(plt$x$data[[3]]$name, ",", ", ")

  plt$x$data[[4]]$text <- plt$x$data[[4]]$text %>%
    str_replace("-?\\d{1}\\.\\d{5,}", function(x) round(as.numeric(x), 3))
  plt$x$data[[4]]$name <- str_replace(plt$x$data[[4]]$name, ",", ", ")

  if (method == "both") {
    plt$x$data[[5]]$text <- plt$x$data[[5]]$text %>%
      str_replace("-?\\d{1}\\.\\d{5,}", function(x) round(as.numeric(x), 3))
    plt$x$data[[5]]$name <- str_replace(plt$x$data[[5]]$name, ",", ", ")

    plt$x$data[[6]]$text <- plt$x$data[[6]]$text %>%
      str_replace("-?\\d{1}\\.\\d{5,}", function(x) round(as.numeric(x), 3))
    plt$x$data[[6]]$name <- str_replace(plt$x$data[[6]]$name, ",", ", ")
  }

  plt$x$data[[1]][["hoverinfo"]] <- "none"
  plt$x$data[[2]][["text"]] <- switch(method,
    "fa" = "Kaiser boundary<br><br>",
    "pca" = "Kaiser boundary<br><br>",
    "both" = c(
      "Kaiser boundary for FA<br><br>",
      "Kaiser boundary for PCA<br><br>"
    )
  )
  plt$x$data[[2]][["hoverinfo"]] <- "none"

  plt
})

# ** DB scree plot ####
output$DB_scree_plot <- downloadHandler(
  filename = function() {
    paste("fig_ScreePlot.png", sep = "")
  },
  content = function(file) {
    ggsave(file,
      plot = validity_factor_parallel_analysis()[["data"]] %>% plot() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# number of factors - parallel analysis
validity_factor_number_pa <- reactive({
  x <- validity_factor_parallel_analysis()[["data"]]
  method <- ifelse(input$validity_factor_pa_method %in% c("fa", "both"), "fa", "pca")

  real_idx <- which(x$data_type == "real" & x$method == method)
  simulated_idx <- which(x$data_type == "simulated" & x$method == method)

  real_eigenvalues <- x$eigenvalue[real_idx]
  simulated_eigenvalues <- x$eigenvalue[simulated_idx]

  factors_below_thr <- which(!(real_eigenvalues > simulated_eigenvalues))

  max(factors_below_thr[1] - 1, 1)
})


output$validity_factor_number <- renderText({
  validity_factor_parallel_analysis()[["text"]] # captured fa_parallel() output
})

# update EFA corr. method - mimic the PA input
observeEvent(input$validity_factor_pa_cor, {
  sel <- switch(input$validity_factor_pa_cor,
    "pearson" = "cor",
    "polychoric" = "poly"
  )
  updateSelectInput(session, "validity_factor_cor_efa",
    selected = sel
  )
})

# update selected number of factors to extract from PA + max
observe({
  updateNumericInput(session, "validity_factor_nfactors",
    value = validity_factor_number_pa(),
    max = ncol(ordinal())
  )
})

# run FA
validity_factor_fa <- reactive({
  validity_factor_nonzero_vars_check()
  fa(ordinal(),
    input$validity_factor_nfactors,
    rotate = input$validity_factor_rotation,
    cor = input$validity_factor_cor_efa
  )
})

# unclassed loadings
validity_factor_loadings_unclassed <- reactive({
  loadings <- validity_factor_fa()$loadings
  loadings %>%
    unclass() %>%
    data.frame() %>%
    setNames(paste0("F", seq_len(ncol(loadings))))
})

# loadings with uniquenesess and cutoff
output$validity_factor_loadings <- renderTable(
  {
    loadings_num <- validity_factor_loadings_unclassed()
    n_factors <- ncol(loadings_num)
    n_items <- nrow(loadings_num)

    # sorting mimick those used by loadings() base R function
    if (input$validity_factor_sort) {
      mx <- max.col(abs(loadings_num))
      idx <- cbind(1:n_items, mx)
      mx[abs(loadings_num[idx]) < 0.5] <- n_factors + 1
      items_order <- order(mx, 1:n_items)
    }

    loadings <- format(round(loadings_num, 2), trim = TRUE)

    loadings[abs(loadings_num) < input$validity_factor_hide] <- ""

    uniqueness <- format(round(validity_factor_fa()$uniquenesses, 2), trim = TRUE)

    res <- data.frame(loadings, uniqueness)

    if (input$validity_factor_sort) {
      res <- res[items_order, ]
    }

    res
  },
  rownames = TRUE
)

# ** DB loadings data ####
output$DB_validity_factor_loadings <- downloadHandler(
  filename = function() {
    "efa_loadings.csv"
  },
  content = function(file) {
    data <- validity_factor_loadings_unclassed()
    write.csv(data, file)
  }
)



# variance explained table, factor summary
output$validity_factor_varex <- renderTable(
  {
    loadings <- validity_factor_loadings_unclassed()

    n_items <- nrow(loadings)

    ss_loadings <- colSums(loadings^2)

    res <- data.frame(
      `SS loadings` = ss_loadings,
      `% variance` = (ss_loadings / n_items) * 100,
      `% variance cumul.` = (cumsum(ss_loadings / n_items)) * 100,
      check.names = FALSE
    )

    if (ncol(loadings) == 1) {
      res["% variance cumul."] <- NULL
    }

    rownames(res) <- colnames(loadings)

    res
  },
  rownames = TRUE
)

output$validity_factor_efa_fit <- renderUI({
  r <- validity_factor_fa()
  withMathJax(
    HTML(paste0(
      "$\\chi^2$(", r$dof, ") = ", round(r$chi, 2), "; <em>p</em> = ", round(r$PVAL, 3), "<br><br>",
      "RMSEA = ", round(r$RMSEA[1], 3),
      ", 90% CI [", round(r$RMSEA[2], 3), ", ", round(r$RMSEA[3], 3), "]<br><br>",
      "TLI = ", round(r$TLI, 3), "; BIC = ", round(r$BIC, 3)
    ))
  )
})

validity_factor_fscores <- reactive({
  r <- validity_factor_fa()
  fscores <- psych::factor.scores(ordinal(), r, method = "Thurstone")$scores
  fscores %>%
    data.frame() %>%
    setNames(paste0("F", seq_len(ncol(fscores))))
})

output$validity_factor_fscores <- renderDT({
  fscores <- validity_factor_fscores()
  fscores %>%
    datatable(options = list(
      scrollX = TRUE,
      autoWidth = TRUE,
      columnDefs = list(list(width = "50px", targets = "_all")),
      pageLength = 10,
      server = TRUE,
      scrollCollapse = TRUE,
      dom = "tipr"
    ), style = "bootstrap") %>%
    formatRound(columns = seq_len(ncol(fscores)), digits = 3)
})

# ** DB factor scores data ####
output$DB_validity_factor_fscores <- downloadHandler(
  filename = function() {
    "efa_factor-scores.csv"
  },
  content = function(file) {
    data <- validity_factor_fscores()
    write.csv(data, file)
  }
)




# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * PREDICTIVE VALIDITY ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Updating item slider ####
observe({
  item_count <- ncol(ordinal())
  updateSliderInput(
    session = session,
    inputId = "validitydistractorSlider",
    max = item_count
  )
})

# ** DDplot with criterion validity AKA DCplot ####
DCplot <- reactive({
  correct <- ordinal()

  difc_type <- input$DCplot_difficulty
  average.score <- (difc_type == "AVGS")

  DDplot(correct,
    item.names = item_numbers(),
    average.score = average.score, criterion = unlist(criterion()),
    thr = switch(input$DCplotThr_cb,
      "TRUE" = input$DCplotThr,
      "FALSE" = NULL
    ),
    val_type = input$DCplot_validity
  )
})

# ** Output for Diif/Disr. plot with plotly ####
output$DCplot <- renderPlotly({
  p <- DCplot() %>%
    ggplotly(tooltip = c("item", "fill", "value", "yintercept"))

  # renaming/removing unnecessary text
  for (i in 1:2) {
    for (j in 1:length(p$x$data[[i]][["text"]])) {
      p$x$data[[i]][["text"]][j] %<>%
        str_remove_all(("parameter: |value: ")) %>%
        str_replace("item", "Item") %>%
        str_remove("(?<=\\.\\d{3}).*")
    }
    if (input$DCplotThr_cb == TRUE) {
      for (j in 1:length(p$x$data[[3]][["text"]])) {
        p$x$date[[3]][["text"]][j] %<>%
          str_replace("yintercept", "Threshold")
      }
    }
  }

  p %>% plotly::config(displayModeBar = F)
})

#** DB DC plot ####
output$DB_DCplot <- downloadHandler(
  filename = function() {
    paste("fig_difficulty-validity_plot.png", sep = "")
  },
  content = function(file) {
    ggsave(file,
      plot = DCplot() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)


# ** Validity boxplot ####
validity_plot_boxplot_Input <- reactive({
  ts <- total_score()
  cv <- unlist(criterion())

  df <- data.table(ts, cv = as.factor(cv))
  df <- df[complete.cases(df), ]

  set.seed(1)

  g <- ggplot(df, aes(y = ts, x = cv, fill = cv)) +
    geom_boxplot() +
    geom_jitter(shape = 16, position = position_jitter(0.2)) +
    scale_fill_brewer(palette = "Blues") +
    xlab("Criterion group") +
    ylab("Total score") +
    coord_flip() +
    theme_app()
  g
})

# ** Validity scatterplot ####
validity_plot_scatter_Input <- reactive({
  ts <- total_score()
  cv <- unlist(criterion())

  size <- as.factor(cv)
  levels(size) <- table(cv)
  size <- as.numeric(as.character(size))

  df <- data.table(ts, cv, size)
  df <- df[complete.cases(df), ]

  g <- ggplot(df, aes(y = cv, x = ts)) +
    geom_point(color = "black", aes(size = size)) +
    geom_smooth(
      method = lm, formula = "y ~ x",
      se = FALSE,
      color = "red",
      show.legend = FALSE
    ) +
    xlab("Total score") +
    ylab("Criterion variable") +
    theme_app() +
    theme(
      legend.justification = c(0.99, 0.01),
      legend.position = c(0.99, 0.01)
    )
  g
})

# ** Validity descriptive plot ####
validity_plot_Input <- reactive({
  cv <- criterion()

  ## this is fixed value to recognize discrete variable
  k <- 6
  if (length(unique(cv)) <= length(cv) / k) {
    g <- validity_plot_boxplot_Input()
  } else {
    g <- validity_plot_scatter_Input()
  }
  g
})

# ** Output validity descriptive plot ####
output$validity_plot <- renderPlotly({
  g <- validity_plot_Input()
  p <- ggplotly(g)

  for (i in 1:length(p$x$data)) {
    text <- p$x$data[[i]]$text
    text <- gsub("ts", "Total score", text)
    text <- gsub("cv", "Criterion variable", text)
    text <- gsub("size", "Count", text)
    text <- lapply(strsplit(text, split = "<br />"), unique)
    text <- unlist(lapply(text, paste, collapse = "<br />"))
    p$x$data[[i]]$text <- text
  }

  p$elementId <- NULL
  p %>% plotly::config(displayModeBar = FALSE)
})

# ** DB validity descriptive plot ####
output$DB_validity_plot <- downloadHandler(
  filename = function() {
    cv <- criterion()
    k <- 6
    type <- ifelse(length(unique(cv)) <= length(cv) / k, "boxplot", "scatterplot")
    paste("fig_CriterionVariable_", type, ".png", sep = "")
  },
  content = function(file) {
    ggsave(file,
      plot = validity_plot_Input() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Validity correlation table ####
validity_table_Input <- reactive({
  ts <- total_score()
  cv <- criterion()

  ct <- cor.test(ts, cv, method = "pearson")

  txt <- HTML(paste0(
    "<em>r</em>(",
    ct$parameter,
    ") = ",
    sub("^(-?)0.", "\\1.", sprintf("%.2f", ct$estimate)), ", <em>p</em> = ",
    ifelse(ct$p.value < .001, "<.001", sub("^(-?)0.", "\\1.", sprintf("%.3f", ct$p.value))), ", 95% CI [",
    sub("^(-?)0.", "\\1.", sprintf("%.2f", ct$conf.int[1])),
    ", ", sub("^(-?)0.", "\\1.", sprintf("%.2f", ct$conf.int[2])),
    "]"
  ))

  list(txt = txt, pval = ct$p.value, est = ct$estimate)
})

# * Output validity correlation table ####
output$validity_table <- renderUI({
  validity_table_Input()$txt
})

# ** Interpretation ####
output$validity_table_interpretation <- renderUI({
  tab <- validity_table_Input()
  pval <- tab$pval
  est <- tab$est

  txt1 <- paste("<b>", "Interpretation:", "</b>")
  txt2 <- ifelse(est > 0, "positively", "negatively")
  txt3 <- ifelse(pval < .05,
    paste("The <em>p</em>-value is less than .05, thus we reject the null hypotheses.
                       The total score and criterion variable are", txt2, "correlated."),
    "The <em>p</em>-value is larger than .05, thus we don't reject the null hypotheses.
                 We cannot conclude that a significant correlation between the total score
                 and criterion variable exists."
  )
  HTML(paste(txt1, txt3))
})

# ** Validity distractor text ####
output$validity_distractor_text <- renderUI({
  cv <- criterion()

  ## this is fixed value to recognize discrete variable
  k <- 6
  if (length(unique(cv)) <= length(cv) / k) {
    num.group <- length(levels(as.factor(cv)))
  } else {
    num.group <- input$validity_group
  }

  txt1 <- paste("Respondents are divided into ")
  txt2 <- ifelse((length(unique(cv)) <= length(cv) / k),
    paste("<b>", num.group, "</b> groups as it seems that criterion variable is discrete. "),
    paste("<b>", num.group, "</b> groups by their criterion variable. ")
  )
  txt3 <- paste("Subsequently, we display a percentage
                 of respondents in each group who selected a given answer (correct answer or distractor).
                 The correct answer should be more often selected by respondents with higher values of
                 the criterion variable than by those with lower values, i.e.")
  txt4 <- paste("<b>", "solid line should be increasing.", "</b>")
  txt5 <- paste("The distractor should work in the opposite direction, i.e. ")
  txt6 <- paste("<b>", "dotted lines should be decreasing.", "<b>")
  HTML(paste(txt1, txt2, txt3, txt4, txt5, txt6))
})

# ** Admisible groups for cut ####
validity_admisible_groups <- reactive({
  cv <- criterion()
  k <- 6

  if (length(unique(cv)) <= length(cv) / k) {
    groups <- length(levels(as.factor(cv)))
    validity_change_cut_indicator$discrete <- TRUE
  } else {
    cv_quant <- lapply(1:5, function(i) quantile(cv, seq(0, 1, by = 1 / i), na.rm = TRUE))
    cv_quant_unique <- sapply(cv_quant, function(i) !any(duplicated(i)))
    validity_change_cut_indicator$discrete <- FALSE
    groups <- c(1:5)[cv_quant_unique]
  }

  groups
})

# ** Status of changing cut ####
validity_change_cut_indicator <- reactiveValues(
  change = FALSE,
  discrete = FALSE
)

# ** Updating cut slider ####
observeEvent(!(input$validity_group %in% validity_admisible_groups()), {
  if (!(input$validity_group %in% validity_admisible_groups())) {
    validity_change_cut_indicator$change <- TRUE
    c <- max(validity_admisible_groups())
    updateSliderInput(session, "validity_group", value = c)
  }
})

# ** Warning for non-unique cut ####
output$validity_groups_alert <- renderUI({
  if (validity_change_cut_indicator$change) {
    if (validity_change_cut_indicator$discrete) {
      txt <- paste0(
        '<font color = "orange">The criterion seems to be discrete. The number of groups was set to ',
        validity_admisible_groups(), ".</font>"
      )
    } else {
      txt <- paste0('<font color = "orange">The cut of criterion variable was not unique. The maximum number of
                    groups, for which criterion variable is unique is ', max(validity_admisible_groups()), ".</font>")
    }
  } else {
    txt <- " "
  }
  HTML(txt)
})

# ** Validity distractors plot ####
validity_distractor_plot_Input <- reactive({
  i <- input$validitydistractorSlider

  plotDistractorAnalysis(
    Data = nominal(),
    key = key(),
    num.groups = input$validity_group,
    item = i,
    item.name = item_names()[i],
    multiple.answers = input$type_validity_combinations_distractor == "Combinations",
    criterion = criterion(),
    crit.discrete = validity_change_cut_indicator$discrete
  )
})

# ** Output validity distractors plot ####
output$validity_distractor_plot <- renderPlotly({
  g <- validity_distractor_plot_Input()
  p <- ggplotly(g)

  for (i in 1:length(p$x$data)) {
    text <- p$x$data[[i]]$text
    text <- lapply(strsplit(text, split = "<br />"), unique)
    text <- unlist(lapply(text, paste, collapse = "<br />"))
    p$x$data[[i]]$text <- text
  }

  p$elementId <- NULL
  p %>% plotly::config(displayModeBar = FALSE)
})

# ** DB validity distractors plot ####
output$DB_validity_distractor_plot <- downloadHandler(
  filename = function() {
    paste("fig_DistractorsValidityPlot.png", sep = "")
  },
  content = function(file) {
    ggsave(file,
      plot = validity_distractor_plot_Input() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Validity correlation table for items ####
validity_table_item_Input <- reactive({
  correct <- binary()
  cv <- criterion()
  i <- input$validitydistractorSlider

  ct <- cor.test(unlist(correct[, i, with = F]), cv, method = "pearson")

  txt <- HTML(paste0(
    "<em>r</em>(",
    ct$parameter,
    ") = ",
    sub("^(-?)0.", "\\1.", sprintf("%.2f", ct$estimate)), ", <em>p</em> = ",
    ifelse(ct$p.value < .001, "<.001", sub("^(-?)0.", "\\1.", sprintf("%.3f", ct$p.value))), ", 95% CI [",
    sub("^(-?)0.", "\\1.", sprintf("%.2f", ct$conf.int[1])),
    ", ", sub("^(-?)0.", "\\1.", sprintf("%.2f", ct$conf.int[2])),
    "]"
  ))

  list(txt = txt, pval = ct$p.value, est = ct$estimate)
})

# ** Output validity correlation table for items ####
output$validity_table_item <- renderUI({
  validity_table_item_Input()$txt
})

# ** Interpretation ####
output$validity_table_item_interpretation <- renderUI({
  tab <- validity_table_item_Input()
  pval <- tab$pval
  est <- tab$est
  i <- input$validitydistractorSlider

  txt1 <- paste("<b>", "Interpretation:", "</b>")
  txt2 <- ifelse(est > 0, "positively", "negatively")
  txt3 <- ifelse(pval < .05,
    paste("The <em>p</em>-value is less than .05, thus we reject the null hypotheses.
                         Scored item", i, "and criterion variable are", txt2, "correlated."),
    paste(
      "The <em>p</em>-value is larger than .05, thus we don't reject the null hypotheses.
                   We cannot conclude that a significant correlation between scored item", i,
      "and criterion variable exists."
    )
  )
  HTML(paste(txt1, txt3))
})

# ** Warning for missing values ####
output$corr_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})
