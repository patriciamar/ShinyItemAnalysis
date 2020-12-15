# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# DIF/FAIRNESS ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * TOTAL SCORES ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Variable selection - Totals OR DMV
DIF_total_matching <- reactive({
  switch(input$DIF_total_matching,
    "score" =  total_score(),
    # "zscore" = scale(apply(as.data.frame(total_score()), 1, sum)),
    "uploaded" = unlist(DIFmatching())
    # "zuploaded" = scale(apply(as.data.frame(unlist(DIFmatching())), 1, sum))
  )
})

# title update -- one reactive output cannot be displayed more than once for some reason...
output$DIF_total_matching_title1 <-
  output$DIF_total_matching_title2 <-
  output$DIF_total_matching_title3 <-
  output$DIF_total_matching_title4 <-
  output$DIF_total_matching_title5 <-
  output$DIF_total_matching_title6 <-
  renderText({
    ifelse(input$DIF_total_matching == "uploaded",
      "uploaded variable",
      "total scores"
    )
  })

# update input according to dataset
observe({
  if (dif_present() == TRUE) {
    updateSelectInput(
      session,
      "DIF_total_matching",
      choices = c(
        "Total score" = "score",
        # "Standardized total score" = "zscore",
        "Uploaded" = "uploaded"
        # "Standarized uploaded" = "zuploaded"
      ),
      selected = "score"
    )
  } else {
    updateSelectInput(
      session,
      "DIF_total_matching",
      choices = c(
        "Total score" = "score"
        # "Standardized total score" = "zscore"
      ),
      selected = "score"
    )
  }
})

# ** Summary of total scores for groups ######
DIF_total_table_Input <- reactive({
  sc_one <- DIF_total_matching()[group() == 1]
  sc_zero <- DIF_total_matching()[group() == 0]

  skewness <- function(x) {
    n <- length(x)
    (sum((x - mean(x, na.rm = TRUE))^3, na.rm = TRUE) / n) / (sum((x - mean(x, na.rm = TRUE))^2, na.rm = TRUE) / n)^(3 / 2)
  }
  kurtosis <- function(x) {
    n <- length(x)
    n * sum((x - mean(x, na.rm = TRUE))^4, na.rm = TRUE) / (sum((x - mean(x, na.rm = TRUE))^2, na.rm = TRUE)^2)
  }

  tab <- data.frame(rbind(
    round(c(
      length(sc_zero),
      min(sc_zero, na.rm = T),
      max(sc_zero, na.rm = T),
      mean(sc_zero, na.rm = T),
      median(sc_zero, na.rm = T),
      sd(sc_zero, na.rm = T),
      ShinyItemAnalysis:::skewness(sc_zero),
      ShinyItemAnalysis:::kurtosis(sc_zero)
    ), 2),
    round(c(
      length(sc_one),
      min(sc_one, na.rm = T),
      max(sc_one, na.rm = T),
      mean(sc_one, na.rm = T),
      median(sc_one, na.rm = T),
      sd(sc_one, na.rm = T),
      ShinyItemAnalysis:::skewness(sc_one),
      ShinyItemAnalysis:::kurtosis(sc_one)
    ), 2)
  ))
  colnames(tab) <- c("n", "Min", "Max", "Mean", "Median", "SD", "Skewness", "Kurtosis")
  tab$n <- as.integer(tab$n)
  rownames(tab) <- c("Reference group (0)", "Focal group (1)")
  tab
})

output$DIF_total_table <- renderTable(
  {
    DIF_total_table_Input()
  },
  digits = 2,
  include.rownames = T,
  include.colnames = T
)

# ** Histogram of total score for group = 1 (focal) ######
DIF_total_hist_Input <- reactive({
  group <- group()
  match <- DIF_total_matching()
  xlab <- switch(input$DIF_total_matching,
    "score" =  "Total score",
    "uploaded" = "Matching criterion"
  )

  df <- data.table(
    score = match,
    group = as.factor(group)
  )

  g <- ggplot(data = df, aes(x = score, fill = group, col = group)) +
    geom_histogram(binwidth = 1, position = "dodge2", alpha = 0.75) +
    xlab(xlab) +
    ylab("Number of respondents") +
    scale_fill_manual(values = c("dodgerblue2", "goldenrod2"), labels = c("Reference", "Focal")) +
    scale_colour_manual(values = c("dodgerblue2", "goldenrod2"), labels = c("Reference", "Focal")) +
    theme_app()
  g
})

output$DIF_total_hist <- renderPlotly({
  g <- DIF_total_hist_Input()
  p <- ggplotly(g)

  nam <- switch(
    input$DIF_total_matching,
    "score" = "Score",
    # "zscore" = "Standardized score",
    "uploaded" = "Uploaded matching"
    # "zuploaded" = "Standardized uploaded matchhing"
  )

  txt1.1 <- gsub(
    "count:", "Count:",
    lapply(
      strsplit(p$x$data[[1]]$text, split = "<br />"),
      function(x) x[1]
    )
  )
  txt1.2 <- paste(
    paste0(nam, ":"),
    sapply(
      strsplit(p$x$data[[1]]$text, split = "score: "),
      function(x) ceiling(as.numeric(paste(x[2])))
    )
  )
  txt1 <- paste(txt1.1, txt1.2, "Group: Reference", sep = "<br />")
  p$x$data[[1]]$text <- txt1

  txt2.1 <- gsub(
    "count:", "Count:",
    lapply(
      strsplit(p$x$data[[2]]$text, split = "<br />"),
      function(x) x[1]
    )
  )
  txt2.2 <- paste(
    paste0(nam, ":"),
    sapply(
      strsplit(p$x$data[[2]]$text, split = "score: "),
      function(x) floor(as.numeric(paste(x[2])))
    )
  )
  txt2 <- paste(txt2.1, txt2.2, "Group: Focal", sep = "<br />")
  p$x$data[[2]]$text <- txt2


  p %>% plotly::config(displayModeBar = F)
})

output$DB_DIF_total_hist <- downloadHandler(
  filename = function() {
    paste("fig_HistogramGroups.png", sep = "")
  },
  content = function(file) {
    ggsave(file,
      plot = DIF_total_hist_Input() +
        theme(
          text = element_text(size = setting_figures$text_size),
          legend.position = c(0.15, 0.85)
        ),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** t-test to compare total scores ######
DIF_total_ttest_Input <- reactive({
  sc0 <- DIF_total_matching()[group() == 0]
  sc1 <- DIF_total_matching()[group() == 1]

  ttest <- t.test(sc0, sc1)

  tab <- c(
    paste0(
      sprintf("%.2f", mean(sc0, na.rm = T) - mean(sc1, na.rm = T)), " (",
      sprintf("%.2f", ttest$conf.int[1]), ", ",
      sprintf("%.2f", ttest$conf.int[2]), ")"
    ),
    sprintf("%.2f", ttest$statistic),
    sprintf("%.2f", ttest$parameter),
    ifelse(ttest$p.value < 0.001, "< 0.001", sprintf("%.3f", ttest$p.value))
  )
  tab <- t(as.data.frame(tab))
  colnames(tab) <- c("Diff. (CI)", "t-value", "df", "p-value")
  tab
})

output$DIF_total_ttest <- renderTable({
  DIF_total_ttest_Input()
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * DELTA PLOT ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

deltaGpurn <- reactive({
  data <- data.table(binary(), group = group())
  switch(input$type_threshold,
    "Fixed" = deltaPlot(data,
      group = "group",
      focal.name = 1,
      thr = 1.5,
      purify = input$puri_DP
      # purType = input$puri_DP_type
      # (only 1IPP is used, so no need to pass this arg)
    ),
    "Normal" = deltaPlot(data,
      group = "group",
      focal.name = 1,
      thr = "norm",
      purify = input$puri_DP,
      purType = input$puri_DP_type
    )
  )
})

deltaGpurn_report <- reactive({
  data <- data.table(binary(), group = group())
  if (!input$customizeCheck) {
    type_threshold_report <- input$type_threshold
    purify_report <- input$puri_DP
    purType_report <- input$puri_DP_type
  } else {
    type_threshold_report <- input$type_threshold_report
    purify_report <- input$puri_DP_report
    purType_report <- input$puri_DP_type_report
  }

  switch(type_threshold_report,
    "Fixed" = deltaPlot(data,
      group = "group",
      focal.name = 1,
      thr = 1.5,
      purify = purify_report
      # purType = purType_report
    ),
    "Normal" = deltaPlot(data,
      group = "group",
      focal.name = 1,
      thr = "norm",
      purify = purify_report,
      purType = purType_report
    )
  )
})

# * Delta plot ######
deltaplotInput <- reactive({
  dp <- deltaGpurn()
  df <- data.frame(dp$Deltas)
  df$nam <- item_numbers()

  par <- dp$axis.par
  thr <- dp$thr

  if (length(par) > 2) {
    par <- par[length(par) / 2, ]
  }

  if (length(thr) > 1) {
    thr <- thr[length(thr)]
  }

  p <- ggplot(
    df,
    aes(x = X1, y = X2, label = nam)
  ) +
    geom_point() +
    geom_text(hjust = 0, nudge_x = 0.05, size = 6) +
    geom_abline(
      intercept = par[1], slope = par[2],
      size = 1
    ) +
    geom_abline(
      intercept = par[1] + thr * sqrt(par[2]^2 + 1),
      slope = par[2],
      color = "red",
      linetype = "dashed",
      size = 1
    ) +
    geom_abline(
      intercept = par[1] - thr * sqrt(par[2]^2 + 1),
      slope = par[2],
      color = "red",
      linetype = "dashed",
      size = 1
    ) +
    labs(
      x = "Reference group",
      y = "Focal group"
    ) +
    xlim(min(dp$Deltas, na.rm = T) - 0.5, max(dp$Deltas, na.rm = T) + 0.5) +
    ylim(min(dp$Deltas, na.rm = T) - 0.5, max(dp$Deltas, na.rm = T) + 0.5) +
    theme_app()
  if (is.numeric(dp$DIFitems)) {
    df2 <- df[dp$DIFitems, ]
    p <- p + geom_point(
      data = df2,
      aes(x = X1, y = X2, label = nam),
      size = 8, color = "black", shape = 1
    )
  }
  p <- p + ggtitle("Delta plot")
  p
})

deltaplotInput_report <- reactive({
  dp <- deltaGpurn_report()
  df <- data.frame(dp$Deltas)
  df$nam <- item_numbers()

  par <- dp$axis.par
  thr <- dp$thr

  if (length(par) > 2) {
    par <- par[length(par) / 2, ]
  }

  if (length(thr) > 1) {
    thr <- thr[length(thr)]
  }

  p <- ggplot(
    df,
    aes(x = X1, y = X2, label = nam)
  ) +
    geom_point() +
    geom_text(hjust = 0, nudge_x = 0.05) +
    geom_abline(
      intercept = par[1], slope = par[2],
      size = 1
    ) +
    geom_abline(
      intercept = par[1] + thr * sqrt(par[2]^2 + 1),
      slope = par[2],
      color = "red",
      linetype = "dashed",
      size = 1
    ) +
    geom_abline(
      intercept = par[1] - thr * sqrt(par[2]^2 + 1),
      slope = par[2],
      color = "red",
      linetype = "dashed",
      size = 1
    ) +
    labs(
      x = "Reference group",
      y = "Focal group"
    ) +
    xlim(min(dp$Deltas, na.rm = T) - 0.5, max(dp$Deltas, na.rm = T) + 0.5) +
    ylim(min(dp$Deltas, na.rm = T) - 0.5, max(dp$Deltas, na.rm = T) + 0.5) +
    theme_app()

  if (is.numeric(dp$DIFitems)) {
    df2 <- df[dp$DIFitems, ]
    p <- p + geom_point(
      data = df2,
      aes(x = X1, y = X2, label = nam),
      size = 6, color = "black", shape = 1
    )
  }
  p <- p + ggtitle("Delta plot")
  p
})

output$deltaplot <- renderPlot({
  deltaplotInput()
})

output$DP_deltaplot <- downloadHandler(
  filename = function() {
    paste("fig_DeltaPlot.png", sep = "")
  },
  content = function(file) {
    ggsave(file,
      plot = deltaplotInput() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# Output
output$dp_text_normal <- renderPrint({
  deltaGpurn()
})

# Output table for LAST iteration
dp_table <- reactive({
  res <- deltaGpurn()
  # from print.deltaPlot:
  tab <- data.frame(round(cbind(res$Props, res$Deltas, res$Dist[, ncol(res$Dist)]), 4))
  symb <- symnum(abs(as.numeric(res$Dist[, ncol(res$Dist)])),
    c(0, abs(res$thr[length(res$thr)]), Inf),
    symbols = c("", "***")
  )
  tab$symb <- symb

  colnames(tab) <- c(
    "Prop. correct %%mathit{_{ref}}%%", "Prop. correct %%mathit{_{foc}}%%",
    "%%mathit{\\Delta_{ref}}%%", "%%mathit{\\Delta_{foc}}%%",
    "Distance", ""
  )
  rownames(tab) <- item_names()

  tab
})

output$coef_dp_table <- renderTable(
  {
    dp_table()
  },
  rownames = T,
  colnames = T
)

# ** Items detected text ######
output$dp_dif_items <- renderPrint({
  DIFitems <- deltaGpurn()$DIFitems
  if (DIFitems[1] == "no DIF item detected") {
    txt <- "No item was detected as DIF."
  } else {
    txt <- paste0("Items detected as DIF items: ", paste(item_names()[DIFitems], collapse = ", "))
  }
  HTML(txt)
})

# ** Purification table ######
dp_puri_table <- reactive({
  model <- deltaGpurn()
  tab <- model$difPur

  if (!is.null(tab)) {
    colnames(tab) <- item_names()
    rownames(tab) <- paste0("Step ", seq(0, nrow(tab) - 1))
    tab
  }
})

output$dp_puri_table <- renderTable(
  {
    dp_puri_table()
  },
  rownames = T,
  colnames = T,
  digits = 0
)

# ** Purification info - number of iter ######
output$dp_puri_info <- renderPrint({
  model <- deltaGpurn()
  if (input$puri_DP & !is.null(deltaGpurn()$difPur)) {
    cat("Table below describes purification process, where rows correspond to purification iteration and columns to items.
        Value of '1' in the i-th row means that an item was detected as DIF in (i-1)-th step, while value of '0' means that
        item was not detected as DIF. The first row corresponds to the initial classification of the items when all items
        were used for calculation of DIF matching criterion. ")
    nrIter <- model$nrIter - 1
    cat(
      "In this case, the convergence was", ifelse(model$convergence, "reached", "NOT reached even"), "after", nrIter,
      ifelse(nrIter == 1, "iteration.", "iterations.")
    )
  } else if (input$puri_DP & is.null(deltaGpurn()$difPur)) {
    cat("No DIF items detected whatsoever, nothing to show.")
  } else {
    cat("Item purification not requested! Nothing to show.")
  }
})

# Note setup
note_dp <- reactive({
  res <- NULL

  model <- deltaGpurn()
  thr <- model$thr
  thr <- thr[length(thr)]

  axis <- round(model$axis.par, 2)
  last <- nrow(axis)
  if (is.null(last)) {
    a <- axis[1]
    b <- axis[2]
  } else {
    a <- axis[last, 1]
    b <- axis[last, 2]
  }

  res$axes <- paste0("Parameters of the major axis (last iteration): a = ", a, ", b = ", b)
  res$puri <- paste("Item purification:", ifelse("purType" %in% names(model), model$purType, "unutilized"))

  res$thr <-
    paste0(
      "Detection threshold (last iteration): ",
      round(thr, 3),
      ifelse(thr == 1.5, " (fixed)", " (normal aproximation)")
    )

  res
})

output$note_dp <- renderUI({
  withMathJax()
  HTML(
    paste(
      "Notes:",
      note_dp()$puri,
      note_dp()$axes,
      note_dp()$thr,
      "Items detected as DIF are flagged with '***'",
      sep = "</br>"
    )
  )
})

# ** Download tables ######
output$download_dp_table <- downloadHandler(
  filename = function() {
    paste("DIF_DP_statistics", ".csv", sep = "")
  },
  content = function(file) {
    data <- dp_table()
    colnames(data) <- c(
      "Ref. prop. correct",
      "Foc. prop. correct",
      "Delta (Ref.)",
      "Delta (Foc.)",
      "Distance",
      ""
    )

    write.csv(data, file)
    write(paste(
      "Note:",
      note_dp()$puri,
      note_dp()$axes,
      note_dp()$thr,
      "Items detected as DIF are flagged with '***'",
      sep = "\n"
    ), file, append = T)
  }
)
output$download_dp_puri <- downloadHandler(
  filename = function() {
    paste0("DIF_DP_purification", ".csv")
  },
  content = function(file) {
    data <- dp_puri_table()
    write.csv(data, file)
  }
)

########################################

# Central DIF matching variable (DMV) presence control ######
dif_present <-
  reactive({
    !(length(dataset$DIFmatching) == 1 &
      any(dataset$DIFmatching == "missing"))
  })

# create vectors with DMV and purification inputs names
# (to feed "lapplies" in respective method section)
match_logistic <- c("DIF_logistic_summary_matching", "DIF_logistic_items_matching")
puri_logistic <- c("DIF_logistic_summary_purification", "DIF_logistic_items_purification")

match_NLR <- c("DIF_NLR_summary_matching", "DIF_NLR_items_matching")
puri_NLR <- c("DIF_NLR_purification_print", "DIF_NLR_purification_plot")

match_cum <- c("DIF_cum_summary_matching", "DIF_cum_items_matching")
puri_cum <- c("DIF_cum_purification_summary", "DIF_cum_purification_items")

match_adj <- c("DIF_adj_summary_matching", "DIF_adj_items_matching")
puri_adj <- c("DIF_adj_purification_summary", "DIF_adj_purification_items")

match_multi <- c("DIF_multi_summary_matching", "DIF_multi_items_matching")
puri_multi <- c("DDF_multi_purification_summary", "DDF_multi_purification_items")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * MANTEL-HAENSZEL ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** UPDATING INPUTS ######

# ** Updating item and score sliders ######
observe({
  item_count <- ncol(binary())

  updateSliderInput(
    session = session,
    inputId = "DIF_MH_items_item",
    max = item_count
  )
  updateSliderInput(
    session = session,
    inputId = "DIF_MH_items_score",
    max = item_count,
    value = median(total_score(), na.rm = T)
  )
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** MODEL ######

# ** Model ######
DIF_MH_model <- reactive({
  group <- unlist(group())
  data <- data.frame(binary())

  fit <- .difMH_edited(
    Data = data, group = group, focal.name = 1,
    p.adjust.method = input$DIF_MH_summary_correction,
    purify = input$DIF_MH_summary_purification
  )
  fit
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** SUMMARY ######

# ** Output print ######
output$DIF_MH_summary_print <- renderPrint({
  print(DIF_MH_model())
})

# Summary and purification tables
mh_table <- reactive({
  res <- DIF_MH_model()
  deltaMH <- -2.35 * log(res$alphaMH)
  effsize <- symnum(abs(deltaMH), c(0, 1, 1.5, Inf), symbols = c("A", "B", "C"))
  blank <- character(length(res$alphaMH))

  pval <- if (res$p.adjust.method == "none") {
    round(res$p.value, 2)
  } else {
    round(res$adjusted.p, 2)
  }
  symb <- symnum(pval,
    c(0, 0.001, 0.01, 0.05, 0.1, 1),
    symbols = c("***", "**", "*", ".", "")
  )

  tab <-
    data.frame(
      round(res$MH, 2),
      pval,
      symb,
      blank,
      res$alphaMH,
      deltaMH,
      effsize
    )

  colnames(tab) <-
    c(
      "MH (%%mathit{\\chi^2}%%)",
      ifelse(
        res$p.adjust.method == "none",
        "%%mathit{p}%%-value",
        "adj. %%mathit{p}%%-value"
      ),
      "",
      "",
      "%%mathit{\\alpha}_{\\mathrm{MH}}%%",
      "%%mathit{\\Delta}_{\\mathrm{MH}}%%",
      ""
    )

  rownames(tab) <- item_names()

  tab
})

output$coef_mh_table <- renderTable(
  {
    mh_table()
  },
  rownames = T,
  colnames = T
)

# ** Items detected text ######
output$mh_dif_items <- renderPrint({
  DIFitems <- DIF_MH_model()$DIFitems
  if (DIFitems[1] == "No DIF item detected") {
    txt <- "No item was detected as DIF."
  } else {
    txt <- paste0("Items detected as DIF items: ", paste(item_names()[DIFitems], collapse = ", "))
  }
  HTML(txt)
})

# ** Purification table ######
mh_puri_table <- reactive({
  model <- DIF_MH_model()
  tab <- model$difPur

  if (!is.null(tab)) {
    colnames(tab) <- item_names()
    rownames(tab) <- paste0("Step ", seq(0, nrow(tab) - 1))
    tab
  }
})

output$mh_puri_table <- renderTable(
  {
    mh_puri_table()
  },
  rownames = T,
  colnames = T,
  digits = 0
)

# ** Purification info - number of iter ######
output$mh_puri_info <- renderPrint({
  model <- DIF_MH_model()
  if (input$DIF_MH_summary_purification & !is.null(DIF_MH_model()$difPur)) {
    cat("Table below describes purification process, where rows correspond to purification iteration and columns to items.
        Value of '1' in the i-th row means that an item was detected as DIF in (i-1)-th step, while value of '0' means that
        item was not detected as DIF. The first row corresponds to the initial classification of the items when all items
        were used for calculation of DIF matching criterion. ")
    nrIter <- model$nrPur
    cat(
      "In this case, the convergence was", ifelse(model$convergence, "reached", "NOT reached even"), "after", nrIter,
      ifelse(nrIter == 1, "iteration.", "iterations.")
    )
  } else if (input$DIF_MH_summary_purification & is.null(DIF_MH_model()$difPur)) {
    cat("No DIF items detected whatsoever, nothing to show.")
  } else {
    cat("Item purification not requested! Nothing to show.")
  }
})

# Note setup
note_mh <- reactive({
  res <- NULL

  model <- DIF_MH_model()
  thr <- DIF_MH_model()$thr

  res$p_adj <- paste("P-value correction method:", switch(
    model$p.adjust.method,
    "BH" = "Benjamini-Hochberg",
    "BY" = "Benjamini-Yekutieli",
    "bonferroni" = "Bonferroni",
    "holm" = "Holm",
    "hochberg" = "Hochberg",
    "hommel" = "Hommel",
    "none" = "none"
  ))

  res$puri <- paste("Item purification:", ifelse(model$purification == T, "used", "unutilized"))

  res$thr_rounded <- paste("Detection threshold:", round(thr[length(thr)], 3))

  res
})

output$note_mh <- renderUI({
  withMathJax()

  HTML(
    paste(
      "Notes:",
      note_mh()$p_adj,
      note_mh()$puri,
      note_mh()$thr_rounded,
      "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1",
      "DIF effect sizes (ETS Delta scale):
      <i>A</i> = negligible effect; <i>B</i> = moderate effect; <i>C</i> = large effect<br>
      Effect size codes (for absolute values of 'MH delta'): 0 'A' 1.0 'B' 1.5 'C'",
      sep = "</br>"
    )
  )
})

# ** Download tables ######
output$download_mh_table <- downloadHandler(
  filename = function() {
    paste("DIF_MH_statistics", ".csv", sep = "")
  },
  content = function(file) {
    data <- mh_table()
    if ("adj. %%mathit{p}%%-value" %in% colnames(data)) {
      colnames(data) <- c("MH (chi-square)", "adj. p-value", "Adj. p-value", "MH alpha", "MH delta", "Effect size", "")
    } else {
      colnames(data) <- c("MH (chi-square)", "p-value", "Adj. p-value", "MH alpha", "MH delta", "Effect size", "")
    }

    write.csv(data[, -4], file)
    write(paste(
      "Note:",
      note_mh()$p_adj,
      note_mh()$puri,
      note_mh()$thr_rounded,
      "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1",
      "DIF effect sizes (ETS Delta scale):
      A = negligible effect; B = moderate effect; C = large effect",
      "Effect size codes (for absolute values of 'MH delta'): 0 'A' 1.0 'B' 1.5 'C'",
      sep = "\n"
    ), file, append = T)
  }
)
output$download_mh_puri <- downloadHandler(
  filename = function() {
    paste0("DIF_MH_purification", ".csv")
  },
  content = function(file) {
    data <- mh_puri_table()
    write.csv(data, file)
  }
)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** ITEMS ######

# ** Contingency tables ######
DIF_MH_items_table <- reactive({
  group <- unlist(group())
  data <- data.frame(binary())
  total <- total_score()
  item <- input$DIF_MH_items_item
  score <- input$DIF_MH_items_score

  df <- data.frame("Answer" = data[, item], "Group" = group)
  df$Answer <- relevel(
    factor(df$Answer, labels = c("Incorrect", "Correct")),
    "Correct"
  )
  df$Group <- factor(df$Group, labels = c("Reference group", "Focal group"))

  df <- df[total == score, ]

  tab <- xtabs(~ Group + Answer, data = df) %>%
    addmargins(FUN = list(Total = sum), quiet = TRUE) %>%
    as.data.frame.matrix()

  tab
})

# ** Contingency tables output ######
output$DIF_MH_items_table <- renderTable(
  {
    DIF_MH_items_table()
  },
  rownames = TRUE,
  digits = 0
)

# ** OR calculation ######
output$DIF_MH_items_interpretation <- renderUI({
  tab <- DIF_MH_items_table()
  a <- tab[1, 1]
  b <- tab[1, 2]
  c <- tab[2, 1]
  d <- tab[2, 2]
  OR <- round((a * d) / (b * c), 2)

  item <- input$DIF_MH_items_item
  score <- input$DIF_MH_items_score

  alphaMH <- round(DIF_MH_model()$alphaMH[item], 2)
  deltaMH <- -2.35 * log(alphaMH)
  effect_size <- symnum(abs(deltaMH), cutpoints = c(0, 1, 1.5, Inf), symbols = LETTERS[1:3])

  txt <- ifelse((b * c == 0) | (a * d == 0), "Odds ratio cannot be calculated!",
    paste0(
      "For respondent who reached total score of ", score,
      " the odds of answering item ", item_numbers()[item],
      " correctly is ",
      ifelse(OR == 1, " the same for both groups. ",
        ifelse(OR > 1,
          paste0(OR, " times higher in the reference group than in the focal group."),
          paste0(OR, " times lower in the reference group than in the focal group.")
        )
      )
    )
  )

  txtMH <- paste0(
    "Mantel-Haenszel estimate of odds ratio accounting for all levels of total score is equal to ",
    alphaMH, ". The odds of answering item ", item_numbers()[item],
    " correctly is ",
    ifelse(alphaMH == 1, " the same for both groups. ",
      ifelse(alphaMH > 1,
        paste0(alphaMH, " times higher in the reference group than in the focal group."),
        paste0(alphaMH, " times lower in the reference group than in the focal group.")
      )
    )
  )
  txtDelta <- paste0(
    "Mantel-Haenszel D-DIF index is equal to ", round(deltaMH, 2), ". This indicates category ", effect_size,
    " DIF effect size - ",
    switch(effect_size,
      "A" = "negligible",
      "B" = "moderate",
      "C" = "large"
    ), "."
  )
  withMathJax(HTML(paste(
    sprintf(
      paste("$$\\mathrm{OR} = \\frac{%d \\cdot %d}{%d \\cdot %d} = %.2f$$", txt),
      a, d, b, c, OR
    ), "<br/><br/>",
    sprintf(
      paste("$$\\alpha_{\\mathrm{MH}} = %.2f$$", txtMH),
      alphaMH
    ), "<br/><br/>",
    sprintf(
      paste("$$\\Delta_{\\mathrm{MH}} = -2.35 \\cdot \\log(\\alpha_{\\mathrm{MH}}) = %.2f$$", txtDelta),
      deltaMH
    )
  )))
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** REPORT ######

# ** Model for report ######
report_DIF_MH_model <- reactive({
  group <- unlist(group())
  data <- data.frame(binary())

  if (!input$customizeCheck) {
    fit <- DIF_MH_model()
  } else {
    p.adjust.method_report <- input$correction_method_MH_report
    purify_report <- input$puri_MH_report

    fit <- .difMH_edited(
      Data = data, group = group, focal.name = 1,
      p.adjust.method = p.adjust.method_report,
      purify = purify_report
    )
  }
  fit
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * LOGISTIC ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** UPDATING INPUTS ######

observe({
  if (dif_present() == TRUE) {
    lapply(match_logistic, function(i) {
      updateSelectInput(
        session,
        paste0(i),
        choices = c(
          "Total score" = "score",
          "Standardized total score" = "zscore",
          "Uploaded" = "uploaded",
          "Standardized uploaded" = "zuploaded"
        ),
        selected = "zscore"
      )
    })
  } else {
    lapply(match_logistic, function(i) {
      updateSelectInput(
        session,
        paste0(i),
        choices = c(
          "Total score" = "score",
          "Standardized total score" = "zscore"
        ),
        selected = "zscore"
      )
    })
  }
})

mapply(function(match, puri) {
  observeEvent(input[[paste0(match)]], {
    if (input[[paste0(match)]] %in% c("uploaded", "zuploaded")) {
      updateCheckboxInput(session, paste0(puri), value = FALSE)
      shinyjs::disable(paste0(puri))
    } else {
      shinyjs::enable(paste0(puri))
    }
  })
},
match = match_logistic, puri = puri_logistic
)

DIF_logistic <- reactiveValues(
  type = NULL,
  correction = NULL,
  purification = NULL,
  matching = NULL
)

# ** Updating type ######
observeEvent(input$DIF_logistic_summary_type, {
  DIF_logistic$type <- input$DIF_logistic_summary_type
})
observeEvent(input$DIF_logistic_items_type, {
  DIF_logistic$type <- input$DIF_logistic_items_type
})
observeEvent(DIF_logistic$type, {
  if (DIF_logistic$type != input$DIF_logistic_summary_type) {
    updateCheckboxGroupInput(
      session = session,
      inputId = "DIF_logistic_summary_type",
      selected = DIF_logistic$type
    )
  }
  if (DIF_logistic$type != input$DIF_logistic_items_type) {
    updateCheckboxGroupInput(
      session = session,
      inputId = "DIF_logistic_items_type",
      selected = DIF_logistic$type
    )
  }
})

# ** Updating correction ######
observeEvent(input$DIF_logistic_summary_correction, {
  DIF_logistic$correction <- input$DIF_logistic_summary_correction
})
observeEvent(input$DIF_logistic_items_correction, {
  DIF_logistic$correction <- input$DIF_logistic_items_correction
})
observeEvent(DIF_logistic$correction, {
  if (DIF_logistic$correction != input$DIF_logistic_summary_correction) {
    updateSelectInput(
      session = session,
      inputId = "DIF_logistic_summary_correction",
      selected = DIF_logistic$correction
    )
  }
  if (DIF_logistic$correction != input$DIF_logistic_items_correction) {
    updateSelectInput(
      session = session,
      inputId = "DIF_logistic_items_correction",
      selected = DIF_logistic$correction
    )
  }
})

# ** Updating purification ######
observeEvent(input$DIF_logistic_summary_purification, {
  DIF_logistic$purification <- input$DIF_logistic_summary_purification
})
observeEvent(input$DIF_logistic_items_purification, {
  DIF_logistic$purification <- input$DIF_logistic_items_purification
})
observeEvent(DIF_logistic$purification, {
  if (DIF_logistic$purification != input$DIF_logistic_summary_purification) {
    updateCheckboxInput(
      session = session,
      inputId = "DIF_logistic_summary_purification",
      value = DIF_logistic$purification
    )
  }
  if (DIF_logistic$purification != input$DIF_logistic_items_purification) {
    updateCheckboxInput(
      session = session,
      inputId = "DIF_logistic_items_purification",
      value = DIF_logistic$purification
    )
  }
})

# ** Updating DMV ######
observeEvent(input$DIF_logistic_summary_matching, {
  DIF_logistic$matching <- input$DIF_logistic_summary_matching
})
observeEvent(input$DIF_logistic_items_matching, {
  DIF_logistic$matching <- input$DIF_logistic_items_matching
})
observeEvent(DIF_logistic$matching, {
  if (DIF_logistic$matching != input$DIF_logistic_summary_matching) {
    updateCheckboxInput(
      session = session,
      inputId = "DIF_logistic_summary_matching",
      value = DIF_logistic$matching
    )
  }
  if (DIF_logistic$matching != input$DIF_logistic_items_matching) {
    updateCheckboxInput(
      session = session,
      inputId = "DIF_logistic_items_matching",
      value = DIF_logistic$matching
    )
  }
})

# ** Updating item slider ######
observe({
  item_count <- ncol(binary())
  updateSliderInput(
    session = session,
    inputId = "DIF_logistic_items_item",
    max = item_count
  )
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** MODEL ######

# ** Model ######
DIF_logistic_model <- reactive({
  group <- unlist(group())
  data <- data.frame(binary())

  if (input$DIF_logistic_summary_matching == "uploaded") {
    match <- unlist(DIFmatching())
  } else if (input$DIF_logistic_summary_matching == "zuploaded") {
    match <- scale(apply(as.data.frame(unlist(DIFmatching())), 1, sum))
  } else if (input$DIF_logistic_summary_matching == "zscore") {
    match <- "zscore"
  } else if (input$DIF_logistic_summary_matching == "score") {
    match <- "score"
  }

  fit <- tryCatch(.difLogistic_edited(
    Data = data, group = group, match = match, focal.name = 1,
    type = input$DIF_logistic_summary_type,
    p.adjust.method = input$DIF_logistic_summary_correction,
    purify = input$DIF_logistic_summary_purification
  ),
  error = function(e) e
  )

  validate(need(
    class(fit) == "Logistic",
    paste0("This method cannot be used on this data. Error returned: ", fit$message)
  ))

  fit
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** SUMMARY ######

# ** Output print ######
# output$DIF_logistic_summary_print <- renderPrint({
#   print(DIF_logistic_model())
# })

# ** Warning for missing values ####
output$DIF_logistic_summary_NA_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})

# ** DIF statistic and parameter tables ####
coef_logistic_dif <- reactive({
  model <- DIF_logistic_model()

  stat <- model$Logistik

  pval <- if (model$p.adjust.method == "none") {
    model$p.value
  } else {
    model$adjusted.p
  }

  pval_symb <- symnum(pval,
    c(0, 0.001, 0.01, 0.05, 0.1, 1),
    symbols = c("***", "**", "*", ".", "")
  )
  blank <- character(length(model$names))
  r2 <- model$deltaR2
  zt <- symnum(r2, c(0, 0.13, 0.26, 1),
    symbols = c("A", "B", "C")
  )
  jg <- symnum(r2, c(0, 0.035, 0.07, 1),
    symbols = c("A", "B", "C")
  )

  tab <- data.frame(
    formatC(stat, format = "f", digits = 3),
    # round is ignored by renderTable, but formatC locks decimals in downloadTable on the other hand...
    formatC(pval, format = "f", digits = 3),
    pval_symb,
    blank,
    formatC(r2, format = "f", digits = 3),
    zt,
    jg,
    blank,
    model$logitPar[, 1],
    model$logitSe[, 1],
    model$logitPar[, 2],
    model$logitSe[, 2],
    model$logitPar[, 3],
    model$logitSe[, 3],
    model$logitPar[, 4],
    model$logitSe[, 4]
  )

  colnames(tab) <-
    c(
      "LR (%%mathit{\\chi^2}%%)",
      "%%mathit{p}%%-value",
      "",
      "",
      "%%mathit{R^2}%%",
      "ZT",
      "JG",
      "",
      "%%mathit{b_0}%%",
      "SE(%%mathit{b_0}%%)",
      "%%mathit{b_1}%%",
      "SE(%%mathit{b_1}%%)",
      "%%mathit{b_2}%%",
      "SE(%%mathit{b_2}%%)",
      "%%mathit{b_3}%%",
      "SE(%%mathit{b_3}%%)"
    )

  if (model$p.adjust.method != "none") {
    colnames(tab) <-
      gsub(
        "%%mathit\\{p\\}%%-value",
        "adj. %%mathit{p}%%-value",
        colnames(tab)
      )
  }

  rownames(tab) <- item_names()

  tab
})

output$coef_logistic_dif <- renderTable(
  {
    coef_logistic_dif()
  },
  rownames = T,
  colnames = T
)

# ** Items detected text ######
output$logistic_dif_items <- renderPrint({
  DIFitems <- DIF_logistic_model()$DIFitems
  if (DIFitems[1] == "No DIF item detected") {
    txt <- "No item was detected as DIF."
  } else {
    txt <- paste0("Items detected as DIF items: ", paste(item_names()[DIFitems], collapse = ", "))
  }
  HTML(txt)
})

# ** Purification table ######
dif_logistic_puri_table <- reactive({
  tab <- DIF_logistic_model()$difPur

  if (!is.null(tab)) {
    colnames(tab) <- item_names()
    rownames(tab) <- paste0("Step ", seq(0, nrow(tab) - 1))
    tab
  }
})
output$dif_logistic_puri_table <- renderTable(
  {
    dif_logistic_puri_table()
  },
  rownames = T,
  colnames = T,
  digits = 0
)

# ** Purification info - number of iter ######
output$dif_logistic_puri_info <- renderPrint({
  model <- DIF_logistic_model()
  if (input$DIF_logistic_summary_purification & !is.null(DIF_logistic_model()$difPur)) {
    cat("Table below describes purification process, where rows correspond to purification iteration and columns to items.
        Value of '1' in the i-th row means that an item was detected as DIF in (i-1)-th step, while value of '0' means that
        item was not detected as DIF. The first row corresponds to the initial classification of the items when all items
        were used for calculation of DIF matching criterion. ")
    nrIter <- model$nrPur
    cat(
      "In this case, the convergence was", ifelse(model$convergence, "reached", "NOT reached even"), "after", nrIter,
      ifelse(nrIter == 1, "iteration.", "iterations.")
    )
  } else if (input$DIF_logistic_summary_purification & is.null(DIF_logistic_model()$difPur)) {
    cat("No DIF items detected whatsoever, nothing to show.")
  } else {
    cat("Item purification not requested! Nothing to show.")
  }
})

# Note setup
note_log <- reactive({
  res <- NULL

  model <- DIF_logistic_model()
  thr <- DIF_logistic_model()$thr

  res$dmv <- paste("DIF matching variable:", switch(
    model$match,
    "score" = "total score",
    "zscore" = "standardized total score",
    "matching variable" = "uploaded"
  ))

  res$type <- paste("DIF type tested:", switch(
    model$type,
    "both" = "any DIF ",
    "udif" = "uniform DIF ",
    "nudif" = "non-uniform DIF "
  ))

  res$p_adj <- paste("P-value correction method:", switch(
    model$p.adjust.method,
    "BH" = "Benjamini-Hochberg",
    "BY" = "Benjamini-Yekutieli",
    "bonferroni" = "Bonferroni",
    "holm" = "Holm",
    "hochberg" = "Hochberg",
    "hommel" = "Hommel",
    "none" = "none"
  ))

  res$puri <- paste("Item purification:", ifelse(model$purification == T, "used", "unutilized"))

  res$thr_rounded <- paste("Detection threshold:", round(thr[length(thr)], 3))

  res
})

output$note_log <- renderUI({
  withMathJax()

  HTML(
    paste(
      "Notes:",
      note_log()$dmv,
      note_log()$type,
      note_log()$p_adj,
      note_log()$puri,
      note_log()$thr_rounded,
      "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1",
      "DIF effect sizes (Nagelkerke's \\(R^2\\)): <i>A</i> = negligible effect, <i>B</i> = moderate effect, <i>C</i> = large effect",
      "Thresholds by Zumbo & Thomas (ZT): 0 'A' 0.13 'B' 0.26 'C' 1",
      "Thresholds by Jodoin & Gierl (JG): 0 'A' 0.035 'B' 0.07 'C' 1",
      sep = "</br>"
    )
  )
})

# ** Download tables ######
output$download_logistic_dif <- downloadHandler(
  filename = function() {
    paste("DIF_Logistic_statistics", ".csv", sep = "")
  },
  content = function(file) {
    data <- coef_logistic_dif()
    if ("adj. %%mathit{p}%%-value" %in% colnames(data)) {
      colnames(data) <- c(
        "LR (chi-square)", "adj. p-value", "", "",
        "pseudo R2", "ZT", "JG", "",
        "b0", "SE(b0)", "b1", "SE(b1)", "b2", "SE(b2)", "b3", "SE(b3)"
      )
    } else {
      colnames(data) <- c(
        "LR (chi-square)", "p-value", "", "",
        "pseudo R2", "ZT", "JG", "",
        "b0", "SE(b0)", "b1", "SE(b1)", "b2", "SE(b2)", "b3", "SE(b3)"
      )
    }

    write.csv(data[, c(1:3, 5:7, 9:16)], file)
    write(paste(
      "Note:",
      note_log()$dmv,
      note_log()$type,
      note_log()$p_adj,
      note_log()$puri,
      note_log()$thr_rounded,
      "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1",
      "DIF effect sizes (Nagelkerke's R^2): A = negligible effect, B = moderate effect, C = large effect",
      "Thresholds by Zumbo & Thomas (ZT): 0 'A' 0.13 'B' 0.26 'C' 1",
      "Thresholds by Jodoin & Gierl (JG): 0 'A' 0.035 'B' 0.07 'C' 1",
      sep = "\n"
    ), file, append = T)
  }
)
output$download_logistic_dif_puri <- downloadHandler(
  filename = function() {
    paste0("DIF_Logistic_purification", ".csv")
  },
  content = function(file) {
    data <- dif_logistic_puri_table()
    write.csv(data, file)
  }
)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** ITEMS ######

# ** Plot ######
DIF_logistic_items_plot <- reactive({
  group <- unlist(group())
  data <- data.frame(binary())
  item <- input$DIF_logistic_items_item

  if (input$DIF_logistic_items_matching == "uploaded") {
    match <- unlist(DIFmatching())
  } else if (input$DIF_logistic_items_matching == "zuploaded") {
    match <- scale(apply(as.data.frame(unlist(DIFmatching())), 1, sum))
  } else if (input$DIF_logistic_items_matching == "zscore") {
    match <- "zscore"
  } else if (input$DIF_logistic_items_matching == "score") {
    match <- "score"
  }

  fit <- DIF_logistic_model()

  g <- plotDIFLogistic(fit,
    item = item, match = match, item.name = item_names()[item],
    Data = data, group = group
  )
  g
})
output$DIF_logistic_items_plot <- renderPlot({
  DIF_logistic_items_plot()
})

# ** DB for plot ######
output$DB_DIF_logistic_items_plot <- downloadHandler(
  filename = function() {
    paste0("fig_DIF_logistic_", item_names()[input$DIF_logistic_items_item], ".png")
  },
  content = function(file) {
    ggsave(file,
      plot = DIF_logistic_items_plot() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Table with coefficients ######
output$DIF_logistic_items_coef_tab <- renderTable(
  {
    fit <- DIF_logistic_model()
    item <- input$DIF_logistic_items_item

    tab_coef <- fit$logitPar[item, ]
    tab_sd <- fit$logitSe[item, ]

    tab <- data.frame(tab_coef, tab_sd)
    rownames(tab) <- c("%%mathit{b}_0%%", "%%mathit{b}_1%%", "%%mathit{b}_2%%", "%%mathit{b}_3%%")
    colnames(tab) <- c("Estimate", "SE")

    tab
  },
  include.rownames = T,
  include.colnames = T
)

# ** Warning for missing values ####
output$DIF_logistic_items_NA_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** REPORT ######

# ** Model for report ######
report_DIF_logistic_model <- reactive({
  group <- unlist(group())
  data <- data.frame(binary())

  if (!input$customizeCheck) {
    fit <- DIF_logistic_model()
  } else {
    type_report <- input$type_print_DIF_logistic_report
    correction_report <- input$correction_method_log_report
    purify_report <- input$puri_LR_report

    fit <- .difLogistic_edited(
      Data = data, group = group, focal.name = 1,
      type = type_report,
      p.adjust.method = correction_report,
      purify = purify_report
    )
  }
  fit
})

# ** Plot for report ######
report_DIF_logistic_plot <- reactive({
  group <- unlist(group())
  data <- data.frame(binary())

  fit <- report_DIF_logistic_model()

  if (fit$DIFitems[1] != "No DIF item detected") {
    graflist <- vector("list", length = length(fit$DIFitems))
    i <- 1
    for (item in fit$DIFitems) {
      g <- plotDIFLogistic(fit,
        item = item,
        item.name = item_names()[item],
        Data = data, group = group
      )
      g <- g + ggtitle(paste0("DIF logistic plot for ", item_names()[item])) +
        theme(
          text = element_text(size = 12),
          plot.title = element_text(size = 12, face = "bold")
        )
      graflist[[i]] <- g
      i <- i + 1
    }
  } else {
    graflist <- NULL
  }
  graflist
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * NLR DIF ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** UPDATING INPUTS ######
DIF_nlr <- reactiveValues(
  model = NULL,
  type = NULL,
  correction = NULL,
  purification = NULL
)

# ** Updating model ######
observeEvent(input$DIF_NLR_model_print, {
  DIF_nlr$model <- input$DIF_NLR_model_print
})
observeEvent(input$DIF_NLR_model_plot, {
  DIF_nlr$model <- input$DIF_NLR_model_plot
})
observeEvent(DIF_nlr$model, {
  if (DIF_nlr$model != input$DIF_NLR_model_print) {
    updateCheckboxGroupInput(
      session = session,
      inputId = "DIF_NLR_model_print",
      selected = DIF_nlr$model
    )
  }
  if (DIF_nlr$model != input$DIF_NLR_model_plot) {
    updateCheckboxGroupInput(
      session = session,
      inputId = "DIF_NLR_model_plot",
      selected = DIF_nlr$model
    )
  }
})

# ** Updating type ######
observeEvent(input$DIF_NLR_type_print, {
  DIF_nlr$type <- input$DIF_NLR_type_print
})
observeEvent(input$DIF_NLR_type_plot, {
  DIF_nlr$type <- input$DIF_NLR_type_plot
})
observeEvent(DIF_nlr$type, {
  if (length(DIF_nlr$type) != length(input$DIF_NLR_type_print)) {
    updateCheckboxGroupInput(
      session = session,
      inputId = "DIF_NLR_type_print",
      selected = DIF_nlr$type
    )
  } else {
    if (any(DIF_nlr$type != input$DIF_NLR_type_print)) {
      updateCheckboxGroupInput(
        session = session,
        inputId = "DIF_NLR_type_print",
        selected = DIF_nlr$type
      )
    }
  }
  if (length(DIF_nlr$type) != length(input$DIF_NLR_type_plot)) {
    updateCheckboxGroupInput(
      session = session,
      inputId = "DIF_NLR_type_plot",
      selected = DIF_nlr$type
    )
  } else {
    if (any(DIF_nlr$type != input$DIF_NLR_type_plot)) {
      updateCheckboxGroupInput(
        session = session,
        inputId = "DIF_NLR_type_plot",
        selected = DIF_nlr$type
      )
    }
  }
})

# ** Updating correction ######
observeEvent(input$DIF_NLR_correction_method_print, {
  DIF_nlr$correction <- input$DIF_NLR_correction_method_print
})
observeEvent(input$DIF_NLR_correction_method_plot, {
  DIF_nlr$correction <- input$DIF_NLR_correction_method_plot
})
observeEvent(DIF_nlr$correction, {
  if (DIF_nlr$correction != input$DIF_NLR_correction_method_print) {
    updateSelectInput(
      session = session,
      inputId = "DIF_NLR_correction_method_print",
      selected = DIF_nlr$correction
    )
  }
  if (DIF_nlr$correction != input$DIF_NLR_correction_method_plot) {
    updateSelectInput(
      session = session,
      inputId = "DIF_NLR_correction_method_plot",
      selected = DIF_nlr$correction
    )
  }
})

# ** Updating purification ######
observeEvent(input$DIF_NLR_purification_print, {
  DIF_nlr$purification <- input$DIF_NLR_purification_print
})
observeEvent(input$DIF_NLR_purification_plot, {
  DIF_nlr$purification <- input$DIF_NLR_purification_plot
})
observeEvent(DIF_nlr$purification, {
  if (DIF_nlr$purification != input$DIF_NLR_purification_print) {
    updateCheckboxInput(
      session = session,
      inputId = "DIF_NLR_purification_print",
      value = DIF_nlr$purification
    )
  }
  if (DIF_nlr$purification != input$DIF_NLR_purification_plot) {
    updateCheckboxInput(
      session = session,
      inputId = "DIF_NLR_purification_plot",
      value = DIF_nlr$purification
    )
  }
})

# ** Updating DMV ######
observeEvent(input$DIF_NLR_summary_matching, {
  DIF_logistic$matching <- input$DIF_NLR_summary_matching
})
observeEvent(input$DIF_NLR_items_matching, {
  DIF_logistic$matching <- input$DIF_NLR_items_matching
})
observeEvent(DIF_logistic$matching, {
  if (DIF_logistic$matching != input$DIF_NLR_summary_matching) {
    updateCheckboxInput(
      session = session,
      inputId = "DIF_NLR_summary_matching",
      value = DIF_logistic$matching
    )
  }
  if (DIF_logistic$matching != input$DIF_NLR_items_matching) {
    updateCheckboxInput(
      session = session,
      inputId = "DIF_NLR_items_matching",
      value = DIF_logistic$matching
    )
  }
})

# update selectInput & disable purification if DMV present
observe({
  if (dif_present() == TRUE) {
    lapply(match_NLR, function(i) {
      updateSelectInput(
        session,
        paste0(i),
        choices = c(
          "Standardized total score" = "zscore",
          "Standardized uploaded" = "zuploaded"
        ),
        selected = "zscore"
      )
    })
  } else {
    lapply(match_NLR, function(i) {
      updateSelectInput(
        session,
        paste0(i),
        choices = c(
          "Standardized total score" = "zscore"
        ),
        selected = "zscore"
      )
    })
  }
})

mapply(function(match, puri) {
  observeEvent(input[[paste0(match)]], {
    if (input[[paste0(match)]] %in% c("uploaded", "zuploaded")) {
      updateCheckboxInput(session, paste0(puri), value = FALSE)
      shinyjs::disable(paste0(puri))
    } else {
      shinyjs::enable(paste0(puri))
    }
  })
},
match = match_NLR, puri = puri_NLR
)

# ** MODEL ######
model_DIF_NLR <- reactive({
  data <- data.frame(binary())
  group <- unlist(group())

  model <- input$DIF_NLR_model_print
  type <- paste0(input$DIF_NLR_type_print, collapse = "")
  adj.method <- input$DIF_NLR_correction_method_print
  purify <- input$DIF_NLR_purification_print

  if (input$DIF_NLR_summary_matching == "zscore") {
    match <- "zscore"
  } else if (input$DIF_NLR_summary_matching == "zuploaded") {
    match <- scale(apply(as.data.frame(unlist(DIFmatching())), 1, sum))
  }

  fit <- tryCatch(difNLR(
    Data = data, group = group, focal.name = 1, match = match,
    model = model, type = type,
    p.adjust.method = adj.method, purify = purify,
    test = "LR"
  ),
  error = function(e) e
  )

  validate(need(
    class(fit) == "difNLR",
    paste0("This method cannot be used on this data. Error returned: ", fit$message)
  ))

  fit
})

# ** Enabling/disabling options for type of DIF in print ####
observeEvent(input$DIF_NLR_model_print, {
  # what parameters can be selected with choice of model
  enaSelection <- switch(input$DIF_NLR_model_print,
    "Rasch" = c("b"),
    "1PL" = c("b"),
    "2PL" = c("a", "b"),
    "3PLcg" = c("a", "b"),
    "3PLdg" = c("a", "b"),
    "3PLc" = c("a", "b", "c"),
    "3PLd" = c("a", "b", "d"),
    "4PLcgdg" = c("a", "b"),
    "4PLcg" = c("a", "b", "d"),
    "4PLdg" = c("a", "b", "c"),
    "4PL" = c("a", "b", "c", "d")
  )
  # what parameters cannot be selected with choice of model
  disSelection <- setdiff(letters[1:4], enaSelection)

  # converting letters to numbers
  myLetters <- letters[1:26]
  disNum <- match(disSelection, myLetters)
  enaNum <- match(enaSelection, myLetters)

  # updating selected choices for type of DIF
  updateCheckboxGroupInput(
    session = session,
    inputId = "DIF_NLR_type_print",
    selected = enaSelection
  )

  # create object that identifies enabled and disabled options
  disElement <- paste0("#DIF_NLR_type_print :nth-child(", disNum, ") label")
  enaElement <- paste0("#DIF_NLR_type_print :nth-child(", enaNum, ") label")

  # disable checkbox options of group
  shinyjs::enable(selector = enaElement)
  shinyjs::disable(selector = disElement)
})

# ** Equation ######
output$DIF_NLR_equation_print <- renderUI({
  model <- input$DIF_NLR_model_print

  if (model == "Rasch") {
    txta <- ""
  } else {
    if (model == "1PL") {
      txta <- "a_i"
    } else {
      txta <- "a_{iG_p}"
    }
  }

  txtb <- "b_{iG_p}"

  txt2 <- paste0(txta, "\\left(Z_p - ", txtb, "\\right)")
  txt2 <- paste0("e^{", txt2, "}")
  txt2 <- paste0("\\frac{", txt2, "}{1 + ", txt2, "}")

  if (model %in% c("3PLcg", "4PLcgdg", "4PLcg")) {
    txtc <- "c_i"
  } else {
    if (model %in% c("3PLc", "4PLdg", "4PL")) {
      txtc <- "c_{iG_p}"
    } else {
      txtc <- ""
    }
  }

  if (model %in% c("3PLdg", "4PLcgdg", "4PLdg")) {
    txtd <- "d_i"
  } else {
    if (model %in% c("3PLd", "4PLcg", "4PL")) {
      txtd <- "d_{iG_p}"
    } else {
      txtd <- ""
    }
  }

  if (txtc == "" & txtd == "") {
    txt3 <- ""
  } else {
    if (txtd == "") {
      txt3 <- paste0(txtc, " + \\left(1 - ", txtc, "\\right) \\cdot ")
    } else {
      if (txtc == "") {
        txt3 <- txtd
      } else {
        txt3 <- paste0(txtc, " + \\left(", txtd, " - ", txtc, "\\right) \\cdot ")
      }
    }
  }

  txt1 <- paste0(
    "\\mathrm{P}\\left(Y_{pi} = 1 | Z_p, G_p\\right) = "
  )

  txt <- withMathJax(paste0("$$", txt1, txt3, txt2, "$$"))
  txt
})

# ** Enabling/disabling options for type of DIF in plot ####
observeEvent(input$DIF_NLR_model_plot, {
  # what parameters can be selected with choice of model
  enaSelection <- switch(input$DIF_NLR_model_plot,
    "Rasch" = c("b"),
    "1PL" = c("b"),
    "2PL" = c("a", "b"),
    "3PLcg" = c("a", "b"),
    "3PLdg" = c("a", "b"),
    "3PLc" = c("a", "b", "c"),
    "3PLd" = c("a", "b", "d"),
    "4PLcgdg" = c("a", "b"),
    "4PLcg" = c("a", "b", "d"),
    "4PLdg" = c("a", "b", "c"),
    "4PL" = c("a", "b", "c", "d")
  )
  # what parameters cannot be selected with choice of model
  disSelection <- setdiff(letters[1:4], enaSelection)

  # converting letters to numbers
  myLetters <- letters[1:26]
  disNum <- match(disSelection, myLetters)
  enaNum <- match(enaSelection, myLetters)

  # updating selected choices for type of DIF
  updateCheckboxGroupInput(
    session = session,
    inputId = "DIF_NLR_type_plot",
    selected = enaSelection
  )

  # create object that identifies enabled and disabled options
  disElement <- paste0("#DIF_NLR_type_plot :nth-child(", disNum, ") label")
  enaElement <- paste0("#DIF_NLR_type_plot :nth-child(", enaNum, ") label")

  # disable checkbox options of group
  shinyjs::enable(selector = enaElement)
  shinyjs::disable(selector = disElement)
})

# ** SUMMARY ######

# ** Summary table ######
coef_nlr_dif <- reactive({
  model <- model_DIF_NLR()

  stat <- model$Sval

  # deal with only one pval base od model specs
  pval <- if (model$p.adjust.method == "none") {
    model$pval
  } else {
    model$adj.pval
  }

  pval_symb <- symnum(pval,
    c(0, 0.001, 0.01, 0.05, 0.1, 1),
    symbols = c("***", "**", "*", ".", "")
  )
  pval_symb[pval_symb == "?"] <- ""

  blank <- character(length(stat))

  coeffs <- coeffs_se_names()$coeffs
  se <- coeffs_se_names()$se

  colnames(coeffs) <- paste0("%%mathit{", gsub("Dif", "_{Dif}", colnames(coeffs)), "}%%")
  colnames(se) <- paste0("SE(%%mathit{", gsub("Dif", "_{Dif}", colnames(se)), "}%%)")

  # zigzag
  coeffs_se <- cbind(coeffs, se)[, order(c(seq(ncol(coeffs)), seq(ncol(se))))]

  tab <- data.frame(
    stat,
    pval,
    pval_symb,
    blank,
    coeffs_se
  )

  colnames(tab) <-
    c(
      "LR (%%mathit{\\chi^2}%%)",
      ifelse(
        model$p.adjust.method == "none",
        "%%mathit{p}%%-value",
        "adj. %%mathit{p}%%-value"
      ),
      "",
      "",
      colnames(coeffs_se)
    )

  rownames(tab) <- item_names()

  tab
})

output$coef_nlr_dif <- renderTable(
  {
    coef_nlr_dif()
  },
  rownames = T,
  colnames = T
)

coeffs_se_names <- reactive({
  model <- model_DIF_NLR()
  res <- NULL

  # res$coeffs <- do.call(rbind, lapply(model$nlrPAR, function(x) {na.omit(x[c("a", "b", "c", "d", "aDif", "bDif", "cDif", "dDif")])}))
  # res$se <- do.call(rbind, lapply(model$nlrSE, function(x) {na.omit(x[c("a", "b", "c", "d", "aDif", "bDif", "cDif", "dDif")])}))
  # colnames(se) <- paste0("SE(", colnames(se), ")")

  res$se <- do.call(rbind, model$nlrSE)
  res$coeffs <- do.call(rbind, model$nlrPAR)
  res
})

# ** Items detected text ######
output$nlr_dif_items <- renderPrint({
  DIFitems <- model_DIF_NLR()$DIFitems
  if (DIFitems[1] == "No DIF item detected") {
    txt <- "No item was detected as DIF."
  } else {
    txt <- paste0("Items detected as DIF items: ", paste(item_names()[DIFitems], collapse = ", "))
  }
  HTML(txt)
})

# ** Purification table ######
dif_nlr_puri_table <- reactive({
  tab <- model_DIF_NLR()$difPur

  if (!is.null(tab)) {
    colnames(tab) <- item_names()
    rownames(tab) <- paste0("Step ", seq(0, nrow(tab) - 1))
    tab
  }
})
output$dif_nlr_puri_table <- renderTable(
  {
    dif_nlr_puri_table()
  },
  rownames = T,
  colnames = T,
  digits = 0
)

# ** Purification info - number of iter ######
output$dif_nlr_puri_info <- renderPrint({
  model <- model_DIF_NLR()
  if (input$DIF_NLR_purification_print & !is.null(model_DIF_NLR()$difPur)) {
    cat("Table below describes purification process, where rows correspond to purification iteration and columns to items.
        Value of '1' in the i-th row means that an item was detected as DIF in (i-1)-th step, while value of '0' means that
        item was not detected as DIF. The first row corresponds to the initial classification of the items when all items
        were used for calculation of DIF matching criterion. ")
    nrIter <- model$nrPur
    cat(
      "In this case, the convergence was", ifelse(model$conv.puri, "reached", "NOT reached even"), "after", nrIter,
      ifelse(nrIter == 1, "iteration.", "iterations.")
    )
  } else if (input$DIF_NLR_purification_print & is.null(model_DIF_NLR()$difPur)) {
    cat("No DIF items detected whatsoever, nothing to show.")
  } else {
    cat("Item purification not requested! Nothing to show.")
  }
})

# ** Note setup ######
note_nlr <- reactive({
  res <- NULL

  model <- model_DIF_NLR()
  thr <- if (length(unique(model$df)) == 1) {
    unique(qchisq(1 - model$alpha, model$df))
  } else {
    NULL
  }

  res$mod <- paste("Model:", switch(unique(model$model),
    "Rasch" = "Rasch model", "1PL" = "1PL model", "2PL" = "2PL model",
    "3PL" = "3PL model", "3PLcg" = "3PL model with fixed guessing for groups",
    "3PLdg" = "3PL model with fixed inattention parameter for groups",
    "3PLc" = "3PL model", "3PLd" = "3PL model with inattention parameter",
    "4PLcgdg" = "4PL model with fixed guessing and inattention parameter for groups",
    "4PLcgd" = "4PL model with fixed guessing for groups",
    "4PLd" = "4PL model with fixed guessing for groups",
    "4PLcdg" = "4PL model with fixed inattention parameter for groups",
    "4PLc" = "4PL model with fixed inattention parameter for groups",
    "4PL" = "4PL model"
  ))

  res$dmv <- paste("DIF matching variable:", switch(
    as.character(model$match[1]), # ensures number is recognize as unnamed element
    "score" = "total score",
    "zscore" = "standardized total score",
    "uploaded"
  ))

  res$type <-
    paste0(
      "DIF type tested: difference in parameters ",
      paste0(input$DIF_NLR_type_print, collapse = ", ")
    )

  res$p_adj <- paste("P-value correction method:", switch(
    model$p.adjust.method,
    holm = "Holm", hochberg = "Hochberg", hommel = "Hommel",
    bonferroni = "Bonferroni", BH = "Benjamini-Hochberg",
    BY = "Benjamini-Yekutieli", fdr = "FDR",
    none = "none"
  ))

  res$puri <- paste("Item purification:", ifelse(model$purification == T, "used", "unutilized"))
  res$thr_rounded <- paste("Detection threshold:", round(thr, 3))

  res
})

output$note_nlr <- renderUI({
  withMathJax()

  HTML(
    paste(
      "Notes:",
      note_nlr()$dmv,
      note_nlr()$mod,
      note_nlr()$type,
      note_nlr()$p_adj,
      note_nlr()$puri,
      note_nlr()$thr_rounded,
      "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1",
      sep = "</br>"
    )
  )
})

# ** Download tables ######
output$download_nlr_dif <- downloadHandler(
  filename = function() {
    paste("DIF_NLR_statistics", ".csv", sep = "")
  },
  content = function(file) {
    data <- coef_nlr_dif()

    coef_names <- colnames(coeffs_se_names()$coeffs)
    se_names <- paste0("SE(", colnames(coeffs_se_names()$se), ")")

    par_names <- c(coef_names, se_names)[order(c(seq(coef_names), seq(se_names)))]

    colnames(data) <-
      c(
        "LR (X^2)",
        ifelse(
          "%%mathit{p}%%-value" %in% colnames(data),
          "p-value",
          "adj. p-value"
        ),
        "",
        "",
        par_names
      )

    rownames(data) <- item_names()

    write.csv(data[, -4], file) # w/o blank col
    write(paste(
      "Note:",
      note_nlr()$dmv,
      note_nlr()$mod,
      gsub(",", "", note_nlr()$type), # get rid of the comma - it separates col in CSV
      note_nlr()$p_adj,
      note_nlr()$puri,
      note_nlr()$thr_rounded,
      "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1",
      sep = "\n"
    ), file, append = T)
  }
)
output$download_nlr_dif_puri <- downloadHandler(
  filename = function() {
    paste0("DIF_NLR_purification", ".csv")
  },
  content = function(file) {
    data <- dif_nlr_puri_table()
    write.csv(data, file)
  }
)

# ** Warning for missing values ####
output$DIF_NLR_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})

# ** ITEMS ######

# ** Plot ######
plot_DIF_NLRInput <- reactive({
  fit <- model_DIF_NLR()
  item <- input$DIF_NLR_item_plot

  g <- plot(fit, item = item)[[1]] +
    theme_app() +
    theme(
      legend.box.just = "top",
      legend.position = c(0.01, 0.98),
      legend.justification = c(0, 1),
      legend.key.width = unit(1, "cm"),
      legend.box = "horizontal"
    ) +
    ggtitle(item_names()[item])
  g
})

# ** Output plot ######
output$plot_DIF_NLR <- renderPlot({
  plot_DIF_NLRInput()
})

# ** DB for plot ######
output$DP_plot_DIF_NLR <- downloadHandler(
  filename = function() {
    paste0("fig_DIFNonlinear_", item_names()[input$DIF_NLR_item_plot], ".png")
  },
  content = function(file) {
    ggsave(file,
      plot = plot_DIF_NLRInput() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Equation ####
output$DIF_NLR_equation_plot <- renderUI({
  model <- input$DIF_NLR_model_plot

  if (model == "Rasch") {
    txta <- ""
  } else {
    if (model == "1PL") {
      txta <- "a_i"
    } else {
      txta <- "a_{iG_p}"
    }
  }

  txtb <- "b_{iG_p}"

  txt2 <- paste0(txta, "\\left(Z_p - ", txtb, "\\right)")
  txt2 <- paste0("e^{", txt2, "}")
  txt2 <- paste0("\\frac{", txt2, "}{1 + ", txt2, "}")

  if (model %in% c("3PLcg", "4PLcgdg", "4PLcg")) {
    txtc <- "c_i"
  } else {
    if (model %in% c("3PLc", "4PLdg", "4PL")) {
      txtc <- "c_{iG_p}"
    } else {
      txtc <- ""
    }
  }

  if (model %in% c("3PLdg", "4PLcgdg", "4PLdg")) {
    txtd <- "d_i"
  } else {
    if (model %in% c("3PLd", "4PLcg", "4PL")) {
      txtd <- "d_{iG_p}"
    } else {
      txtd <- ""
    }
  }

  if (txtc == "" & txtd == "") {
    txt3 <- ""
  } else {
    if (txtd == "") {
      txt3 <- paste0(txtc, " + \\left(1 - ", txtc, "\\right) \\cdot ")
    } else {
      if (txtc == "") {
        txt3 <- txtd
      } else {
        txt3 <- paste0(txtc, " + \\left(", txtd, " - ", txtc, "\\right) \\cdot ")
      }
    }
  }

  txt1 <- paste0(
    "\\mathrm{P}\\left(Y_{pi} = 1 | Z_p, G_p\\right) = "
  )

  txt <- withMathJax(paste0("$$", txt1, txt3, txt2, "$$"))
  txt
})

# ** Table of coefficients ######
output$tab_coef_DIF_NLR <- renderTable(
  {
    item <- input$DIF_NLR_item_plot
    fit <- model_DIF_NLR()

    tab_coef <- fit$nlrPAR[[item]]
    tab_sd <- fit$nlrSE[[item]]

    tab <- t(rbind(tab_coef, tab_sd))
    withMathJax()

    rownames(tab) <- paste0("%%mathit{", gsub("Dif", "_{Dif}", rownames(tab)), "}%%")
    colnames(tab) <- c("Estimate", "SE")

    tab
  },
  include.rownames = T
)

# ** Warning for missing values ####
output$DIF_NLR_item_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * IRT LORD ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** UPDATING INPUTS ######
DIF_lord <- reactiveValues(
  type = NULL,
  correction = NULL,
  purification = NULL
)

# ** Updating type ######
observeEvent(input$type_print_DIF_IRT_lord, {
  DIF_lord$type <- input$type_print_DIF_IRT_lord
})
observeEvent(input$type_plot_DIF_IRT_lord, {
  DIF_lord$type <- input$type_plot_DIF_IRT_lord
})
observeEvent(DIF_lord$type, {
  if (DIF_lord$type != input$type_print_DIF_IRT_lord) {
    updateCheckboxGroupInput(
      session = session,
      inputId = "type_print_DIF_IRT_lord",
      selected = DIF_lord$type
    )
  }
  if (DIF_lord$type != input$type_plot_DIF_IRT_lord) {
    updateCheckboxGroupInput(
      session = session,
      inputId = "type_plot_DIF_IRT_lord",
      selected = DIF_lord$type
    )
  }
})

# ** Updating correction ######
observeEvent(input$correction_method_DIF_IRT_lordSummary, {
  DIF_lord$correction <- input$correction_method_DIF_IRT_lordSummary
})
observeEvent(input$correction_method_DIF_IRT_lordItems, {
  DIF_lord$correction <- input$correction_method_DIF_IRT_lordItems
})
observeEvent(DIF_lord$correction, {
  if (DIF_lord$correction != input$correction_method_DIF_IRT_lordSummary) {
    updateSelectInput(
      session = session,
      inputId = "correction_method_DIF_IRT_lordSummary",
      selected = DIF_lord$correction
    )
  }
  if (DIF_lord$correction != input$correction_method_DIF_IRT_lordItems) {
    updateSelectInput(
      session = session,
      inputId = "correction_method_DIF_IRT_lordItems",
      selected = DIF_lord$correction
    )
  }
})

# ** Updating purification ######
observeEvent(input$puri_Lord, {
  DIF_lord$purification <- input$puri_Lord
})
observeEvent(input$puri_Lord_plot, {
  DIF_lord$purification <- input$puri_Lord_plot
})
observeEvent(DIF_lord$purification, {
  if (DIF_lord$purification != input$puri_Lord) {
    updateCheckboxInput(
      session = session,
      inputId = "puri_Lord",
      value = DIF_lord$purification
    )
  }
  if (DIF_lord$purification != input$puri_Lord_plot) {
    updateCheckboxInput(
      session = session,
      inputId = "puri_Lord_plot",
      value = DIF_lord$purification
    )
  }
})

# ** MODEL ######
model_DIF_IRT_Lord <- reactive({
  group <- unlist(group())
  data <- data.frame(binary())

  if (input$type_print_DIF_IRT_lord == "3PL") {
    guess <- itemPar3PL(data)[, 3]
  }

  fit <- tryCatch(switch(input$type_print_DIF_IRT_lord,
    "1PL" = .difLord_edited(
      Data = data, group = group, focal.name = 1,
      model = "1PL",
      p.adjust.method = input$correction_method_DIF_IRT_lordSummary,
      purify = input$puri_Lord
    ),
    "2PL" = .difLord_edited(
      Data = data, group = group, focal.name = 1,
      model = "2PL",
      p.adjust.method = input$correction_method_DIF_IRT_lordSummary,
      purify = input$puri_Lord
    ),
    "3PL" = .difLord_edited(
      Data = data, group = group, focal.name = 1,
      model = "3PL", c = guess,
      p.adjust.method = input$correction_method_DIF_IRT_lordSummary,
      purify = input$puri_Lord
    )
  ),
  error = function(e) e
  )

  validate(need(
    class(fit) == "Lord",
    paste0("This method cannot be used on this data. Error returned: ", fit$message)
  ))
  fit
})

# ** SUMMARY ######

# ** Interpretation for summary ######
output$DIF_Lord_interpretation_summary <- renderUI({
  type <- input$type_plot_DIF_IRT_lord
  withMathJax()
  txt <- switch(type,
    "1PL" = paste("As the parameters are estimated separately for two groups, there is one
                             equation for each group. Parameters \\(b_{iR}\\) and \\(b_{iF}\\)
                             are difficulties for the reference and the focal group for item \\(i\\). "),
    "2PL" = paste("As the parameters are estimated separately for two groups, there is one
                             equation for each group. Parameters \\(a_{iR}\\) and \\(b_{iR}\\) are discrimination
                             and difficulty for the reference group for item \\(i\\). Parameters \\(a_{iF}\\) and \\(b_{iF}\\)
                             are discrimination and difficulty for the focal group for item \\(i\\). "),
    "3PL" = paste("As the parameters are estimated separately for two groups, there is one
                             equation for each group. Parameters \\(a_{iR}\\) and \\(b_{iR}\\) are discrimination
                             and difficulty for the reference group for item \\(i\\). Parameters  \\(a_{iF}\\) and \\(b_{iF}\\)
                             are discrimination and difficulty for the focal group for item \\(i\\).
                             Parameter \\(c_i\\) is a common guessing parameter for item \\(i\\). ")
  )
  withMathJax(HTML(txt))
})

# ** Equation for summary ######
output$DIF_Lord_equation_summary <- renderUI({
  type <- input$type_plot_DIF_IRT_lord
  eqR <- switch(type,
    "1PL" = paste("$$\\mathrm{P}\\left(Y_{pi} = 1 | \\theta_p, G_p = 0\\right) =
                              \\frac{e^{\\theta_p - b_{iR}}}
                              {1 + e^{\\theta_p - b_{iR} }}$$"),
    "2PL" = paste("$$\\mathrm{P}\\left(Y_{pi} = 1 | \\theta_p, G_p = 0\\right) =
                              \\frac{e^{a_{iR} \\left(\\theta_p - b_{iR} \\right)}}
                              {1 + e^{a_{iR} \\left(\\theta_p - b_{iR} \\right)}}$$"),
    "3PL" = paste("$$\\mathrm{P}\\left(Y_{pi} = 1 | \\theta_p, G_p = 0\\right) =
                              c_i + \\left(1 - c_i\\right) \\cdot \\frac{e^{a_{iR}
                              \\left(\\theta_p - b_{iR} \\right)}}
                              {1 + e^{a_{iR} \\left(\\theta_p - b_{iR} \\right)}}$$")
  )

  eqF <- switch(type,
    "1PL" = paste("$$\\mathrm{P}\\left(Y_{pi} = 1 | \\theta_p, G_p = 1\\right) =
                              \\frac{e^{\\theta_p - b_{iF}}}
                              {1 + e^{\\theta_p - b_{iF}}}$$"),
    "2PL" = paste("$$\\mathrm{P}\\left(Y_{pi} = 1 | \\theta_p, G_p = 1\\right) =
                              \\frac{e^{a_{iF} \\left(\\theta_p - b_{iF} \\right)}}
                              {1 + e^{a_{iF} \\left(\\theta_p - b_{iF} \\right)}}$$"),
    "3PL" = paste("$$\\mathrm{P}\\left(Y_{pi} = 1 | \\theta_p, G_p = 1\\right) =
                              c_i + \\left(1 - c_i\\right) \\cdot \\frac{e^{a_{iF}
                              \\left(\\theta_p - b_{iF} \\right)}}
                              {1 + e^{a_{iF} \\left(\\theta_p - b_{iF} \\right)}}$$")
  )
  withMathJax(paste(eqR, eqF))
})

# ** Warning for missing values ######
output$DIF_IRT_LORD_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})

# ** Summary table ######
coef_lord_dif <- reactive({
  res <- model_DIF_IRT_Lord()

  # deal with only one pval base od model specs
  pval <- if (res$p.adjust.method == "none") {
    res$p.value
  } else {
    res$adjusted.p
  }

  pval_symb <- symnum(pval,
    c(0, 0.001, 0.01, 0.05, 0.1, 1),
    symbols = c("***", "**", "*", ".", "")
  )
  blank <- character(length(res$names))

  # IRT pars
  if (res$purification) {
    m <- nrow(res$itemParFinal) / 2

    mR <- res$itemParFinal[1:m, ]
    mF <- res$itemParFinal[(m + 1):(2 * m), ]
  } else {
    m <- nrow(res$itemParInit) / 2

    mR <- res$itemParInit[1:m, ]
    mF <- res$itemParInit[(m + 1):(2 * m), ]

    mF <- itemRescale(mR, mF) # rescaling!
  }

  par <- rbind(mR, mF)

  wh_coef <- switch(res$model,
    "1PL" = 1,
    "2PL" = 1:2,
    "3PL" = c(1, 2)
  )
  wh_sd <- switch(res$model,
    "1PL" = 2,
    "2PL" = 3:4,
    "3PL" = 3:4
  )

  R_indices <- seq(1, m)
  F_indices <- seq(m + 1, 2 * m)

  if (res$model == "3PL") {
    guess_both <- par[R_indices, 6]
  }

  coefs_R <- par[R_indices, wh_coef]
  coefs_F <- par[F_indices, wh_coef]

  if (res$model != "1PL") {
    colnames(coefs_R) <- paste0(colnames(coefs_R), "_R")
    colnames(coefs_F) <- paste0(colnames(coefs_F), "_F")
  }

  coefs <- cbind(coefs_R, coefs_F)

  if (res$model == "1PL") {
    colnames(coefs) <- c("b_R", "b_F")
  }

  se_R <- par[R_indices, wh_sd]
  se_F <- par[F_indices, wh_sd]

  if (res$model != "1PL") {
    colnames(se_R) <- paste0(colnames(se_R), "_R")
    colnames(se_F) <- paste0(colnames(se_F), "_F")
  }

  ses <- cbind(se_R, se_F)

  if (res$model == "1PL") {
    colnames(ses) <- c("se(b)_R", "se(b)_F")
  }

  pars_zigzag <-
    cbind(coefs, ses)[, order(c(seq(ncol(coefs)), seq(ncol(ses))))]
  if (res$model == "3PL") {
    pars_zigzag <- cbind(pars_zigzag, guess_both)
  }


  colnames(pars_zigzag) <- switch(
    res$model,
    "1PL" = c(
      "%%mathit{b}_{R}%%",
      "SE(%%mathit{b}_{R}%%)",
      "%%mathit{b}_{F}%%",
      "SE(%%mathit{b}_{F}%%)"
    ),

    "2PL" = c(
      "%%mathit{a}_{R}%%",
      "SE(%%mathit{a}_{R}%%)",
      "%%mathit{b}_{R}%%",
      "SE(%%mathit{b}_{R}%%)",
      "%%mathit{a}_{F}%%",
      "SE(%%mathit{a}_{F}%%)",
      "%%mathit{b}_{F}%%",
      "SE(%%mathit{b}_{F}%%)"
    ),

    "3PL" = c(
      "%%mathit{a}_{R}%%",
      "SE(%%mathit{a}_{R}%%)",
      "%%mathit{b}_{R}%%",
      "SE(%%mathit{b}_{R}%%)",
      "%%mathit{a}_{F}%%",
      "SE(%%mathit{a}_{F}%%)",
      "%%mathit{b}_{F}%%",
      "SE(%%mathit{b}_{F}%%)",
      "%%mathit{c}%%"
    )
  )

  # from print.Lord:
  if (is.null(res$anchor.names)) {
    itk <- 1:length(res$LordChi)
  } else {
    itk <- (1:length(res$LordChi))[!is.na(res$LordChi)]
  }

  if (res$model == "1PL") {
    if (res$purification & is.null(res$anchor.names)) {
      pars <- res$itemParFinal
    } else {
      pars <- res$itemParInit
    }
    J <- nrow(pars) / 2
    mR <- pars[1:J, 1]
    mF <- itemRescale(pars[1:J, ], pars[(J + 1):(2 * J), ])[, 1]
    rr1 <- round(mF - mR, 4)
    rr2 <- round(-2.35 * rr1, 4)
    symb1 <-
      symnum(abs(rr2), c(0, 1, 1.5, Inf), symbols = c("A", "B", "C"))
    matR2 <- cbind(rr1, rr2)[itk, ]
  }

  tab <- if (res$model == "1PL") {
    data.frame(
      res$LordChi,
      pval,
      pval_symb,
      blank,
      matR2[, 1],
      matR2[, 2],
      symb1, # eff size based on delta
      blank,
      pars_zigzag
    )
  } else {
    data.frame(
      res$LordChi,
      pval,
      pval_symb,
      blank,
      pars_zigzag
    )
  }
  colnames(tab) <- if (res$model != "1PL") {
    c(
      "Lord's %%mathit{\\chi^2}%%",
      ifelse(
        res$p.adjust.method == "none",
        "%%mathit{p}%%-value",
        "adj. %%mathit{p}%%-value"
      ),
      "",
      "",
      colnames(pars_zigzag)
    )
  } else {
    c(
      "Lord's %%mathit{\\chi^2}%%",
      ifelse(
        res$p.adjust.method == "none",
        "%%mathit{p}%%-value",
        "adj. %%mathit{p}%%-value"
      ),
      "",
      "",
      "mF-mR",
      "deltaLord",
      "",
      "",
      colnames(pars_zigzag)
    )
  }

  rownames(tab) <- item_names()

  tab
})

output$coef_lord_dif <- renderTable(
  {
    coef_lord_dif()
  },
  rownames = T,
  colnames = T
)

# ** Items detected text ######
output$lord_dif_items <- renderPrint({
  DIFitems <- model_DIF_IRT_Lord()$DIFitems
  if (DIFitems[1] == "No DIF item detected") {
    txt <- "No item was detected as DIF."
  } else {
    txt <- paste0("Items detected as DIF items: ", paste(item_names()[DIFitems], collapse = ", "))
  }
  HTML(txt)
})

# ** Purification table ######
dif_lord_puri_table <- reactive({
  tab <- model_DIF_IRT_Lord()$difPur

  if (!is.null(tab)) {
    colnames(tab) <- item_names()
    rownames(tab) <- paste0("Step ", seq(0, nrow(tab) - 1))
    tab
  }
})
output$dif_lord_puri_table <- renderTable(
  {
    dif_lord_puri_table()
  },
  rownames = T,
  colnames = T,
  digits = 0
)

# ** Purification info - number of iter ######
output$dif_lord_puri_info <- renderPrint({
  model <- model_DIF_IRT_Lord()
  if (input$puri_Lord & !is.null(model_DIF_IRT_Lord()$difPur)) {
    cat("Table below describes purification process, where rows correspond to purification iteration and columns to items.
        Value of '1' in the i-th row means that an item was detected as DIF in (i-1)-th step, while value of '0' means that
        item was not detected as DIF. The first row corresponds to the initial classification of the items when all items
        were used for calculation of DIF matching criterion. ")
    nrIter <- model$nrPur
    cat(
      "In this case, the convergence was", ifelse(model$convergence, "reached", "NOT reached even"), "after", nrIter,
      ifelse(nrIter == 1, "iteration.", "iterations.")
    )
  } else if (input$puri_Lord & is.null(model_DIF_IRT_Lord()$difPur)) {
    cat("No DIF items detected whatsoever, nothing to show.")
  } else {
    cat("Item purification not requested! Nothing to show.")
  }
})

# ** Note setup ######
note_lord <- reactive({
  res <- NULL

  model <- model_DIF_IRT_Lord()

  res$mod <- paste0("Model: ", model$model)

  res$p_adj <- paste("P-value correction method:", switch(
    model$p.adjust.method,
    holm = "Holm", hochberg = "Hochberg", hommel = "Hommel",
    bonferroni = "Bonferroni", BH = "Benjamini-Hochberg",
    BY = "Benjamini-Yekutieli",
    none = "none"
  ))

  res$puri <- paste("Item purification:", ifelse(model$purification, "used", "unutilized"))

  res$thr <- paste0("Detection threshold: ", round(model$thr, 3))

  res
})

output$note_lord <- renderUI({
  txt_effect <- ifelse(input$type_print_DIF_IRT_lord == "1PL",
    "Effect size codes: 'A': negligible effect; 'B': moderate effect; 'C': large effect </br>
                       Effect size codes: 0 'A' 1.0 'B' 1.5 'C' (for absolute values of 'deltaLord')",
    ""
  )
  HTML(
    paste(
      "Notes:",
      note_lord()$mod,
      note_lord()$p_adj,
      note_lord()$puri,
      note_lord()$thr,
      "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1",
      txt_effect,
      sep = "</br>"
    )
  )
})

# ** Download tables ######
output$download_lord_dif <- downloadHandler(
  filename = function() {
    paste("DIF_Lord_statistics", ".csv", sep = "")
  },
  content = function(file) {
    data <- coef_lord_dif()

    colnames(data) <- c("Lord's X^2", gsub("[%{}]|(mathit)", "", colnames(data)[-1]))
    rownames(data) <- item_names()

    write.csv(data[, -4], file) # w/o blank col
    write(paste(
      "Notes:",
      note_lord()$mod,
      note_lord()$p_adj,
      note_lord()$puri,
      note_lord()$thr,
      "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1",
      "Effect size codes: 'A': negligible effect; 'B': moderate effect; 'C': large effect",
      "Effect size codes: 0 'A' 1.0 'B' 1.5 'C' (for absolute values of 'deltaLord')",
      sep = "\n"
    ), file, append = T)
  }
)
output$download_lord_dif_puri <- downloadHandler(
  filename = function() {
    paste0("DIF_Lord_purification", ".csv")
  },
  content = function(file) {
    data <- dif_lord_puri_table()
    write.csv(data, file)
  }
)

# ** ITEMS ######

# ** Plot ######
plot_DIF_IRT_LordInput <- reactive({
  fitLord <- model_DIF_IRT_Lord()
  item <- input$difirt_lord_itemSlider

  g <- plotDIFirt(
    parameters = fitLord$itemParInit,
    item = item,
    item.name = item_names()[item]
  )[[item]]
  g
})

output$plot_DIF_IRT_Lord <- renderPlot({
  plot_DIF_IRT_LordInput()
})

# ** DB for plot ######
output$DP_plot_DIF_IRT_Lord <- downloadHandler(
  filename = function() {
    paste("fig_DIFIRTLord_", item_names()[input$difirt_lord_itemSlider], ".png", sep = "")
  },
  content = function(file) {
    ggsave(file,
      plot = plot_DIF_IRT_LordInput() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Table with coefficients ######
tab_coef_DIF_IRT_Lord <- reactive({
  fitLord <- model_DIF_IRT_Lord()

  if (input$puri_Lord_plot) {
    m <- nrow(fitLord$itemParFinal) / 2
    mR <- fitLord$itemParFinal[1:m, ]
    mF <- fitLord$itemParFinal[(m + 1):(2 * m), ]
  } else {
    m <- nrow(fitLord$itemParInit) / 2
    mR <- fitLord$itemParInit[1:m, ]
    mF <- fitLord$itemParInit[(m + 1):(2 * m), ]
    mF <- itemRescale(mR, mF)
  }

  par <- rbind(mR, mF)

  wh_coef <- switch(input$type_plot_DIF_IRT_lord,
    "1PL" = 1,
    "2PL" = 1:2,
    "3PL" = c(1, 2, 6)
  )
  wh_sd <- switch(input$type_plot_DIF_IRT_lord,
    "1PL" = 2,
    "2PL" = 3:4,
    "3PL" = 3:4
  )

  item <- input$difirt_lord_itemSlider
  tab_coef <- c(par[c(item, m + item), wh_coef])
  tab_sd <- c(par[c(item, m + item), wh_sd])

  if (input$type_plot_DIF_IRT_lord == "3PL") {
    tab_coef <- tab_coef[-6]
  }

  if (input$type_plot_DIF_IRT_lord == "3PL") {
    tab_sd <- c(tab_sd, NA)
  }


  tab <- data.frame(tab_coef, tab_sd)
  rownames(tab) <- switch(input$type_plot_DIF_IRT_lord,
    "1PL" = c("%%mathit{b}_{R}%%", "%%mathit{b}_{F}%%"),
    "2PL" = c("%%mathit{a}_{R}%%", "%%mathit{a}_{F}%%", "%%mathit{b}_{R}%%", "%%mathit{b}_{F}%%"),
    "3PL" = c("%%mathit{a}_{R}%%", "%%mathit{a}_{F}%%", "%%mathit{b}_{R}%%", "%%mathit{b}_{F}%%", "%%mathit{c}%%")
  )
  colnames(tab) <- c("Estimate", "SE")

  tab
})

# ** Interpretation ######
output$irtint_lord <- renderUI({
  type <- input$type_plot_DIF_IRT_lord
  withMathJax()
  txt <- switch(type,
    "1PL" = paste("As the parameters are estimated separately for two groups, there is one
                             equation for each group. Parameters \\(b_{iR}\\) and \\(b_{iF}\\)
                             are difficulties for the reference and the focal group for item \\(i\\). "),
    "2PL" = paste("As the parameters are estimated separately for two groups, there is one
                             equation for each group. Parameters \\(a_{iR}\\) and \\(b_{iR}\\) are discrimination
                             and difficulty for the reference group for item \\(i\\). Parameters \\(a_{iF}\\) and \\(b_{iF}\\)
                             are discrimination and difficulty for the focal group for item \\(i\\). "),
    "3PL" = paste("As the parameters are estimated separately for two groups, there is one
                             equation for each group. Parameters \\(a_{iR}\\) and \\(b_{iR}\\) are discrimination
                             and difficulty for the reference group for item \\(i\\). Parameters  \\(a_{iF}\\) and \\(b_{iF}\\)
                             are discrimination and difficulty for the focal group for item \\(i\\).
                             Parameter \\(c_i\\) is a common guessing parameter for item \\(i\\). ")
  )
  withMathJax(HTML(txt))
})

# ** Equation ######
output$irteq_lord <- renderUI({
  type <- input$type_plot_DIF_IRT_lord
  eqR <- switch(type,
    "1PL" = paste("$$\\mathrm{P}\\left(Y_{pi} = 1 | \\theta_p, G_p = 0\\right) =
                              \\frac{e^{\\theta_p - b_{iR}}}
                              {1 + e^{\\theta_p - b_{iR} }}$$"),
    "2PL" = paste("$$\\mathrm{P}\\left(Y_{pi} = 1 | \\theta_p, G_p = 0\\right) =
                              \\frac{e^{a_{iR} \\left(\\theta_p - b_{iR} \\right)}}
                              {1 + e^{a_{iR} \\left(\\theta_p - b_{iR} \\right)}}$$"),
    "3PL" = paste("$$\\mathrm{P}\\left(Y_{pi} = 1 | \\theta_p, G_p = 0\\right) =
                              c_i + \\left(1 - c_i\\right) \\cdot \\frac{e^{a_{iR}
                              \\left(\\theta_p - b_{iR} \\right)}}
                              {1 + e^{a_{iR} \\left(\\theta_p - b_{iR} \\right)}}$$")
  )

  eqF <- switch(type,
    "1PL" = paste("$$\\mathrm{P}\\left(Y_{pi} = 1 | \\theta_p, G_p = 1\\right) =
                              \\frac{e^{\\theta_p - b_{iF}}}
                              {1 + e^{\\theta_p - b_{iF}}}$$"),
    "2PL" = paste("$$\\mathrm{P}\\left(Y_{pi} = 1 | \\theta_p, G_p = 1\\right) =
                              \\frac{e^{a_{iF} \\left(\\theta_p - b_{iF} \\right)}}
                              {1 + e^{a_{iF} \\left(\\theta_p - b_{iF} \\right)}}$$"),
    "3PL" = paste("$$\\mathrm{P}\\left(Y_{pi} = 1 | \\theta_p, G_p = 1\\right) =
                              c_i + \\left(1 - c_i\\right) \\cdot \\frac{e^{a_{iF}
                              \\left(\\theta_p - b_{iF} \\right)}}
                              {1 + e^{a_{iF} \\left(\\theta_p - b_{iF} \\right)}}$$")
  )
  withMathJax(paste(eqR, eqF))
})

# ** Table with coefficients output ######
output$tab_coef_DIF_IRT_Lord <- renderTable(
  {
    tab_coef_DIF_IRT_Lord()
  },
  include.rownames = T,
  include.colnames = T
)

# ** Warning for missing values ####
output$DIF_IRT_LORD_item_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * IRT Raju ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** UPDATING INPUTS ######
DIF_raju <- reactiveValues(
  type = NULL,
  correction = NULL,
  purification = NULL
)

# ** Updating type ######
observeEvent(input$type_print_DIF_IRT_raju, {
  DIF_raju$type <- input$type_print_DIF_IRT_raju
})
observeEvent(input$type_plot_DIF_IRT_raju, {
  DIF_raju$type <- input$type_plot_DIF_IRT_raju
})
observeEvent(DIF_raju$type, {
  if (DIF_raju$type != input$type_print_DIF_IRT_raju) {
    updateCheckboxGroupInput(
      session = session,
      inputId = "type_print_DIF_IRT_raju",
      selected = DIF_raju$type
    )
  }
  if (DIF_raju$type != input$type_plot_DIF_IRT_raju) {
    updateCheckboxGroupInput(
      session = session,
      inputId = "type_plot_DIF_IRT_raju",
      selected = DIF_raju$type
    )
  }
})

# ** Updating correction ######
observeEvent(input$correction_method_DIF_IRT_rajuSummary, {
  DIF_raju$correction <- input$correction_method_DIF_IRT_rajuSummary
})
observeEvent(input$correction_method_DIF_IRT_rajuItems, {
  DIF_raju$correction <- input$correction_method_DIF_IRT_rajuItems
})
observeEvent(DIF_raju$correction, {
  if (DIF_raju$correction != input$correction_method_DIF_IRT_rajuSummary) {
    updateSelectInput(
      session = session,
      inputId = "correction_method_DIF_IRT_rajuSummary",
      selected = DIF_raju$correction
    )
  }
  if (DIF_raju$correction != input$correction_method_DIF_IRT_rajuItems) {
    updateSelectInput(
      session = session,
      inputId = "correction_method_DIF_IRT_rajuItems",
      selected = DIF_raju$correction
    )
  }
})

# ** Updating purification ######
observeEvent(input$puri_Raju, {
  DIF_raju$purification <- input$puri_Raju
})
observeEvent(input$puri_Raju_plot, {
  DIF_raju$purification <- input$puri_Raju_plot
})
observeEvent(DIF_raju$purification, {
  if (DIF_raju$purification != input$puri_Raju) {
    updateCheckboxInput(
      session = session,
      inputId = "puri_Raju",
      value = DIF_raju$purification
    )
  }
  if (DIF_raju$purification != input$puri_Raju_plot) {
    updateCheckboxInput(
      session = session,
      inputId = "puri_Raju_plot",
      value = DIF_raju$purification
    )
  }
})

# ** MODEL ######
model_DIF_IRT_Raju <- reactive({
  group <- unlist(group())
  data <- data.frame(binary())

  if (input$type_print_DIF_IRT_raju == "3PL") {
    guess <- itemPar3PL(data)[, 3]
  }

  fit <- tryCatch(switch(input$type_print_DIF_IRT_raju,
    "1PL" = .difRaju_edited(
      Data = data, group = group, focal.name = 1,
      model = "1PL",
      p.adjust.method = input$correction_method_DIF_IRT_rajuSummary,
      purify = input$puri_Raju
    ),
    "2PL" = .difRaju_edited(
      Data = data, group = group, focal.name = 1,
      model = "2PL",
      p.adjust.method = input$correction_method_DIF_IRT_rajuSummary,
      purify = input$puri_Raju
    ),
    "3PL" = .difRaju_edited(
      Data = data, group = group, focal.name = 1,
      model = "3PL", c = guess,
      p.adjust.method = input$correction_method_DIF_IRT_rajuSummary,
      purify = input$puri_Raju
    )
  ),
  error = function(e) e
  )

  validate(need(
    class(fit) == "Raj",
    paste0("This method cannot be used on this data. Error returned: ", fit$message)
  ))
  fit
})

# ** SUMMARY ######

# ** Interpretation for summary ######
output$DIF_Raju_interpretation_summary <- renderUI({
  type <- input$type_plot_DIF_IRT_raju
  withMathJax()
  txt <- switch(type,
    "1PL" = paste("As the parameters are estimated separately for two groups, there is one
                             equation for each group. Parameters \\(b_{iR}\\) and \\(b_{iF}\\)
                             are difficulties for the reference and the focal group for item \\(i\\). "),
    "2PL" = paste("As the parameters are estimated separately for two groups, there is one
                             equation for each group. Parameters \\(a_{iR}\\) and \\(b_{iR}\\) are discrimination
                             and difficulty for the reference group for item \\(i\\). Parameters \\(a_{iF}\\) and \\(b_{iF}\\)
                             are discrimination and difficulty for the focal group for item \\(i\\). "),
    "3PL" = paste("As the parameters are estimated separately for two groups, there is one
                             equation for each group. Parameters \\(a_{iR}\\) and \\(b_{iR}\\) are discrimination
                             and difficulty for the reference group for item \\(i\\). Parameters  \\(a_{iF}\\) and \\(b_{iF}\\)
                             are discrimination and difficulty for the focal group for item \\(i\\).
                             Parameter \\(c_i\\) is a common guessing parameter for item \\(i\\). ")
  )
  withMathJax(HTML(txt))
})

# ** Equation for summary ######
output$DIF_Raju_equation_summary <- renderUI({
  type <- input$type_plot_DIF_IRT_raju
  eqR <- switch(type,
    "1PL" = paste("$$\\mathrm{P}\\left(Y_{pi} = 1 | \\theta_p, G_p = 0\\right) =
                              \\frac{e^{\\theta_p - b_{iR}}}
                              {1 + e^{\\theta_p - b_{iR} }}$$"),
    "2PL" = paste("$$\\mathrm{P}\\left(Y_{pi} = 1 | \\theta_p, G_p = 0\\right) =
                              \\frac{e^{a_{iR} \\left(\\theta_p - b_{iR} \\right)}}
                              {1 + e^{a_{iR} \\left(\\theta_p - b_{iR} \\right)}}$$"),
    "3PL" = paste("$$\\mathrm{P}\\left(Y_{pi} = 1 | \\theta_p, G_p = 0\\right) =
                              c_i + \\left(1 - c_i\\right) \\cdot \\frac{e^{a_{iR}
                              \\left(\\theta_p - b_{iR} \\right)}}
                              {1 + e^{a_{iR} \\left(\\theta_p - b_{iR} \\right)}}$$")
  )
  eqF <- switch(type,
    "1PL" = paste("$$\\mathrm{P}\\left(Y_{pi} = 1 | \\theta_p, G_p = 1\\right) =
                              \\frac{e^{\\theta_p - b_{iF}}}
                              {1 + e^{\\theta_p - b_{iF}}}$$"),
    "2PL" = paste("$$\\mathrm{P}\\left(Y_{pi} = 1 | \\theta_p, G_p = 1\\right) =
                              \\frac{e^{a_{iF} \\left(\\theta_p - b_{iF} \\right)}}
                              {1 + e^{a_{iF} \\left(\\theta_p - b_{iF} \\right)}}$$"),
    "3PL" = paste("$$\\mathrm{P}\\left(Y_{pi} = 1 | \\theta_p, G_p = 1\\right) =
                              c_i + \\left(1 - c_i\\right) \\cdot \\frac{e^{a_{iF}
                              \\left(\\theta_p - b_{iF} \\right)}}
                              {1 + e^{a_{iF} \\left(\\theta_p - b_{iF} \\right)}}$$")
  )
  withMathJax(paste(eqR, eqF))
})

# ** Warning for missing values ######
output$DIF_Raju_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})

# ** Summary table ######
coef_raju_dif <- reactive({
  model <- model_DIF_IRT_Raju()

  # deal with only one pval base od model specs
  pval <- if (model$p.adjust.method == "none") {
    model$p.value
  } else {
    model$adjusted.p
  }

  pval_symb <- symnum(pval,
    c(0, 0.001, 0.01, 0.05, 0.1, 1, Inf),
    symbols = c("***", "**", "*", ".", "", "?!")
  )

  blank <- character(length(model$names))

  # IRT pars
  if (model$purification) {
    m <- nrow(model$itemParFinal) / 2

    mR <- model$itemParFinal[1:m, ]
    mF <- model$itemParFinal[(m + 1):(2 * m), ]
  } else {
    m <- nrow(model$itemParInit) / 2

    mR <- model$itemParInit[1:m, ]
    mF <- model$itemParInit[(m + 1):(2 * m), ]

    mF <- itemRescale(mR, mF) # rescaling!
  }

  par <- rbind(mR, mF)

  wh_coef <- switch(model$model,
    "1PL" = 1,
    "2PL" = 1:2,
    "3PL" = c(1, 2)
  )
  wh_sd <- switch(model$model,
    "1PL" = 2,
    "2PL" = 3:4,
    "3PL" = 3:4
  )

  R_indices <- seq(1, m)
  F_indices <- seq(m + 1, 2 * m)

  if (model$model == "3PL") {
    guess_both <- par[R_indices, 6]
  }

  coefs_R <- par[R_indices, wh_coef]
  coefs_F <- par[F_indices, wh_coef]

  if (model$model != "1PL") {
    colnames(coefs_R) <- paste0(colnames(coefs_R), "_R")
    colnames(coefs_F) <- paste0(colnames(coefs_F), "_F")
  }

  coefs <- cbind(coefs_R, coefs_F)

  if (model$model == "1PL") {
    colnames(coefs) <- c("b_R", "b_F")
  }

  se_R <- par[R_indices, wh_sd]
  se_F <- par[F_indices, wh_sd]

  if (model$model != "1PL") {
    colnames(se_R) <- paste0(colnames(se_R), "_R")
    colnames(se_F) <- paste0(colnames(se_F), "_F")
  }

  ses <- cbind(se_R, se_F)

  if (model$model == "1PL") {
    colnames(ses) <- c("se(b)_R", "se(b)_F")
  }

  pars_zigzag <-
    cbind(coefs, ses)[, order(c(seq(ncol(coefs)), seq(ncol(ses))))]
  if (model$model == "3PL") {
    pars_zigzag <- cbind(pars_zigzag, guess_both)
  }


  colnames(pars_zigzag) <- switch(
    model$model,
    "1PL" = c(
      "%%mathit{b}_{R}%%",
      "SE(%%mathit{b}_{R}%%)",
      "%%mathit{b}_{F}%%",
      "SE(%%mathit{b}_{F}%%)"
    ),

    "2PL" = c(
      "%%mathit{a}_{R}%%",
      "SE(%%mathit{a}_{R}%%)",
      "%%mathit{b}_{R}%%",
      "SE(%%mathit{b}_{R}%%)",
      "%%mathit{a}_{F}%%",
      "SE(%%mathit{a}_{F}%%)",
      "%%mathit{b}_{F}%%",
      "SE(%%mathit{b}_{F}%%)"
    ),

    "3PL" = c(
      "%%mathit{a}_{R}%%",
      "SE(%%mathit{a}_{R}%%)",
      "%%mathit{b}_{R}%%",
      "SE(%%mathit{b}_{R}%%)",
      "%%mathit{a}_{F}%%",
      "SE(%%mathit{a}_{F}%%)",
      "%%mathit{b}_{F}%%",
      "SE(%%mathit{b}_{F}%%)",
      "%%mathit{c}%%"
    )
  )


  # from print.raju:
  if (model$model == "1PL") {
    if (model$purification & is.null(model$anchor.names)) {
      pars <- model$itemParFinal
    } else {
      pars <- model$itemParInit
      J <- nrow(pars) / 2
      mR <- pars[1:J, 1]
      mF <- itemRescale(pars[1:J, ], pars[(J + 1):(2 * J), ])[, 1]
      rr1 <- round(mF - mR, 4)
      rr2 <- round(-2.35 * rr1, 4)
      symb1 <- symnum(abs(rr2), c(0, 1, 1.5, Inf), symbols = c("A", "B", "C"))
      matR2 <- cbind(rr1, rr2)
      # matR2 <- noquote(cbind(format(matR2, justify = "right"), symb1))
    }

    colnames(matR2) <- c("mF-mR", "deltaRaju")
  }

  tab <- if (model$model == "1PL") {
    data.frame(
      model$RajuZ,
      pval,
      pval_symb,
      blank,
      matR2[, 1],
      matR2[, 2],
      symb1, # eff size based on delta
      blank,
      pars_zigzag
    )
  } else {
    data.frame(
      model$RajuZ,
      pval,
      pval_symb,
      blank,
      pars_zigzag
    )
  }

  colnames(tab) <- if (model$model != "1PL") {
    c(
      "Raju's Z",
      ifelse(
        model$p.adjust.method == "none",
        "%%mathit{p}%%-value",
        "adj. %%mathit{p}%%-value"
      ),
      "",
      "",
      colnames(pars_zigzag)
    )
  } else {
    c(
      "Raju's Z",
      ifelse(
        model$p.adjust.method == "none",
        "%%mathit{p}%%-value",
        "adj. %%mathit{p}%%-value"
      ),
      "",
      "",
      "mF-mR",
      "deltaRaju",
      "",
      "",
      colnames(pars_zigzag)
    )
  }

  rownames(tab) <- item_names()

  tab
})

output$coef_raju_dif <- renderTable(
  {
    coef_raju_dif()
  },
  rownames = T,
  colnames = T
)

# ** Items detected text ######
output$raju_dif_items <- renderPrint({
  DIFitems <- model_DIF_IRT_Raju()$DIFitems
  if (DIFitems[1] == "No DIF item detected") {
    txt <- "No item was detected as DIF."
  } else {
    txt <- paste0("Items detected as DIF items: ", paste(item_names()[DIFitems], collapse = ", "))
  }
  HTML(txt)
})

# ** Purification table ######
dif_raju_puri_table <- reactive({
  tab <- model_DIF_IRT_Raju()$difPur

  if (!is.null(tab)) {
    colnames(tab) <- item_names()
    rownames(tab) <- paste0("Step ", seq(0, nrow(tab) - 1))
    tab
  }
})
output$dif_raju_puri_table <- renderTable(
  {
    dif_raju_puri_table()
  },
  rownames = T,
  colnames = T,
  digits = 0
)

# ** Purification info - number of iter ######
output$dif_raju_puri_info <- renderPrint({
  model <- model_DIF_IRT_Raju()
  if (input$puri_Raju & !is.null(model_DIF_IRT_Raju()$difPur)) {
    cat("Table below describes purification process, where rows correspond to purification iteration and columns to items.
        Value of '1' in the i-th row means that an item was detected as DIF in (i-1)-th step, while value of '0' means that
        item was not detected as DIF. The first row corresponds to the initial classification of the items when all items
        were used for calculation of DIF matching criterion. ")
    nrIter <- model$nrPur
    cat(
      "In this case, the convergence was", ifelse(model$convergence, "reached", "NOT reached even"), "after", nrIter,
      ifelse(nrIter == 1, "iteration.", "iterations.")
    )
  } else if (input$puri_Raju & is.null(model_DIF_IRT_Raju()$difPur)) {
    cat("No DIF items detected whatsoever, nothing to show.")
  } else {
    cat("Item purification not requested! Nothing to show.")
  }
})

# ** Note setup ######
note_raju <- reactive({
  model <- model_DIF_IRT_Raju()

  res <- NULL

  res$mod <- paste0("Model: ", model$model)
  res$p_adj <- paste("P-value correction method:", switch(
    model$p.adjust.method,
    holm = "Holm", hochberg = "Hochberg", hommel = "Hommel",
    bonferroni = "Bonferroni", BH = "Benjamini-Hochberg",
    BY = "Benjamini-Yekutieli",
    none = "none"
  ))
  res$puri <- paste("Item purification:", ifelse(model$purification, "used", "unutilized"))
  res$thr <- paste0(
    "Detection threshold: ", -round(model$thr, 4),
    " and ", round(model$thr, 4), " (significance level: ",
    model$alpha, ")"
  )
  res$signed <- paste0(
    "Type of Raju's Z statistic: ",
    ifelse(model$signed, "based on signed area", "based on unsigned area")
  )
  res
})

output$note_raju <- renderUI({
  txt_effect <- ifelse(input$type_print_DIF_IRT_lord == "1PL",
    "Effect size codes: 'A': negligible effect; 'B': moderate effect; 'C': large effect </br>
                       Effect size codes: 0 'A' 1.0 'B' 1.5 'C' (for absolute values of 'deltaRaju')",
    ""
  )
  HTML(
    paste(
      "Notes:",
      note_raju()$mod,
      note_raju()$p_adj,
      note_raju()$puri,
      note_raju()$thr,
      note_raju()$signed,
      "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1",
      txt_effect,
      sep = "</br>"
    )
  )
})

# ** Download tables ######
output$download_raju_dif <- downloadHandler(
  filename = function() {
    paste("DIF_Raju_statistics", ".csv", sep = "")
  },
  content = function(file) {
    data <- coef_raju_dif()

    colnames(data) <- gsub("[%{}]|(mathit)", "", colnames(data))
    rownames(data) <- item_names()

    write.csv(data[, -4], file) # w/o blank col
    write(paste(
      "Notes:",
      note_raju()$mod,
      note_raju()$p_adj,
      note_raju()$puri,
      note_raju()$thr,
      note_raju()$signed,
      "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1",
      "Effect size codes: 'A': negligible effect; 'B': moderate effect; 'C': large effect",
      "Effect size codes: 0 'A' 1.0 'B' 1.5 'C' (for absolute values of 'deltaRaju')",
      sep = "\n"
    ), file, append = T)
  }
)
output$download_raju_dif_puri <- downloadHandler(
  filename = function() {
    paste0("DIF_Raju_purification", ".csv")
  },
  content = function(file) {
    data <- dif_raju_puri_table()
    write.csv(data, file)
  }
)

# ** ITEMS ######

# ** Plot ######
plot_DIF_IRT_RajuInput <- reactive({
  fitRaju <- model_DIF_IRT_Raju()
  item <- input$difirt_raju_itemSlider

  g <- plotDIFirt(
    parameters = fitRaju$itemParInit, test = "Raju",
    item = item, item.name = item_names()[item]
  )[[item]]
  g
})

output$plot_DIF_IRT_Raju <- renderPlot({
  plot_DIF_IRT_RajuInput()
})

output$DP_plot_DIF_IRT_Raju <- downloadHandler(
  filename = function() {
    paste0("fig_DIFIRTRaju_", item_names()[input$difirt_raju_itemSlider], ".png")
  },
  content = function(file) {
    ggsave(file,
      plot = plot_DIF_IRT_RajuInput() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Interpretation ######
output$irtint_raju <- renderUI({
  type <- input$type_plot_DIF_IRT_raju
  withMathJax()
  txt <- switch(type,
    "1PL" = paste("As the parameters are estimated separately for two groups, there is one
                             equation for each group. Parameters \\(b_{iR}\\) and \\(b_{iF}\\)
                             are difficulties for the reference and the focal group for item \\(i\\). "),
    "2PL" = paste("As the parameters are estimated separately for two groups, there is one
                             equation for each group. Parameters \\(a_{iR}\\) and \\(b_{iR}\\) are discrimination
                             and difficulty for the reference group for item \\(i\\). Parameters \\(a_{iF}\\) and \\(b_{iF}\\)
                             are discrimination and difficulty for the focal group for item \\(i\\). "),
    "3PL" = paste("As the parameters are estimated separately for two groups, there is one
                             equation for each group. Parameters \\(a_{iR}\\) and \\(b_{iR}\\) are discrimination
                             and difficulty for the reference group for item \\(i\\). Parameters  \\(a_{iF}\\) and \\(b_{iF}\\)
                             are discrimination and difficulty for the focal group for item \\(i\\).
                             Parameter \\(c_i\\) is a common guessing parameter for item \\(i\\). ")
  )
  withMathJax(HTML(txt))
})

# ** Equation ######
output$irteq_raju <- renderUI({
  type <- input$type_plot_DIF_IRT_raju
  eqR <- switch(type,
    "1PL" = paste("$$\\mathrm{P}\\left(Y_{pi} = 1 | \\theta_p, G_p = 0\\right) =
                              \\frac{e^{\\theta_p - b_{iR}}}
                              {1 + e^{\\theta_p - b_{iR} }}$$"),
    "2PL" = paste("$$\\mathrm{P}\\left(Y_{pi} = 1 | \\theta_p, G_p = 0\\right) =
                              \\frac{e^{a_{iR} \\left(\\theta_p - b_{iR} \\right)}}
                              {1 + e^{a_{iR} \\left(\\theta_p - b_{iR} \\right)}}$$"),
    "3PL" = paste("$$\\mathrm{P}\\left(Y_{pi} = 1 | \\theta_p, G_p = 0\\right) =
                              c_i + \\left(1 - c_i\\right) \\cdot \\frac{e^{a_{iR}
                              \\left(\\theta_p - b_{iR} \\right)}}
                              {1 + e^{a_{iR} \\left(\\theta_p - b_{iR} \\right)}}$$")
  )
  eqF <- switch(type,
    "1PL" = paste("$$\\mathrm{P}\\left(Y_{pi} = 1 | \\theta_p, G_p = 1\\right) =
                              \\frac{e^{\\theta_p - b_{iF}}}
                              {1 + e^{\\theta_p - b_{iF}}}$$"),
    "2PL" = paste("$$\\mathrm{P}\\left(Y_{pi} = 1 | \\theta_p, G_p = 1\\right) =
                              \\frac{e^{a_{iF} \\left(\\theta_p - b_{iF} \\right)}}
                              {1 + e^{a_{iF} \\left(\\theta_p - b_{iF} \\right)}}$$"),
    "3PL" = paste("$$\\mathrm{P}\\left(Y_{pi} = 1 | \\theta_p, G_p = 1\\right) =
                              c_i + \\left(1 - c_i\\right) \\cdot \\frac{e^{a_{iF}
                              \\left(\\theta_p - b_{iF} \\right)}}
                              {1 + e^{a_{iF} \\left(\\theta_p - b_{iF} \\right)}}$$")
  )
  withMathJax(paste(eqR, eqF))
})

# ** Table with coefficients ######
tab_coef_DIF_IRT_Raju <- reactive({
  fitRaju <- model_DIF_IRT_Raju()
  m <- nrow(fitRaju$itemParInit) / 2

  if (input$puri_Raju_plot) {
    mR <- fitRaju$itemParFinal[1:m, ]
    mF <- fitRaju$itemParFinal[(m + 1):(2 * m), ]
  } else {
    mR <- fitRaju$itemParInit[1:m, ]
    mF <- fitRaju$itemParInit[(m + 1):(2 * m), ]
    mF <- itemRescale(mR, mF)
  }

  par <- rbind(mR, mF)

  wh_coef <- switch(input$type_plot_DIF_IRT_raju,
    "1PL" = 1,
    "2PL" = 1:2,
    "3PL" = c(1, 2, 6)
  )
  wh_sd <- switch(input$type_plot_DIF_IRT_raju,
    "1PL" = 2,
    "2PL" = 3:4,
    "3PL" = 3:4
  )
  item <- input$difirt_raju_itemSlider

  tab_coef <- c(par[c(item, m + item), wh_coef])
  tab_sd <- c(par[c(item, m + item), wh_sd])

  if (input$type_plot_DIF_IRT_raju == "3PL") {
    tab_coef <- tab_coef[-6]
  }

  if (input$type_plot_DIF_IRT_raju == "3PL") {
    tab_sd <- c(tab_sd, NA)
  }

  tab <- data.frame(tab_coef, tab_sd)
  rownames(tab) <- switch(input$type_plot_DIF_IRT_raju,
    "1PL" = c("%%mathit{b}_{R}%%", "%%mathit{b}_{F}%%"),
    "2PL" = c("%%mathit{a}_{R}%%", "%%mathit{a}_{F}%%", "%%mathit{b}_{R}%%", "%%mathit{b}_{F}%%"),
    "3PL" = c("%%mathit{a}_{R}%%", "%%mathit{a}_{F}%%", "%%mathit{b}_{R}%%", "%%mathit{b}_{F}%%", "%%mathit{c}%%")
  )
  colnames(tab) <- c("Estimate", "SE")

  tab
})

# ** Table with coefficients output ######
output$tab_coef_DIF_IRT_Raju <- renderTable(
  {
    tab_coef_DIF_IRT_Raju()
  },
  include.rownames = T,
  include.colnames = T
)

# ** Warning for missing values ####
output$DIF_Raju_item_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * SIBTEST ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Model for print ####
DIF_SIBTEST_model <- reactive({
  # data
  group <- unlist(group())
  data <- data.frame(binary())

  # inputs
  type <- input$DIF_SIBTEST_type
  purify <- input$DIF_SIBTEST_purification
  adj.method <- input$DIF_SIBTEST_correction

  # model
  fit <- .difSIBTEST_edited(
    Data = data, group = group, focal.name = 1,
    type = type,
    purify = purify, p.adjust.method = adj.method
  )
  fit
})

# ** Output print ######
output$DIF_SIBTEST_print <- renderPrint({
  print(DIF_SIBTEST_model())
})

# ** Warning for missing values ####
output$DIF_SIBTEST_NA_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})


####### ** DIF statistic and parameter tables ####
coef_sibtest_dif <- reactive({
  res <- DIF_SIBTEST_model()

  # deal with only one pval base od model specs
  pval <- if (res$p.adjust.method == "none") {
    res$p.value
  } else {
    res$adjusted.p
  }

  pval_symb <- symnum(pval,
    c(0, 0.001, 0.01, 0.05, 0.1, 1),
    symbols = c("***", "**", "*", ".", "")
  )

  tab <- data.frame(
    res$Beta,
    res$SE,
    res$X2,
    pval,
    pval_symb
  )

  colnames(tab) <-
    c(
      "%%mathit{\\beta}%%",
      "SE(%%mathit{\\beta}%%)",
      "%%mathit{\\chi^2}%%",
      ifelse(
        res$p.adjust.method == "none",
        "%%mathit{p}%%-value",
        "adj. %%mathit{p}%%-value"
      ),
      ""
    )

  rownames(tab) <- item_names()

  tab
})

output$coef_sibtest_dif <- renderTable(
  {
    coef_sibtest_dif()
  },
  rownames = T,
  colnames = T
)

# ** Items detected text ######
output$sibtest_dif_items <- renderPrint({
  DIFitems <- DIF_SIBTEST_model()$DIFitems
  if (DIFitems[1] == "No DIF item detected") {
    txt <- "No item was detected as DIF."
  } else {
    txt <- paste0("Items detected as DIF items: ", paste(item_names()[DIFitems], collapse = ", "))
  }
  HTML(txt)
})

# ** Purification table ######
dif_sibtest_puri_table <- reactive({
  tab <- DIF_SIBTEST_model()$difPur

  if (!is.null(tab)) {
    colnames(tab) <- item_names()
    rownames(tab) <- paste0("Step ", seq(0, nrow(tab) - 1))
    tab
  }
})
output$dif_sibtest_puri_table <- renderTable(
  {
    dif_sibtest_puri_table()
  },
  rownames = T,
  colnames = T,
  digits = 0
)

# ** Purification info - number of iter ######
output$dif_sibtest_puri_info <- renderPrint({
  model <- DIF_SIBTEST_model()
  if (input$DIF_SIBTEST_purification & !is.null(model$difPur)) {
    cat("Table below describes purification process, where rows correspond to purification iteration and columns to items.
        Value of '1' in the i-th row means that an item was detected as DIF in (i-1)-th step, while value of '0' means that
        item was not detected as DIF. The first row corresponds to the initial classification of the items when all items
        were used for calculation of DIF matching criterion. ")
    nrIter <- model$nrPur
    cat(
      "In this case, the convergence was", ifelse(model$convergence, "reached", "NOT reached even"), "after", nrIter,
      ifelse(nrIter == 1, "iteration.", "iterations.")
    )
  } else if (input$DIF_SIBTEST_purification & is.null(model$difPur)) {
    cat("No DIF items detected whatsoever, nothing to show.")
  } else {
    cat("Item purification not requested! Nothing to show.")
  }
})

# Note setup
note_sibtest <- reactive({
  res <- NULL

  model <- DIF_SIBTEST_model()

  res$type <- paste0("Tested DIF type: ", switch(model$type,
    "udif" = "uniform",
    "nudif" = "non-uniform"
  ))

  res$p_adj <-
    paste("P-value correction method:", switch(
      model$p.adjust.method,
      bonferroni = "Bonferroni",
      holm = "Holm",
      hochberg = "Hochberg",
      hommel = "Hommel",
      BH = "Benjamini-Hochberg",
      BY = "Benjamini-Yekutieli",
      none = "none"
    ))

  res$puri <- paste("Item purification:", ifelse(model$purification == T, "used", "unutilized"))

  thr <- qchisq(1 - model$alpha, model$df[1])
  res$thr <-
    paste0(
      "Detection threshold: ",
      round(thr, 4),
      " (significance level: ",
      model$alpha,
      ")"
    )

  res
})

output$note_sibtest <- renderUI({
  HTML(
    paste(
      "Notes:",
      note_sibtest()$type,
      note_sibtest()$p_adj,
      note_sibtest()$puri,
      note_sibtest()$thr,
      "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1",
      sep = "</br>"
    )
  )
})

# ** Download tables ######
output$download_sibtest_dif <- downloadHandler(
  filename = function() {
    paste("DIF_SIBTEST_statistics", ".csv", sep = "")
  },
  content = function(file) {
    data <- coef_sibtest_dif()

    colnames(data) <-
      c(
        "Beta",
        "SE(Beta)",
        "X^2",
        ifelse(
          "%%mathit{p}%%-value" %in% colnames(data),
          "p-value",
          "adj. p-value"
        ),
        ""
      )

    rownames(data) <- item_names()

    write.csv(data, file)
    write(paste(
      "Notes:",
      note_sibtest()$type,
      note_sibtest()$p_adj,
      note_sibtest()$puri,
      note_sibtest()$thr,
      "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1",
      sep = "\n"
    ), file, append = T)
  }
)
output$download_sibtest_dif_puri <- downloadHandler(
  filename = function() {
    paste0("DIF_SIBTEST_purification", ".csv")
  },
  content = function(file) {
    data <- dif_sibtest_puri_table()
    write.csv(data, file)
  }
)




# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * METHOD COMPARISON ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

observeEvent(input$unify_button, {
  if (input$mc_dmv == "score") {
    # DMV ---------------------------------------------------------------------
    # change DMV only if custom DMV is used

    if (input$DIF_logistic_summary_matching %in% c("uploaded", "zuploaded")) {
      updateSelectInput(session,
        inputId = "DIF_logistic_summary_matching",
        selected = "zscore"
      )
    }

    if (input$DIF_NLR_summary_matching %in% c("uploaded", "zuploaded")) {
      updateSelectInput(session,
        inputId = "DIF_NLR_summary_matching",
        selected = "zscore"
      )
    }
  }

  # Purification ------------------------------------------------------------
  # no need to examine already set up values
  # (when value of reactive remains the same, no additional computation is required)
  # TODO: use mapply and don't repeat the code...

  if (input$mc_puri == "purify") {
    lapply(c(
      "puri_DP",
      "DIF_MH_summary_purification",
      "DIF_logistic_summary_purification",
      "DIF_NLR_purification_print",
      "puri_Lord_plot",
      "puri_Raju",
      "DIF_SIBTEST_purification"
    ), function(x) {
      updateCheckboxInput(session,
        inputId = x,
        value = TRUE
      )
    })
  } else if (input$mc_puri == "dontpurify") {
    lapply(c(
      "puri_DP",
      "DIF_MH_summary_purification",
      "DIF_logistic_summary_purification",
      "DIF_NLR_purification_print",
      "puri_Lord_plot",
      "puri_Raju",
      "DIF_SIBTEST_purification"
    ), function(x) {
      updateCheckboxInput(session,
        inputId = x,
        value = FALSE
      )
    })
  }

  # Correction --------------------------------------------------------------
  # note that there is no correction method in deltaPlot

  if (input$mc_corr != "asis") {
    lapply(c(
      "DIF_MH_summary_correction",
      "DIF_logistic_summary_correction",
      "DIF_NLR_correction_method_print",
      "correction_method_DIF_IRT_lordSummary",
      "correction_method_DIF_IRT_rajuSummary",
      "DIF_SIBTEST_correction"
    ), function(x) {
      updateSelectInput(session,
        inputId = x,
        selected = input$mc_corr
      )
    })
  }
})

# output to be detected by conditionalPanel which in hand shows the button
output$unify_methods_condition <- reactive({
  length(same_puri()) != 0 |
    length(same_corr()) != 0 | length(same_dmv()) != 0
})

# in order to evaluate the condition above, it have to be rendered in UI
# we don't want to render any element, but we can pseudo-render it by setting:
outputOptions(output, "unify_methods_condition", suspendWhenHidden = FALSE)

same_puri <- reactive({
  if (length(unique(
    list(
      input$puri_DP,
      input$DIF_MH_summary_purification,
      input$DIF_logistic_summary_purification,
      input$DIF_NLR_purification_print,
      input$puri_Lord_plot,
      input$puri_Raju,
      input$DIF_SIBTEST_purification
    )
  )) != 1) {
    HTML("Warning: Purification differs across the methods.")
  }
})

same_corr <- reactive({
  if (length(unique(
    list(
      input$DIF_MH_summary_correction,
      input$DIF_logistic_summary_correction,
      input$DIF_NLR_correction_method_print,
      input$correction_method_DIF_IRT_lordSummary,
      input$correction_method_DIF_IRT_rajuSummary,
      input$DIF_SIBTEST_correction
    )
  )) != 1) {
    HTML("Warning: Correction method varies across the methods.")
  }
})

same_dmv <- reactive({
  if (input$DIF_logistic_summary_matching %in% c("uploaded", "zuploaded") |
    input$DIF_NLR_summary_matching %in% c("uploaded", "zuploaded")) {
    HTML("Warning: DIF matching variable should be unified across the methods!")
  }
})

output$same_puri <- renderPrint({
  same_puri()
})
output$same_corr <- renderPrint({
  same_corr()
})
output$same_dmv <- renderPrint({
  same_dmv()
})

output$mc_settings <- renderUI({
  HTML(paste(
    "Notes:",
    paste(
      "<b>Delta plot</b>",
      note_dp()$puri,
      note_dp()$axes,
      note_dp()$thr,
      sep = "<br>"
    ),
    paste(
      "<b>Mantel-Haenszel</b>",
      note_mh()$p_adj,
      note_mh()$puri,
      note_mh()$thr_rounded,
      sep = "<br>"
    ),
    paste(
      "<b>Logistic regression</b>",
      note_log()$dmv,
      note_log()$type,
      note_log()$p_adj,
      note_log()$puri,
      note_log()$thr_rounded,
      sep = "<br>"
    ),
    paste(
      "<b>Generalized logistic</b>",
      note_nlr()$dmv,
      note_nlr()$mod,
      note_nlr()$type,
      note_nlr()$p_adj,
      note_nlr()$puri,
      note_nlr()$thr_rounded,
      sep = "<br>"
    ),
    paste(
      "<b>IRT Lord</b>",
      note_lord()$mod,
      note_lord()$p_adj,
      note_lord()$puri,
      note_lord()$thr,
      sep = "<br>"
    ),
    paste(
      "<b>IRT Raju</b>",
      note_raju()$mod,
      note_raju()$p_adj,
      note_raju()$puri,
      note_raju()$thr,
      note_raju()$signed,
      sep = "<br>"
    ),
    paste(
      "<b>SIBTEST</b>",
      note_sibtest()$type,
      note_sibtest()$p_adj,
      note_sibtest()$puri,
      note_sibtest()$thr,
      sep = "<br>"
    ),
    sep = "<p>"
  ))
})


output$method_comparison_table <- renderTable(
  {
    group <- group()

    l_methods <- list()
    l_methods[["Delta"]] <- try(deltaGpurn()$DIFitems)
    l_methods[["MH"]] <- try(DIF_MH_model()$DIFitems)
    l_methods[["LR"]] <- try(DIF_logistic_model()$DIFitems)
    l_methods[["NLR"]] <- try(model_DIF_NLR()$DIFitems)
    l_methods[["LORD"]] <- try(model_DIF_IRT_Lord()$DIFitems)
    l_methods[["RAJU"]] <- try(model_DIF_IRT_Raju()$DIFitems)
    l_methods[["SIBTEST"]] <- try(DIF_SIBTEST_model()$DIFitems)
    # l_methods[['DFF']] <- try(DDF_multi_model()$DDFitems)

    k <- length(item_names())
    idx <- lapply(l_methods, class)
    idx <- which(unlist(idx) != "try-error")

    v <- matrix(NA, ncol = length(l_methods), nrow = k)
    v[, idx] <- 0

    # there is need to handle Delta method and DDF differently
    for (j in idx) {
      if (names(l_methods)[j] == "Delta") {
        if (all(l_methods[[j]] != "no DIF item detected")) v[as.numeric(paste(l_methods[[j]])), j] <- 1
      } else {
        # if (names(l_methods)[j] == "DDF"){
        #   if (all(l_methods[[j]] != 'No DDF item detected')) v[as.numeric(paste(l_methods[[j]])), j] <- 1
        # } else {
        if (all(l_methods[[j]] != "No DIF item detected")) v[as.numeric(paste(l_methods[[j]])), j] <- 1
        # }
      }
    }

    tab <- as.data.frame(apply(v, c(1, 2), as.integer))
    rownames(tab) <- item_names()
    colnames(tab) <- names(l_methods)

    n <- nrow(tab)
    k <- ncol(tab)

    rDIF <- rowSums(tab, na.rm = T)
    cDIF <- colSums(tab, na.rm = T)
    cDIF[k + 1] <- 0

    tab <- cbind(tab, as.integer(rDIF))
    tab <- rbind(tab, as.integer(cDIF))

    rownames(tab)[n + 1] <- "Total"
    colnames(tab)[k + 1] <- "Total"

    tab
  },
  include.rownames = T,
  include.colnames = T
)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * CUMULATIVE ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** UPDATING INPUTS ######

# update selectInput & disable purification if DMV present

observe({
  if (dif_present() == TRUE) {
    lapply(match_cum, function(i) {
      updateSelectInput(
        session,
        paste0(i),
        choices = c(
          "Standardized total score" = "zscore",
          "Standardized uploaded" = "zuploaded"
        ),
        selected = "zscore"
      )
    })
  } else {
    lapply(match_cum, function(i) {
      updateSelectInput(
        session,
        paste0(i),
        choices = c(
          "Standardized total score" = "zscore"
        ),
        selected = "zscore"
      )
    })
  }
})

mapply(function(match, puri) {
  observeEvent(input[[paste0(match)]], {
    if (input[[paste0(match)]] %in% c("uploaded", "zuploaded")) {
      updateCheckboxInput(session, paste0(puri), value = FALSE)
      shinyjs::disable(paste0(puri))
    } else {
      shinyjs::enable(paste0(puri))
    }
  })
},
match = match_cum, puri = puri_cum
)

DIF_cum <- reactiveValues(
  type = NULL,
  correction = NULL,
  purification = NULL,
  matching = NULL
)

# ** Updating type ######
observeEvent(input$DIF_cum_type_summary, {
  DIF_cum$type <- input$DIF_cum_type_summary
})
observeEvent(input$DIF_cum_type_items, {
  DIF_cum$type <- input$DIF_cum_type_items
})
observeEvent(DIF_cum$type, {
  if (DIF_cum$type != input$DIF_cum_type_summary) {
    updateCheckboxGroupInput(
      session = session,
      inputId = "DIF_cum_type_summary",
      selected = DIF_cum$type
    )
  }
  if (DIF_cum$type != input$DIF_cum_type_items) {
    updateCheckboxGroupInput(
      session = session,
      inputId = "DIF_cum_type_items",
      selected = DIF_cum$type
    )
  }
})

# ** Updating correction ######
observeEvent(input$DIF_cum_correction_summary, {
  DIF_cum$correction <- input$DIF_cum_correction_summary
})
observeEvent(input$DIF_cum_correction_items, {
  DIF_cum$correction <- input$DIF_cum_correction_items
})
observeEvent(DIF_cum$correction, {
  if (DIF_cum$correction != input$DIF_cum_correction_summary) {
    updateSelectInput(
      session = session,
      inputId = "DIF_cum_correction_summary",
      selected = DIF_cum$correction
    )
  }
  if (DIF_cum$correction != input$DIF_cum_correction_items) {
    updateSelectInput(
      session = session,
      inputId = "DIF_cum_correction_items",
      selected = DIF_cum$correction
    )
  }
})

# ** Updating purification ######
observeEvent(input$DIF_cum_purification_summary, {
  DIF_cum$purification <- input$DIF_cum_purification_summary
})
observeEvent(input$DIF_cum_purification_items, {
  DIF_cum$purification <- input$DIF_cum_purification_items
})
observeEvent(DIF_cum$purification, {
  if (DIF_cum$purification != input$DIF_cum_purification_summary) {
    updateCheckboxInput(
      session = session,
      inputId = "DIF_cum_purification_summary",
      value = DIF_cum$purification
    )
  }
  if (DIF_cum$purification != input$DIF_cum_purification_items) {
    updateCheckboxInput(
      session = session,
      inputId = "DIF_cum_purification_items",
      value = DIF_cum$purification
    )
  }
})

# ** Updating DMV ########
observeEvent(input$DIF_cum_summary_matching, {
  DIF_cum$matching <- input$DIF_cum_summary_matching
})
observeEvent(input$DIF_cum_items_matching, {
  DIF_cum$matching <- input$DIF_cum_items_matching
})
observeEvent(DIF_cum$matching, {
  if (DIF_cum$matching != input$DIF_cum_summary_matching) {
    updateCheckboxInput(
      session = session,
      inputId = "DIF_cum_summary_matching",
      value = DIF_cum$matching
    )
  }
  if (DIF_cum$matching != input$DIF_cum_items_matching) {
    updateCheckboxInput(
      session = session,
      inputId = "DIF_cum_items_matching",
      value = DIF_cum$matching
    )
  }
})

# ** Updating item slider ######
observe({
  item_count <- ncol(ordinal())
  updateSliderInput(
    session = session,
    inputId = "DIF_cum_items",
    max = item_count
  )
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** SUMMARY ######

# ** Model for cumulative regression - summary ######
# it's the same as for items as the inputs are synchronized
DIF_cum_model <- reactive({
  data <- ordinal()
  group <- unlist(group())
  type <- input$DIF_cum_type_summary
  puri <- input$DIF_cum_purification_summary
  corr <- input$DIF_cum_correction_summary

  if (input$DIF_cum_summary_matching == "zscore") {
    match <- "zscore"
  } else if (input$DIF_cum_summary_matching == "zuploaded") {
    match <- scale(apply(as.data.frame(unlist(DIFmatching())), 1, sum))
  }

  fit <- difORD(data, group,
    focal.name = 1, model = "cumulative", match = match,
    type = type, purify = puri, p.adjust.method = corr,
    parametrization = "classic"
  )
  fit
})

# ** Output print ######
output$DIF_cum_print <- renderPrint({
  print(DIF_cum_model())
})

# ** Equation - cumulative probability ######
DIF_cum_equation1_summary_Input <- reactive({
  # txt1 <- ifelse(input$DIF_cum_matching_summary == "score", "X_p", "Z_p")
  txt1 <- "Z_p"
  txt2 <- paste0("b_{i0k} + b_{i1} ", txt1, " + b_{i2} G_p + b_{i3} ", txt1, " G_p")
  txt3 <- "Z_p, G_p"

  txt <- paste0("$$\\mathrm{P}(Y_{pi} \\geq k|", txt3, ") = \\frac{e^{", txt2, "}}{1 + e^{", txt2, "}}$$")

  txt
})

output$DIF_cum_equation1_summary <- renderUI({
  withMathJax(HTML(DIF_cum_equation1_summary_Input()))
})

# ** Equation - category probability ######
DIF_cum_equation2_summary_Input <- reactive({
  # txt1 <- ifelse(input$DIF_cum_matching_summary == "score", "X_p", "Z_p")
  txt1 <- "Z_p"
  txt3 <- "Z_p, G_p"
  txt4 <- paste0(txt1, ", b_{i0k+1}, b_{i1}, b_{i2}, b_{i3}")

  txt <- paste0("$$\\mathrm{P}(Y_{pi} = k|", txt3, ") = \\mathrm{P}(Y_{pi} \\geq k|", txt3, ") - \\mathrm{P}(Y_{pi} \\geq k + 1|", txt3, ")$$")

  txt
})

output$DIF_cum_equation2_summary <- renderUI({
  withMathJax(HTML(DIF_cum_equation2_summary_Input()))
})

# ** Warning for missing values ####
output$DIF_cum_NA_warning_summary <- renderUI({
  txt <- na_score()
  HTML(txt)
})



# ** DIF statistic and parameter tables ####
coef_cum_dif <- reactive({
  res <- DIF_cum_model()

  # only one pval var, based on model specs
  pval <- if (res$p.adjust.method == "none") {
    res$pval
  } else {
    res$adj.pval
  }

  pval_symb <- symnum(pval,
    c(0, 0.001, 0.01, 0.05, 0.1, 1),
    symbols = c("***", "**", "*", ".", "")
  )

  blank <- character(ncol(res$Data))

  # estimated coefficients for all items with standard errors
  coefs <- coef(res, SE = TRUE, simplify = TRUE)

  estims <- coefs[c(TRUE, FALSE), ]

  ses <- coefs[c(FALSE, TRUE), ]

  pars_zigzag <-
    cbind(estims, ses)[, order(c(seq(ncol(estims)), seq(ncol(ses))))]

  pars_digits_only <- stringr::str_extract(colnames(estims), "\\d")
  colnames(pars_zigzag) <-
    paste0(c("", "SE("), rep(c(
      paste0("b_0", pars_digits_only[!is.na(pars_digits_only)]), "b_1", "b_2", "b_3"
    ), each = 2), c("", ")"))

  # # FOR IRT PARAMETRIZATION!!!
  # colnames(pars_zigzag) <-
  #   paste0(
  #     c("", "SE("),
  #     stringr::str_replace(
  #       stringr::str_replace(colnames(pars_zigzag), "(?=\\d)", "}_{"),
  #       ".*(?=b|a)",
  #       "%%mathit{"
  #     )
  #     ,
  #     c("}%%", "}%%)")
  #   )

  # table
  tab <-
    data.frame(
      check.names = FALSE, # in R 4.x.x, this changes parentheses
      res$Sval, # to dots (to ensure "syntactical validity")
      pval,
      pval_symb,
      blank,
      pars_zigzag
    )

  # colnames rodeo -- add math formatting
  colnames(tab) <- c(
    "LR (%%mathit{\\chi^2}%%)",
    ifelse(
      res$p.adjust.method == "none",
      "%%mathit{p}%%-value",
      "adj. %%mathit{p}%%-value"
    ),
    "",
    "",
    str_replace(
      str_replace(
        str_replace(
          colnames(pars_zigzag),
          "(?=b)", "%%mathit{"
        ),
        "(?<=\\d$)|(?=\\))",
        "}%%"
      ),
      "_",
      "}_{"
    )
  )


  rownames(tab) <- item_names()

  tab
})

output$coef_cum_dif <- renderTable(
  {
    coef_cum_dif()
  },
  rownames = T,
  colnames = T
)

# ** Items detected text ######
output$cum_dif_items <- renderPrint({
  DIFitems <- DIF_cum_model()$DIFitems
  if (DIFitems[1] == "No DIF item detected") {
    txt <- "No item was detected as DIF."
  } else {
    txt <- paste0("Items detected as DIF items: ", paste(item_names()[DIFitems], collapse = ", "))
  }
  HTML(txt)
})

# ** Purification table ######
dif_cum_puri_table <- reactive({
  tab <- DIF_cum_model()$difPur

  if (!is.null(tab)) {
    colnames(tab) <- item_names()
    rownames(tab) <- paste0("Step ", seq(0, nrow(tab) - 1))
    tab
  }
})
output$dif_cum_puri_table <- renderTable(
  {
    dif_cum_puri_table()
  },
  rownames = T,
  colnames = T,
  digits = 0
)

# ** Purification info - number of iter ######
output$dif_cum_puri_info <- renderPrint({
  model <- DIF_cum_model()
  if (input$DIF_cum_purification_summary & !is.null(DIF_cum_model()$difPur)) {
    cat("Table below describes purification process, where rows correspond to purification iteration and columns to items.
        Value of '1' in the i-th row means that an item was detected as DIF in (i-1)-th step, while value of '0' means that
        item was not detected as DIF. The first row corresponds to the initial classification of the items when all items
        were used for calculation of DIF matching criterion. ")
    nrIter <- model$nrPur
    cat(
      "In this case, the convergence was", ifelse(model$conv.puri, "reached", "NOT reached even"), "after", nrIter,
      ifelse(nrIter == 1, "iteration.", "iterations.")
    )
  } else if (input$DIF_cum_purification_summary & is.null(DIF_cum_model()$difPur)) {
    cat("No DIF items detected whatsoever, nothing to show.")
  } else {
    cat("Item purification not requested! Nothing to show.")
  }
})

# Note setup
note_cum <- reactive({
  res <- NULL

  model <- DIF_cum_model()

  res$dmv <- paste(
    "DIF matching variable:",
    if (model$match[1] == "score") {
      "total score"
    } else if (model$match[1] == "zscore") {
      "standardized total score"
    } else {
      "standardized uploaded"
    }
  )


  res$type <- paste("DIF type tested:", switch(
    model$type,
    "both" = "any DIF ",
    "udif" = "uniform DIF ",
    "nudif" = "non-uniform DIF "
  ))

  res$p_adj <- paste("P-value correction method:", switch(
    model$p.adjust.method,
    "BH" = "Benjamini-Hochberg",
    "BY" = "Benjamini-Yekutieli",
    "bonferroni" = "Bonferroni",
    "holm" = "Holm",
    "hochberg" = "Hochberg",
    "hommel" = "Hommel",
    "none" = "none"
  ))

  res$puri <- paste("Item purification:", ifelse(model$purification == T, "used", "unutilized"))

  res
})

output$note_cum <- renderUI({
  withMathJax()

  HTML(
    paste(
      "Notes:",
      note_cum()$dmv,
      note_cum()$type,
      note_cum()$p_adj,
      note_cum()$puri,
      "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1",
      sep = "</br>"
    )
  )
})

# ** Download tables ######
output$download_cum_dif <- downloadHandler(
  filename = function() {
    paste("DIF_Cumulative_statistics", ".csv", sep = "")
  },
  content = function(file) {
    data <- coef_cum_dif()

    # remove all math-format characters -->> plaintext
    colnames(data) <- stringr::str_remove_all(colnames(data), "mathit|[%{}]|\\\\")

    write.csv(data[-4], file)
    write(paste(
      "Note:",
      note_cum()$dmv,
      note_cum()$type,
      note_cum()$p_adj,
      note_cum()$puri,
      "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1",
      sep = "\n"
    ), file, append = T)
  }
)
output$download_cum_dif_puri <- downloadHandler(
  filename = function() {
    paste0("DIF_Cumulative_purification", ".csv")
  },
  content = function(file) {
    data <- dif_cum_puri_table()
    write.csv(data, file)
  }
)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** ITEMS ######

# ** Plot - cumulative ######
DIF_cum_plot_cumulative_Input <- reactive({
  fit <- DIF_cum_model()
  item <- input$DIF_cum_items

  g <- plot(fit, item = item, plot.type = "cumulative")[[1]] +
    theme_app() +
    ggtitle(item_names()[item]) +
    theme(
      legend.box.just = "top",
      legend.justification = c("right", "bottom"),
      legend.position = c(0.98, 0.02),
      legend.box = "horizontal",
      legend.margin = margin(0, 0, 0, 0, unit = "cm")
    )
  g
})

output$DIF_cum_plot_cumulative <- renderPlot({
  DIF_cum_plot_cumulative_Input()
})

# ** DB for plot - cumulative ######
output$DB_DIF_cum_plot_cumulative <- downloadHandler(
  filename = function() {
    paste0("fig_DIF_cum_cumulative_", item_names()[input$DIF_cum_items], ".png")
  },
  content = function(file) {
    ggsave(file,
      plot = DIF_cum_plot_cumulative_Input() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height,
      width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Plot - category ######
DIF_cum_plot_category_Input <- reactive({
  fit <- DIF_cum_model()
  item <- input$DIF_cum_items

  g <- plot(fit, item = item, plot.type = "category")[[1]] +
    theme_app() +
    ggtitle(item_names()[item]) +
    theme(
      legend.box.just = "top",
      legend.justification = c("left", "top"),
      legend.position = c(0.02, 0.98),
      legend.box = "horizontal",
      legend.margin = margin(0, 0, 0, 0, unit = "cm")
    )
  g
})

output$DIF_cum_plot_category <- renderPlot({
  DIF_cum_plot_category_Input()
})

# ** DB for plot - cumulative ######
output$DB_DIF_cum_plot_category <- downloadHandler(
  filename = function() {
    paste0("fig_DIF_cum_category_", item_names()[input$DIF_cum_items], ".png")
  },
  content = function(file) {
    ggsave(file,
      plot = DIF_cum_plot_category_Input() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height,
      width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Table of coefficients ######
DIF_cum_coef_tab_Input <- reactive({
  fit <- DIF_cum_model()
  item <- input$DIF_cum_items
  cat <- sort(unique(data.frame(ordinal())[, item]))[-1]

  tab <- matrix(0, nrow = length(cat) + 3, ncol = 2)
  rownames(tab) <- c(paste0("(Intercept):", cat), "x", "group", "x:group")
  colnames(tab) <- c("estimate", "SE")

  tmp <- t(coef(fit, SE = T)[[item]])
  tab[rownames(tmp), ] <- tmp
  colnames(tab) <- c("Estimate", "SE")
  rownames(tab) <- c(
    paste0("%%mathit{b}_{0", cat, "}%%"),
    "%%mathit{b}_{1}%%",
    "%%mathit{b}_{2}%%",
    "%%mathit{b}_{3}%%"
  )

  tab
})

output$DIF_cum_coef_tab <- renderTable(
  {
    DIF_cum_coef_tab_Input()
  },
  include.rownames = T,
  include.colnames = T
)

# ** Equation - cumulative probability ######
DIF_cum_equation1_items_Input <- reactive({
  # txt1 <- ifelse(input$DIF_cum_matching_summary == "score", "X_p", "Z_p")
  txt1 <- "Z_p"
  txt2 <- paste0("b_{i0k} + b_{i1} ", txt1, " + b_{i2} G_p + b_{i3} ", txt1, " G_p")
  txt3 <- "Z_p, G_p"

  txt <- paste0("$$\\mathrm{P}(Y_{pi} \\geq k|", txt3, ") = \\frac{e^{", txt2, "}}{1 + e^{", txt2, "}}$$")

  txt
})

output$DIF_cum_equation1_items <- renderUI({
  withMathJax(HTML(DIF_cum_equation1_items_Input()))
})

# ** Equation - category probability ######
DIF_cum_equation2_items_Input <- reactive({
  # txt1 <- ifelse(input$DIF_cum_matching_summary == "score", "X_p", "Z_p")
  txt1 <- "Z_p"
  txt3 <- "Z_p, G_p"

  txt <- paste0("$$\\mathrm{P}(Y_{pi} = k|", txt3, ") = \\mathrm{P}(Y_{pi} \\geq k|", txt3, ") - \\mathrm{P}(Y_{pi} \\geq k + 1|", txt3, ")$$")

  txt
})

output$DIF_cum_equation2_items <- renderUI({
  withMathJax(HTML(DIF_cum_equation2_items_Input()))
})

# ** Warning for missing values ####
output$DIF_cum_NA_warning_items <- renderUI({
  txt <- na_score()
  HTML(txt)
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * ADJACENT ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** UPDATING INPUTS ######

# update selectInput & disable purification if DMV present

observe({
  if (dif_present() == TRUE) {
    lapply(match_adj, function(i) {
      updateSelectInput(
        session,
        paste0(i),
        choices = c(
          "Standardized total score" = "zscore",
          "Standardized uploaded" = "zuploaded"
        ),
        selected = "zscore"
      )
    })
  } else {
    lapply(match_adj, function(i) {
      updateSelectInput(
        session,
        paste0(i),
        choices = c(
          "Standardized total score" = "zscore"
        ),
        selected = "zscore"
      )
    })
  }
})

mapply(function(match, puri) {
  observeEvent(input[[paste0(match)]], {
    if (input[[paste0(match)]] %in% c("uploaded", "zuploaded")) {
      updateCheckboxInput(session, paste0(puri), value = FALSE)
      shinyjs::disable(paste0(puri))
    } else {
      shinyjs::enable(paste0(puri))
    }
  })
},
match = match_adj, puri = puri_adj
)

DIF_adj <- reactiveValues(
  type = NULL,
  correction = NULL,
  purification = NULL,
  matching = NULL
)

# ** Updating type ######
observeEvent(input$DIF_adj_type_summary, {
  DIF_adj$type <- input$DIF_adj_type_summary
})
observeEvent(input$DIF_adj_type_items, {
  DIF_adj$type <- input$DIF_adj_type_items
})
observeEvent(DIF_adj$type, {
  if (DIF_adj$type != input$DIF_adj_type_summary) {
    updateCheckboxGroupInput(
      session = session,
      inputId = "DIF_adj_type_summary",
      selected = DIF_adj$type
    )
  }
  if (DIF_adj$type != input$DIF_adj_type_items) {
    updateCheckboxGroupInput(
      session = session,
      inputId = "DIF_adj_type_items",
      selected = DIF_adj$type
    )
  }
})

# ** Updating DMV ########
observeEvent(input$DIF_adj_summary_matching, {
  DIF_adj$matching <- input$DIF_adj_summary_matching
})
observeEvent(input$DIF_adj_items_matching, {
  DIF_adj$matching <- input$DIF_adj_items_matching
})
observeEvent(DIF_adj$matching, {
  if (DIF_adj$matching != input$DIF_adj_summary_matching) {
    updateCheckboxInput(
      session = session,
      inputId = "DIF_adj_summary_matching",
      value = DIF_adj$matching
    )
  }
  if (DIF_adj$matching != input$DIF_adj_items_matching) {
    updateCheckboxInput(
      session = session,
      inputId = "DIF_adj_items_matching",
      value = DIF_adj$matching
    )
  }
})

# ** Updating correction ######
observeEvent(input$DIF_adj_correction_summary, {
  DIF_adj$correction <- input$DIF_adj_correction_summary
})
observeEvent(input$DIF_adj_correction_items, {
  DIF_adj$correction <- input$DIF_adj_correction_items
})
observeEvent(DIF_adj$correction, {
  if (DIF_adj$correction != input$DIF_adj_correction_summary) {
    updateSelectInput(
      session = session,
      inputId = "DIF_adj_correction_summary",
      selected = DIF_adj$correction
    )
  }
  if (DIF_adj$correction != input$DIF_adj_correction_items) {
    updateSelectInput(
      session = session,
      inputId = "DIF_adj_correction_items",
      selected = DIF_adj$correction
    )
  }
})

# ** Updating purification ######
observeEvent(input$DIF_adj_purification_summary, {
  DIF_adj$purification <- input$DIF_adj_purification_summary
})
observeEvent(input$DIF_adj_purification_items, {
  DIF_adj$purification <- input$DIF_adj_purification_items
})
observeEvent(DIF_adj$purification, {
  if (DIF_adj$purification != input$DIF_adj_purification_summary) {
    updateCheckboxInput(
      session = session,
      inputId = "DIF_adj_purification_summary",
      value = DIF_adj$purification
    )
  }
  if (DIF_adj$purification != input$DIF_adj_purification_items) {
    updateCheckboxInput(
      session = session,
      inputId = "DIF_adj_purification_items",
      value = DIF_adj$purification
    )
  }
})

# ** Updating item slider ######
observe({
  item_count <- ncol(ordinal())
  updateSliderInput(
    session = session,
    inputId = "DIF_adj_items",
    max = item_count
  )
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** SUMMARY ######

# ** Model for adjacent regression - summary ######
# it's the same as for items as the inputs are synchronized
DIF_adj_model <- reactive({
  data <- ordinal()
  group <- unlist(group())
  type <- input$DIF_adj_type_summary
  puri <- input$DIF_adj_purification_summary
  corr <- input$DIF_adj_correction_summary

  if (input$DIF_adj_summary_matching == "zscore") {
    match <- "zscore"
  } else if (input$DIF_adj_summary_matching == "zuploaded") {
    match <- scale(apply(as.data.frame(unlist(DIFmatching())), 1, sum))
  }

  fit <- difORD(data, group,
    focal.name = 1, model = "adjacent", match = match,
    type = type, purify = puri, p.adjust.method = corr,
    parametrization = "classic"
  )
  fit
})

# ** Output print ######
output$DIF_adj_print <- renderPrint({
  print(DIF_adj_model())
})

# ** Equation ######
DIF_adj_equation_summary_Input <- reactive({
  # txt1 <- ifelse(input$DIF_adj_matching_summary == "score", "X_p", "Z_p")
  txt1 <- "Z_p"
  txt2 <- paste0("b_{0it} + b_{i1} ", txt1, " + b_{i2} G_p + b_{i3} ", txt1, " G_p")
  txt3 <- "Z_p, G_p"

  txt <- paste0("$$\\mathrm{P}(Y_{pi} = k|", txt3, ") = \\frac{e^{\\sum_{t = 0}^{k}", txt2, "}}{\\sum_{r = 0}^{K_i}e^{\\sum_{t = 0}^{r}", txt2, "}}$$")

  txt
})

output$DIF_adj_equation_summary <- renderUI({
  withMathJax(HTML(DIF_adj_equation_summary_Input()))
})

# ** Warning for missing values ####
output$DIF_adj_NA_warning_summary <- renderUI({
  txt <- na_score()
  HTML(txt)
})


# ** DIF statistic and parameter tables ####
coef_adj_dif <- reactive({
  res <- DIF_adj_model()

  # only one pval var, based on model specs
  pval <- if (res$p.adjust.method == "none") {
    res$pval
  } else {
    res$adj.pval
  }

  pval_symb <- symnum(pval,
    c(0, 0.001, 0.01, 0.05, 0.1, 1),
    symbols = c("***", "**", "*", ".", "")
  )

  blank <- character(ncol(res$Data))

  # estimated coefficients for all items with standard errors
  coefs <- coef(res, SE = TRUE, simplify = TRUE)

  estims <- coefs[c(TRUE, FALSE), ]

  ses <- coefs[c(FALSE, TRUE), ]

  pars_zigzag <-
    cbind(estims, ses)[, order(c(seq(ncol(estims)), seq(ncol(ses))))]

  pars_digits_only <- stringr::str_extract(colnames(estims), "\\d")
  colnames(pars_zigzag) <-
    paste0(c("", "SE("), rep(c(
      paste0("b_0", pars_digits_only[!is.na(pars_digits_only)]), "b_1", "b_2", "b_3"
    ), each = 2), c("", ")"))

  # # FOR IRT PARAMETRIZATION!!!
  # colnames(pars_zigzag) <-
  #   paste0(
  #     c("", "SE("),
  #     stringr::str_replace(
  #       stringr::str_replace(colnames(pars_zigzag), "(?=\\d)", "}_{"),
  #       ".*(?=b|a)",
  #       "%%mathit{"
  #     )
  #     ,
  #     c("}%%", "}%%)")
  #   )

  # table
  tab <-
    data.frame(
      check.names = FALSE,
      res$Sval,
      pval,
      pval_symb,
      blank,
      pars_zigzag
    )

  # colnames rodeo -- add math formatting
  colnames(tab) <- c(
    "LR (%%mathit{\\chi^2}%%)",
    ifelse(
      res$p.adjust.method == "none",
      "%%mathit{p}%%-value",
      "adj. %%mathit{p}%%-value"
    ),
    "",
    "",
    str_replace(
      str_replace(
        str_replace(
          colnames(pars_zigzag),
          "(?=b)", "%%mathit{"
        ),
        "(?<=\\d$)|(?=\\))",
        "}%%"
      ),
      "_",
      "}_{"
    )
  )


  rownames(tab) <- item_names()

  tab
})

output$coef_adj_dif <- renderTable(
  {
    coef_adj_dif()
  },
  rownames = T,
  colnames = T
)

# ** Items detected text ######
output$adj_dif_items <- renderPrint({
  DIFitems <- DIF_adj_model()$DIFitems
  if (DIFitems[1] == "No DIF item detected") {
    txt <- "No item was detected as DIF."
  } else {
    txt <- paste0("Items detected as DIF items: ", paste(item_names()[DIFitems], collapse = ", "))
  }
  HTML(txt)
})

# ** Purification table ######
dif_adj_puri_table <- reactive({
  tab <- DIF_adj_model()$difPur

  if (!is.null(tab)) {
    colnames(tab) <- item_names()
    rownames(tab) <- paste0("Step ", seq(0, nrow(tab) - 1))
    tab
  }
})
output$dif_adj_puri_table <- renderTable(
  {
    dif_adj_puri_table()
  },
  rownames = T,
  colnames = T,
  digits = 0
)

# ** Purification info - number of iter ######
output$dif_adj_puri_info <- renderPrint({
  model <- DIF_adj_model()
  if (input$DIF_adj_purification_summary & !is.null(DIF_adj_model()$difPur)) {
    cat("Table below describes purification process, where rows correspond to purification iteration and columns to items.
        Value of '1' in the i-th row means that an item was detected as DIF in (i-1)-th step, while value of '0' means that
        item was not detected as DIF. The first row corresponds to the initial classification of the items when all items
        were used for calculation of DIF matching criterion. ")
    nrIter <- model$nrPur
    cat(
      "In this case, the convergence was", ifelse(model$conv.puri, "reached", "NOT reached even"), "after", nrIter,
      ifelse(nrIter == 1, "iteration.", "iterations.")
    )
  } else if (input$DIF_adj_purification_summary & is.null(DIF_adj_model()$difPur)) {
    cat("No DIF items detected whatsoever, nothing to show.")
  } else {
    cat("Item purification not requested! Nothing to show.")
  }
})

# Note setup
note_adj <- reactive({
  res <- NULL

  model <- DIF_adj_model()

  res$dmv <- paste(
    "DIF matching variable:",
    if (model$match[1] == "score") {
      "total score"
    } else if (model$match[1] == "zscore") {
      "standardized total score"
    } else {
      "standardized uploaded"
    }
  )


  res$type <- paste("DIF type tested:", switch(
    model$type,
    "both" = "any DIF ",
    "udif" = "uniform DIF ",
    "nudif" = "non-uniform DIF "
  ))

  res$p_adj <- paste("P-value correction method:", switch(
    model$p.adjust.method,
    "BH" = "Benjamini-Hochberg",
    "BY" = "Benjamini-Yekutieli",
    "bonferroni" = "Bonferroni",
    "holm" = "Holm",
    "hochberg" = "Hochberg",
    "hommel" = "Hommel",
    "none" = "none"
  ))

  res$puri <- paste("Item purification:", ifelse(model$purification == T, "used", "unutilized"))

  res
})

output$note_adj <- renderUI({
  withMathJax()

  HTML(
    paste(
      "Notes:",
      note_adj()$dmv,
      note_adj()$type,
      note_adj()$p_adj,
      note_adj()$puri,
      "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1",
      sep = "</br>"
    )
  )
})

# ** Download tables ######
output$download_adj_dif <- downloadHandler(
  filename = function() {
    paste("DIF_Adjacent_statistics", ".csv", sep = "")
  },
  content = function(file) {
    data <- coef_adj_dif()

    # remove all math-format characters -->> plaintext
    colnames(data) <- stringr::str_remove_all(colnames(data), "mathit|[%{}]|\\\\")

    write.csv(data[-4], file)
    write(paste(
      "Note:",
      note_adj()$dmv,
      note_adj()$type,
      note_adj()$p_adj,
      note_adj()$puri,
      "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1",
      sep = "\n"
    ), file, append = T)
  }
)
output$download_adj_dif_puri <- downloadHandler(
  filename = function() {
    paste0("DIF_Adjacent_purification", ".csv")
  },
  content = function(file) {
    data <- dif_adj_puri_table()
    write.csv(data, file)
  }
)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** ITEMS ######

# ** Plot ######
DIF_adj_plot_Input <- reactive({
  fit <- DIF_adj_model()
  item <- input$DIF_adj_items

  g <- plot(fit, item = item)[[1]] +
    theme_app() +
    ggtitle(item_names()[item]) +
    theme(
      legend.box.just = "top",
      legend.justification = c("left", "top"),
      legend.position = c(0.02, 0.98),
      legend.box = "horizontal",
      legend.margin = margin(0, 0, 0, 0, unit = "cm")
    )
  g
})

output$DIF_adj_plot <- renderPlot({
  DIF_adj_plot_Input()
})

# ** DB for plot ######
output$DB_DIF_adj_plot <- downloadHandler(
  filename = function() {
    paste0("fig_DIF_adj_", item_names()[input$DIF_adj_items], ".png")
  },
  content = function(file) {
    ggsave(file,
      plot = DIF_adj_plot_Input() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height,
      width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Table of coefficients ######
DIF_adj_coef_tab_Input <- reactive({
  fit <- DIF_adj_model()
  item <- input$DIF_adj_items
  cat <- sort(unique(data.frame(ordinal())[, item]))[-1]

  tab <- matrix(0, nrow = length(cat) + 3, ncol = 2)
  rownames(tab) <- c(paste0("(Intercept):", cat), "x", "group", "x:group")
  colnames(tab) <- c("estimate", "SE")

  tmp <- t(coef(fit, SE = T)[[item]])
  tab[rownames(tmp), ] <- tmp
  colnames(tab) <- c("Estimate", "SE")
  rownames(tab) <- c(
    paste0("%%mathit{b}_{0", cat, "}%%"), "%%mathit{b}_{1}%%",
    "%%mathit{b}_{2}%%", "%%mathit{b}_{3}%%"
  )

  tab
})

output$DIF_adj_coef_tab <- renderTable(
  {
    DIF_adj_coef_tab_Input()
  },
  include.rownames = T,
  include.colnames = T
)

# ** Equation ######
DIF_adj_equation_items_Input <- reactive({
  # txt1 <- ifelse(input$DIF_adj_matching_summary == "score", "X_p", "Z_p")
  txt1 <- "Z_p"
  txt2 <- paste0("b_{i0t} + b_{i1} ", txt1, " + b_{i2} G_p + b_{i3} ", txt1, " G_p")
  txt3 <- "Z_p, G_p"

  txt <- paste0("$$\\mathrm{P}(Y_{pi} = k|", txt3, ") = \\frac{e^{\\sum_{t = 0}^{k}", txt2, "}}{\\sum_{r = 0}^{K_i}e^{\\sum_{t = 0}^{r}", txt2, "}}$$")

  txt
})

output$DIF_adj_equation_items <- renderUI({
  withMathJax(HTML(DIF_adj_equation_items_Input()))
})

# ** Warning for missing values ####
output$DIF_adj_NA_warning_items <- renderUI({
  txt <- na_score()
  HTML(txt)
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * MULTINOMIAL ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** UPDATING INPUTS ######

# update selectInput & disable purification if DMV present

observe({
  if (dif_present() == TRUE) {
    lapply(match_multi, function(i) {
      updateSelectInput(
        session,
        paste0(i),
        choices = c(
          "Standardized total score" = "zscore",
          "Standardized uploaded" = "zuploaded"
        ),
        selected = "zscore"
      )
    })
  } else {
    lapply(match_multi, function(i) {
      updateSelectInput(
        session,
        paste0(i),
        choices = c(
          "Standardized total score" = "zscore"
        ),
        selected = "zscore"
      )
    })
  }
})

mapply(function(match, puri) {
  observeEvent(input[[paste0(match)]], {
    if (input[[paste0(match)]] %in% c("uploaded", "zuploaded")) {
      updateCheckboxInput(session, paste0(puri), value = FALSE)
      shinyjs::disable(paste0(puri))
    } else {
      shinyjs::enable(paste0(puri))
    }
  })
},
match = match_multi, puri = puri_multi
)

DDF_multi <- reactiveValues(
  type = NULL,
  correction = NULL,
  purification = NULL,
  matching = NULL
)

# ** Updating type ######
observeEvent(input$DDF_multi_type_summary, {
  DDF_multi$type <- input$DDF_multi_type_summary
})
observeEvent(input$DDF_multi_type_items, {
  DDF_multi$type <- input$DDF_multi_type_items
})
observeEvent(DDF_multi$type, {
  if (DDF_multi$type != input$DDF_multi_type_summary) {
    updateCheckboxGroupInput(
      session = session,
      inputId = "DDF_multi_type_summary",
      selected = DDF_multi$type
    )
  }
  if (DDF_multi$type != input$DDF_multi_type_items) {
    updateCheckboxGroupInput(
      session = session,
      inputId = "DDF_multi_type_items",
      selected = DDF_multi$type
    )
  }
})

# ** Updating correction ######
observeEvent(input$DDF_multi_correction_summary, {
  DDF_multi$correction <- input$DDF_multi_correction_summary
})
observeEvent(input$DDF_multi_correction_items, {
  DDF_multi$correction <- input$DDF_multi_correction_items
})
observeEvent(DDF_multi$correction, {
  if (DDF_multi$correction != input$DDF_multi_correction_summary) {
    updateSelectInput(
      session = session,
      inputId = "DDF_multi_correction_summary",
      selected = DDF_multi$correction
    )
  }
  if (DDF_multi$correction != input$DDF_multi_correction_items) {
    updateSelectInput(
      session = session,
      inputId = "DDF_multi_correction_items",
      selected = DDF_multi$correction
    )
  }
})

# ** Updating DMV ########
observeEvent(input$DIF_multi_summary_matching, {
  DDF_multi$matching <- input$DIF_multi_summary_matching
})
observeEvent(input$DIF_multi_items_matching, {
  DDF_multi$matching <- input$DIF_multi_items_matching
})
observeEvent(DDF_multi$matching, {
  if (DDF_multi$matching != input$DIF_multi_summary_matching) {
    updateCheckboxInput(
      session = session,
      inputId = "DIF_multi_summary_matching",
      value = DDF_multi$matching
    )
  }
  if (DDF_multi$matching != input$DIF_multi_items_matching) {
    updateCheckboxInput(
      session = session,
      inputId = "DIF_multi_items_matching",
      value = DDF_multi$matching
    )
  }
})

# ** Updating purification ######
observeEvent(input$DDF_multi_purification_summary, {
  DDF_multi$purification <- input$DDF_multi_purification_summary
})
observeEvent(input$DDF_multi_purification_items, {
  DDF_multi$purification <- input$DDF_multi_purification_items
})
observeEvent(DDF_multi$purification, {
  if (DDF_multi$purification != input$DDF_multi_purification_summary) {
    updateCheckboxInput(
      session = session,
      inputId = "DDF_multi_purification_summary",
      value = DDF_multi$purification
    )
  }
  if (DDF_multi$purification != input$DDF_multi_purification_items) {
    updateCheckboxInput(
      session = session,
      inputId = "DDF_multi_purification_items",
      value = DDF_multi$purification
    )
  }
})

# ** Updating item slider ######
observe({
  item_count <- ncol(ordinal())
  updateSliderInput(
    session = session,
    inputId = "DDF_multi_items",
    max = item_count
  )
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** SUMMARY ######

# ** Model for print ####
# it's the same as for items as the inputs are synchronized
DDF_multi_model <- reactive({
  data <- nominal()
  group <- unlist(group())
  key <- key()

  type <- input$DDF_multi_type_summary
  puri <- input$DDF_multi_purification_summary
  corr <- input$DDF_multi_correction_summary

  if (input$DIF_multi_summary_matching == "zscore") {
    match <- "zscore"
  } else if (input$DIF_multi_summary_matching == "zuploaded") {
    match <- scale(apply(as.data.frame(unlist(DIFmatching())), 1, sum))
  }

  fit <- tryCatch(ddfMLR(
    Data = data, group = group, focal.name = 1, match = match,
    key = key, p.adjust.method = corr,
    type = type, purify = puri, parametrization = "classic"
  ),
  error = function(e) e
  )

  validate(need(
    class(fit) == "ddfMLR",
    paste0("This method cannot be used on this data. Error returned: ", fit$message)
  ))

  fit
})

# ** Output print ######
output$DDF_multi_print <- renderPrint({
  print(DDF_multi_model())
})

# ** Warning for missing values ####
output$DDF_multi_NA_warning_summary <- renderUI({
  txt <- na_score()
  HTML(txt)
})


# ** DIF statistic and parameter tables ####
coef_multi_dif <- reactive({
  res <- DDF_multi_model()

  # only one pval var, based on model specs
  pval <- if (res$p.adjust.method == "none") {
    res$pval
  } else {
    res$adj.pval
  }

  pval_symb <- symnum(pval,
    c(0, 0.001, 0.01, 0.05, 0.1, 1),
    symbols = c("***", "**", "*", ".", "")
  )
  blank <- character(ncol(res$Data))


  # table
  tab_stat <-
    data.frame(
      check.names = FALSE,
      res$Sval,
      pval,
      pval_symb
    )

  colnames(tab_stat) <- c(
    "LR (%%mathit{\\chi^2}%%)",
    ifelse(
      res$p.adjust.method == "none",
      "%%mathit{p}%%-value",
      "adj. %%mathit{p}%%-value"
    ),
    ""
  )

  rownames(tab_stat) <- item_names()

  tab_stat
})

output$coef_multi_dif <- renderTable(
  {
    coef_multi_dif()
  },
  rownames = T,
  colnames = T
)

coef_multi_dif_pars <- reactive({
  res <- DDF_multi_model()

  tab_pars <-
    left_join(
      res$mlrPAR %>%
        map2_dfr(
          .y = item_names(),
          ~ .x %>%
            as.data.frame() %>%
            rownames_to_column("resp") %>%
            add_column(item = .y, .before = 1)
        ) %>% dplyr::rename(
          "b_0" = "(Intercept)",
          "b_1" = "x",
          "b_2" = "group",
          "b_3" = "x:group"
        ),
      res$mlrSE %>%
        map2_dfr(
          .y = item_names(),
          ~ .x %>%
            as.data.frame() %>%
            rownames_to_column("resp") %>%
            add_column(item = .y, .before = 1)
        ) %>% dplyr::rename(
          "SE(b_0)" = "(Intercept)",
          "SE(b_1)" = "x",
          "SE(b_2)" = "group",
          "SE(b_3)" = "x:group"
        ),
      by = c("item", "resp")
    ) %>%
    dplyr::mutate(rowname = paste0(item, " (", resp, ")")) %>%
    column_to_rownames("rowname") %>%
    replace(is.na(.), 0) %>%
    dplyr::select(-item, -resp) %>%
    dplyr::select(order(c(seq(4), seq(4))))

  colnames(tab_pars) <-
    str_replace(colnames(tab_pars), "(?=b)", "%%mathit{") %>%
    str_replace("(?<=\\d)", "}%%")

  tab_pars
})

output$coef_multi_dif_pars <- renderTable(
  {
    coef_multi_dif_pars()
  },
  rownames = T,
  colnames = T
)

# ** Items detected text ######
output$multi_dif_items <- renderPrint({
  DDFitems <- DDF_multi_model()$DDFitems
  if (DDFitems[1] == "No DDF item detected") {
    txt <- "No item was detected as DDF."
  } else {
    txt <- paste0("Items detected as DDF items: ", paste(item_names()[DDFitems], collapse = ", "))
  }
  HTML(txt)
})

# ** Purification table ######
dif_multi_puri_table <- reactive({
  tab <- DDF_multi_model()$ddfPur

  if (!is.null(tab)) {
    colnames(tab) <- item_names()
    rownames(tab) <- paste0("Step ", seq(0, nrow(tab) - 1))
    tab
  }
})

output$dif_multi_puri_table <- renderTable(
  {
    dif_multi_puri_table()
  },
  rownames = T,
  colnames = T,
  digits = 0
)

# ** Purification info - number of iter ######
output$dif_multi_puri_info <- renderPrint({
  model <- DDF_multi_model()
  if (input$DDF_multi_purification_summary & !is.null(DDF_multi_model()$ddfPur)) {
    cat("Table below describes purification process, where rows correspond to purification iteration and columns to items.
        Value of '1' in the i-th row means that an item was detected as DDF in (i-1)-th step, while value of '0' means that
        item was not detected as DDF. The first row corresponds to the initial classification of the items when all items
        were used for calculation of DDF matching criterion. ")
    nrIter <- model$nrPur
    cat(
      "In this case, the convergence was", ifelse(model$conv.puri, "reached", "NOT reached even"), "after", nrIter,
      ifelse(nrIter == 1, "iteration.", "iterations.")
    )
  } else if (input$DDF_multi_purification_summary & is.null(DDF_multi_model()$ddfPur)) {
    cat("No DDF items detected whatsoever, nothing to show.")
  } else {
    cat("Item purification not requested! Nothing to show.")
  }
})

# Note setup
note_multi <- reactive({
  res <- NULL

  model <- DDF_multi_model()

  res$dmv <- paste(
    "DDF matching variable:",
    if (model$match[1] == "score") {
      "total score"
    } else if (model$match[1] == "zscore") {
      "standardized total score"
    } else {
      "standardized uploaded"
    }
  )


  res$type <- paste("DDF type tested:", switch(
    model$type,
    "both" = "any DDF ",
    "udif" = "uniform DDF ",
    "nudif" = "non-uniform DDF "
  ))

  res$p_adj <- paste("P-value correction method:", switch(
    model$p.adjust.method,
    "BH" = "Benjamini-Hochberg",
    "BY" = "Benjamini-Yekutieli",
    "bonferroni" = "Bonferroni",
    "holm" = "Holm",
    "hochberg" = "Hochberg",
    "hommel" = "Hommel",
    "none" = "none"
  ))

  res$puri <- paste("Item purification:", ifelse(model$purification == T, "used", "unutilized"))

  res
})

output$note_multi <- output$note_multi_2 <- renderUI({
  withMathJax()

  HTML(
    paste(
      "Notes:",
      note_multi()$dmv,
      note_multi()$type,
      note_multi()$p_adj,
      note_multi()$puri,
      "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1",
      sep = "</br>"
    )
  )
})

# ** Download tables ######
output$download_multi_dif <- downloadHandler(
  filename = function() {
    paste("DDF_Multinomial_statistics", ".csv", sep = "")
  },
  content = function(file) {
    data <- coef_multi_dif()

    write.csv(data, file)
    write(paste(
      "Note:",
      note_multi()$dmv,
      note_multi()$type,
      note_multi()$p_adj,
      note_multi()$puri,
      "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1",
      sep = "\n"
    ), file, append = T)
  }
)

# params separately
output$download_multi_dif_pars <- downloadHandler(
  filename = function() {
    paste("DDF_Multinomial_parameters", ".csv", sep = "")
  },
  content = function(file) {
    data <- coef_multi_dif_pars()

    write.csv(data, file)
    write(paste(
      "Note:",
      note_multi()$dmv,
      note_multi()$type,
      note_multi()$p_adj,
      note_multi()$puri,
      "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1",
      sep = "\n"
    ), file, append = T)
  }
)

output$download_multi_dif_puri <- downloadHandler(
  filename = function() {
    paste0("DDF_Multinomial_purification", ".csv")
  },
  content = function(file) {
    data <- dif_multi_puri_table()
    write.csv(data, file)
  }
)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** ITEMS ######

# ** Plot ######
# DDF_multi_plot_Input <- reactive({
#   fit <- DDF_multi_model()
#   item <- input$DDF_multi_items
#
#   g <- plot(fit, item = item)[[1]] +
#     theme_app() +
#     ggtitle(item_names()[item]) +
#     theme(legend.box.just = "top",
#           legend.justification = c("left", "top"),
#           legend.position = c(0.02, 0.98),
#           legend.box = "horizontal",
#           legend.margin = margin(0, 0, 0, 0, unit = "cm"))
#   g
# })

# ** Plot all items ######
DDF_multi_plot_all_Input <- reactive({
  fit <- DDF_multi_model()

  g <- list()
  for (i in 1:ncol(nominal())) {
    g[[i]] <- plot(fit, item = i)[[1]] +
      theme_app() +
      ggtitle(item_names()[i]) +
      theme(
        legend.box.just = "top",
        legend.justification = c("left", "top"),
        legend.position = c(0.02, 0.98),
        legend.box = "horizontal",
        legend.margin = margin(0, 0, 0, 0, unit = "cm")
      )
  }
  g
})

# ** Plot ######
DDF_multi_plot_Input <- reactive({
  item <- input$DDF_multi_items

  g <- DDF_multi_plot_all_Input()
  g[[item]]
})

# ** Output plot ######
output$DDF_multi_plot <- renderPlot({
  DDF_multi_plot_Input()
})

# ** DB for plot ######
output$DB_DDF_multi_plot <- downloadHandler(
  filename = function() {
    paste0("fig_DDF_multi_", item_names()[input$DDF_multi_items], ".png")
  },
  content = function(file) {
    ggsave(file,
      plot = DDF_multi_plot_Input() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height,
      width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

output$DB_DDF_multi_plot_all <- downloadHandler(
  filename = function() {
    paste0("fig_DDF_multi_all.zip")
  },
  content = function(file) {
    # go to a temp dir to avoid permission issues
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    files <- NULL

    # loop through the sheets
    for (i in 1:ncol(ordinal())) {
      # write each sheet to a csv file, save the name
      fileName <- paste0("fig_DDF_multi_", item_names()[i], ".png")
      ggsave(fileName,
        plot = DDF_multi_plot_all_Input()[[i]] +
          theme(text = element_text(size = setting_figures$text_size)),
        device = "png",
        height = setting_figures$height,
        width = setting_figures$width,
        dpi = setting_figures$dpi
      )
      files <- c(fileName, files)
    }
    # create the zip file
    zip(file, files)
  }
)

# ** Table of coefficients ######
output$DDF_multi_coef_tab <- renderTable(
  {
    fit <- DDF_multi_model()
    item <- input$DDF_multi_items

    data <- as.data.frame(nominal())
    key <- as.factor(key())

    tmp <- as.factor(data[, item])
    nams <- levels(tmp)[!(levels(tmp) %in% key[item])]

    tab_coef <- fit$mlrPAR[[item]]
    if (is.null(dim(tab_coef))) tab_coef <- matrix(tab_coef, nrow = 1)
    tab_se <- fit$mlrSE[[item]]
    tab_se <- matrix(tab_se, ncol = ncol(tab_coef), byrow = T)

    if (ncol(tab_coef) == 2) {
      tab_coef <- data.frame(tab_coef, 0, 0)
      tab_se <- data.frame(tab_se, 0, 0)
    } else {
      if (ncol(tab_coef) == 3) {
        tab_coef <- data.frame(tab_coef, 0)
        tab_se <- data.frame(tab_se, 0)
      }
    }

    colnames(tab_se) <- colnames(tab_coef) <- c("b0", "b1", "b2", "b3")
    rownames(tab_se) <- rownames(tab_coef) <- nams

    tab_coef <- data.frame(tab_coef, answ = rownames(tab_coef))
    tab_se <- data.frame(tab_se, answ = rownames(tab_se))

    df1 <- tab_coef %>% tidyr::pivot_longer(-answ)
    df2 <- tab_se %>% tidyr::pivot_longer(-answ)

    tab <- data.frame(
      df1$value,
      df2$value
    )
    rownames(tab) <- paste0(
      "%%mathit{", substr(df1$name, 1, 1), "}_{",
      df1$answ,
      substr(df1$name, 2, 2), "}%%"
    )
    colnames(tab) <- c("Estimate", "SE")
    tab
  },
  rownames = TRUE
)

# ** Equation ######
output$DDF_multi_equation_items <- renderUI({
  item <- input$DDF_multi_items
  key <- key()

  cor_option <- key[item]
  withMathJax(
    sprintf(
      "$$\\text{For item } %s \\text{ are corresponding equations of multinomial model given by: } \\\\
      \\mathrm{P}(Y_{pi} = %s|Z_p, G_p) =
      \\frac{1}{1 + \\sum_l e^{\\left( b_{i0l} + b_{i1l} Z_p + b_{i2l} G_p + b_{i3l} Z_p:G_p\\right)}}, \\\\
      \\mathrm{P}(Y_{pi} = k|Z_p, G_p) =
      \\frac{e^{\\left( b_{i0k} + b_{i1k} Z_p + b_{i2k} G_p + b_{i3k} Z_p:G_p\\right)}}
      {1 + \\sum_l e^{\\left(b_{i0l} + b_{i1l} Z_p + b_{i2l} G_p + b_{i3l} Z_p:G_p\\right)}}, \\\\
      \\text{where } %s \\text{ is the correct answer and } k \\text{ is one of the wrong options. }$$",
      item, cor_option, cor_option, cor_option, cor_option
    )
  )
})

# ** Warning for missing values ####
output$DDF_multi_NA_warning_items <- renderUI({
  txt <- na_score()
  HTML(txt)
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** REPORTS ######

# ** Model for report ######
DDF_multi_model_report <- reactive({
  if (!input$customizeCheck) {
    adj.method <- input$correction_method_print_DDF
    type <- input$type_print_DDF
    purify <- input$puri_DDF_print


    fit <- DDF_multi_model()
  } else {
    data <- nominal()
    group <- unlist(group())
    key <- key()

    # if (input$matching_DDF_report == "uploaded") {
    #   match <- unlist(DIFmatching())
    # } else if (input$matching_DDF_report == "zscore") {
    #   match <- "zscore"
    # } else {
    #   match <- "score"
    # }

    type <- input$type_DDF_report
    puri <- input$puri_DDF_report
    corr <- input$correction_method_DDF_report

    fit <- tryCatch(ddfMLR(
      Data = data, group = group, focal.name = 1, # match = match,
      key = key, p.adjust.method = corr,
      type = type, purify = puri, parametrization = "classic"
    ),
    error = function(e) e
    )

    validate(need(
      class(fit) == "ddfMLR",
      paste0("This method cannot be used on this data. Error returned: ", fit$message)
    ))
  }

  fit
})

# ** Plot for report ######
DDF_multi_plot_report <- reactive({
  fit <- DDF_multi_model_report()

  graflist <- list()
  if (fit$DDFitems[[1]] != "No DDF item detected") {
    for (i in 1:length(fit$DDFitems)) {
      g <- plot(fit, item = fit$DDFitems[i])[[1]] +
        theme(
          text = element_text(size = 12),
          plot.title = element_text(size = 12, face = "bold", vjust = 1.5)
        ) +
        ggtitle(paste("\nDDF multinomial plot for item ", item_numbers()[fit$DDFitems[i]]))
      graflist[[i]] <- g
    }
  } else {
    graflist <- NULL
  }
  graflist
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * TRAINING ######
# ** Plot for training ######
DIF_training_plot_Input <- reactive({
  aR <- input$DIF_training_parameter_aR
  bR <- input$DIF_training_parameter_bR
  cR <- 0
  dR <- 1

  aF <- input$DIF_training_parameter_aF
  bF <- input$DIF_training_parameter_bF
  cF <- 0
  dF <- 1

  theta0 <- input$DIF_training_parameter_theta

  ccirt <- function(theta, a, b, c, d) {
    return(c + (d - c) / (1 + exp(-a * (theta - b))))
  }

  probR <- ccirt(theta0, a = aR, b = bR, c = cR, d = dR)
  probF <- ccirt(theta0, a = aF, b = bF, c = cF, d = dF)

  df <- data.frame(
    Reference = ccirt(seq(-4, 4, 0.01), aR, bR, cR, dR),
    Focal = ccirt(seq(-4, 4, 0.01), aF, bF, cF, dF),
    Ability = seq(-4, 4, 0.01)
  )

  df <- tidyr::pivot_longer(df, -Ability, names_to = "Group", values_to = "Probability")
  df$Group <- factor(df$Group, levels = c("Reference", "Focal"))

  g <- ggplot(data = df, aes(x = Ability, y = Probability, col = Group, linetype = Group)) +
    geom_line(size = 0.8) +
    geom_segment(aes(
      y = probR, yend = probR,
      x = -4, xend = theta0
    ),
    color = "gray", linetype = "dotdash"
    ) +
    geom_segment(aes(
      y = probF, yend = probF,
      x = -4, xend = theta0
    ),
    color = "gray", linetype = "dotdash"
    ) +
    geom_segment(aes(
      y = 0,
      yend = max(probR, probF),
      x = theta0, xend = theta0
    ),
    color = "gray", linetype = "dotdash"
    ) +
    xlim(-4, 4) +
    xlab("Ability") +
    ylab("Probability of correct answer") +
    ylim(0, 1) +
    scale_color_manual(
      name = "",
      values = c("blue", "#e6b800"),
      labels = c(
        paste(paste(letters[1:2], "=", c(aR, bR)),
          collapse = ", "
        ),
        paste(paste(paste(letters[1:2], "=", c(aF, bF))),
          collapse = ", "
        )
      )
    ) +
    scale_linetype_manual(
      name = "",
      values = c("solid", "dashed"),
      labels = c(
        paste(paste(letters[1:2], "=", c(aR, bR)),
          collapse = ", "
        ),
        paste(paste(paste(letters[1:2], "=", c(aF, bF))),
          collapse = ", "
        )
      )
    ) +
    theme_app()
  g
})
# ** Plotly for training ######
output$DIF_training_plot <- renderPlotly({
  g <- DIF_training_plot_Input()
  p <- ggplotly(g)

  # Reference group, probabilities
  text <- gsub("Group: Reference<br />Group: Reference", "Group: Reference", p$x$data[[1]]$text)
  p$x$data[[1]]$text <- text

  # Focal group, probabilities
  text <- gsub("Group: Focal<br />Group: Focal", "Group: Focal", p$x$data[[2]]$text)
  p$x$data[[2]]$text <- text

  # Reference group, selected theta
  text <- paste(strsplit(p$x$data[[3]]$text, "<br />")[[1]][-c(1:4)], collapse = "<br />")
  text <- gsub("Group: gray<br />Group: dotdash", "Group: Reference", text)
  p$x$data[[3]]$text <- text

  # Focal group, selected theta
  text <- paste(strsplit(p$x$data[[4]]$text, "<br />")[[1]][-c(1:4)], collapse = "<br />")
  text <- gsub("Group: gray<br />Group: dotdash", "Group: Focal", text)
  p$x$data[[4]]$text <- text

  # Selected theta
  text <- paste(strsplit(p$x$data[[5]]$text, "<br />")[[1]][5], collapse = "<br />")
  text <- paste(text, "<br />Group: Reference<br />Group: Focal")
  p$x$data[[5]]$text <- text

  p$elementId <- NULL

  p %>% plotly::config(displayModeBar = FALSE)
})

# ** DB for plot ######
output$DB_DIF_training_plot <- downloadHandler(
  filename = function() {
    paste("fig_DIFtraining.png", sep = "")
  },
  content = function(file) {
    ggsave(file,
      plot = DIF_training_plot_Input() +
        theme(
          legend.position = c(0.97, 0.03),
          legend.justification = c(0.97, 0.03)
        ) +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height,
      width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Exercise 1 ######
# *** Correct answers for Exercise 1 ######
DIF_training_correct_answers_1 <- reactive({
  ccirt <- function(theta, a, b, c, d) {
    return(1 / (1 + exp(-a * (theta - b))))
  }
  # Exercise 1.1
  # correct parameters for reference and focal group
  aR <- 1
  bR <- 0
  aF <- 1
  bF <- 1
  parR <- c(aR, bR)
  parF <- c(aF, bF)

  # Exercise 1.3
  # probability calculation
  theta0 <- c(-2, 0, 2)

  probR <- ccirt(theta0, aR, bR)
  probF <- ccirt(theta0, aF, bF)

  correct_answers <- list(
    Ex1_1 = list(
      parR = parR,
      parF = parF
    ),
    Ex1_2 = "uniform",
    Ex1_3 = list(
      probR = probR,
      probF = probF
    ),
    Ex1_4 = "reference"
  )
  correct_answers
})

# ** Evaluation of answers for Exercise 1 ######
DIF_training_answers_check_1 <- eventReactive(input$DIF_training_1_submit, {
  correct_answers <- DIF_training_correct_answers_1()

  # Exercise 1.1
  aR <- input$DIF_training_parameter_aR
  bR <- input$DIF_training_parameter_bR
  aF <- input$DIF_training_parameter_aF
  bF <- input$DIF_training_parameter_bF
  parR <- c(aR, bR)
  parF <- c(aF, bF)

  check1_1 <- c(all(abs(parR - correct_answers[["Ex1_1"]]$parR) <= 0.05) &
    all(abs(parF - correct_answers[["Ex1_1"]]$parF) <= 0.05))
  # Exercise 1.2
  check1_2 <- input$DIF_training_1_2 == correct_answers[["Ex1_2"]]

  # Exercise 1.3
  probR <- c(input$DIF_training_1_3_1R, input$DIF_training_1_3_2R, input$DIF_training_1_3_3R)
  probF <- c(input$DIF_training_1_3_1F, input$DIF_training_1_3_2F, input$DIF_training_1_3_3F)
  check1_3R <- abs(probR - correct_answers[["Ex1_3"]]$probR) <= 0.05
  check1_3F <- abs(probF - correct_answers[["Ex1_3"]]$probF) <= 0.05

  # Exercise 1.4
  check1_4 <- input$DIF_training_1_4 == correct_answers[["Ex1_4"]]

  check <- list(
    check1_1 = check1_1,
    check1_2 = check1_2,
    check1_3R = check1_3R,
    check1_3F = check1_3F,
    check1_4 = check1_4
  )
  res <- sum(sapply(check, sum)) / sum(sapply(check, length))
  ans <- lapply(check, function(x) {
    ifelse(is.na(x),
      "<b><font color = 'red'>!</font></b>",
      ifelse(x,
        "<font color='green'>&#10004;</font>",
        "<font color='red'>&#10006;</font>"
      )
    )
  })
  ans[["total"]] <- res
  ans
})
# *** Checkmarks for Exercise 1 ######
output$DIF_training_1_1_answer <- renderUI({
  HTML(DIF_training_answers_check_1()[["check1_1"]])
})
output$DIF_training_1_2_answer <- renderUI({
  HTML(DIF_training_answers_check_1()[["check1_2"]])
})
output$DIF_training_1_3_1R_answer <- renderUI({
  HTML(DIF_training_answers_check_1()[["check1_3R"]][1])
})
output$DIF_training_1_3_2R_answer <- renderUI({
  HTML(DIF_training_answers_check_1()[["check1_3R"]][2])
})
output$DIF_training_1_3_3R_answer <- renderUI({
  HTML(DIF_training_answers_check_1()[["check1_3R"]][3])
})
output$DIF_training_1_3_1F_answer <- renderUI({
  HTML(DIF_training_answers_check_1()[["check1_3F"]][1])
})
output$DIF_training_1_3_2F_answer <- renderUI({
  HTML(DIF_training_answers_check_1()[["check1_3F"]][2])
})
output$DIF_training_1_3_3F_answer <- renderUI({
  HTML(DIF_training_answers_check_1()[["check1_3F"]][3])
})
output$DIF_training_1_4_answer <- renderUI({
  HTML(DIF_training_answers_check_1()[["check1_4"]])
})
output$DIF_training_1_answer <- renderUI({
  res <- DIF_training_answers_check_1()[["total"]]
  HTML(ifelse(is.na(res),
    "<font color = 'red'>Check the format</font>",
    ifelse(res == 1,
      "<font color='green'>Everything correct! Well done!</font>",
      paste0("<font color='red'>", round(100 * res), "% correct. Try again.</font>")
    )
  ))
})

# ** Exercise 2 ######
# *** Correct answers for Exercise 2 ######
DIF_training_correct_answers_2 <- reactive({
  ccirt <- function(theta, a, b, c, d) {
    return(1 / (1 + exp(-a * (theta - b))))
  }
  # Exercise 2.1
  # correct parameters for reference and focal group
  aR <- 0.8
  bR <- -0.5
  aF <- 1.5
  bF <- 1
  parR <- c(aR, bR)
  parF <- c(aF, bF)

  # Exercise 2.3
  # probability calculation
  theta0 <- c(-1, 0, 1)

  probR <- ccirt(theta0, aR, bR)
  probF <- ccirt(theta0, aF, bF)

  correct_answers <- list(
    Ex2_1 = list(
      parR = parR,
      parF = parF
    ),
    Ex2_2 = "nonuniform",
    Ex2_3 = list(
      probR = probR,
      probF = probF
    ),
    Ex2_4 = "depends"
  )
  correct_answers
})

# ** Evaluation of answers for Exercise 2 ######
DIF_training_answers_check_2 <- eventReactive(input$DIF_training_2_submit, {
  correct_answers <- DIF_training_correct_answers_2()

  # Exercise 2.1
  aR <- input$DIF_training_parameter_aR
  bR <- input$DIF_training_parameter_bR
  aF <- input$DIF_training_parameter_aF
  bF <- input$DIF_training_parameter_bF
  parR <- c(aR, bR)
  parF <- c(aF, bF)

  check2_1 <- c(all(abs(parR - correct_answers[["Ex2_1"]]$parR) <= 0.05) &
    all(abs(parF - correct_answers[["Ex2_1"]]$parF) <= 0.05))
  # Exercise 2.2
  check2_2 <- input$DIF_training_2_2 == correct_answers[["Ex2_2"]]

  # Exercise 2.3
  probR <- c(input$DIF_training_2_3_1R, input$DIF_training_2_3_2R, input$DIF_training_2_3_3R)
  probF <- c(input$DIF_training_2_3_1F, input$DIF_training_2_3_2F, input$DIF_training_2_3_3F)
  check2_3R <- abs(probR - correct_answers[["Ex2_3"]]$probR) <= 0.05
  check2_3F <- abs(probF - correct_answers[["Ex2_3"]]$probF) <= 0.05

  # Exercise 2.4
  check2_4 <- input$DIF_training_2_4 == correct_answers[["Ex2_4"]]

  check <- list(
    check2_1 = check2_1,
    check2_2 = check2_2,
    check2_3R = check2_3R,
    check2_3F = check2_3F,
    check2_4 = check2_4
  )
  res <- sum(sapply(check, sum)) / sum(sapply(check, length))
  ans <- lapply(check, function(x) {
    ifelse(is.na(x),
      "<b><font color = 'red'>!</font></b>",
      ifelse(x,
        "<font color='green'>&#10004;</font>",
        "<font color='red'>&#10006;</font>"
      )
    )
  })
  ans[["total"]] <- res
  ans
})

# *** Checkmarks for Exercise 2 ######
output$DIF_training_2_1_answer <- renderUI({
  HTML(DIF_training_answers_check_2()[["check2_1"]])
})
output$DIF_training_2_2_answer <- renderUI({
  HTML(DIF_training_answers_check_2()[["check2_2"]])
})
output$DIF_training_2_3_1R_answer <- renderUI({
  HTML(DIF_training_answers_check_2()[["check2_3R"]][1])
})
output$DIF_training_2_3_2R_answer <- renderUI({
  HTML(DIF_training_answers_check_2()[["check2_3R"]][2])
})
output$DIF_training_2_3_3R_answer <- renderUI({
  HTML(DIF_training_answers_check_2()[["check2_3R"]][3])
})
output$DIF_training_2_3_1F_answer <- renderUI({
  HTML(DIF_training_answers_check_2()[["check2_3F"]][1])
})
output$DIF_training_2_3_2F_answer <- renderUI({
  HTML(DIF_training_answers_check_2()[["check2_3F"]][2])
})
output$DIF_training_2_3_3F_answer <- renderUI({
  HTML(DIF_training_answers_check_2()[["check2_3F"]][3])
})
output$DIF_training_2_4_answer <- renderUI({
  HTML(DIF_training_answers_check_2()[["check2_4"]])
})
output$DIF_training_2_answer <- renderUI({
  res <- DIF_training_answers_check_2()[["total"]]
  HTML(ifelse(is.na(res),
    "<font color = 'red'>Check the format</font>",
    ifelse(res == 1,
      "<font color='green'>Everything correct! Well done!</font>",
      paste0("<font color='red'>", round(100 * res), "% correct. Try again.</font>")
    )
  ))
})
