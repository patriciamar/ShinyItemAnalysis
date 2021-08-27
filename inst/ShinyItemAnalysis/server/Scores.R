# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SUMMARY  ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * TOTAL SCORES ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Tooltips for total scores ####
output$totalscores_tooltip_mean <- renderUI({
  span(
    class = "ttooltip",
    style = "color: #2286bf",
    "mean,",
    span(
      class = "ttooltiptext",
      withMathJax("$$\\bar{X} = \\frac{1}{n}\\sum_{p = 1}^n X_p$$")
    )
  )
})
output$totalscores_tooltip_sd <- renderUI({
  span(
    class = "ttooltip",
    style = "color: #2286bf",
    "sample standard deviation",
    span(
      class = "ttooltiptext",
      withMathJax("$$\\textrm{SD}(X) = \\sqrt{\\frac{1}{n - 1}\\sum_{p = 1}^n (X_p - \\bar{X})^2}$$")
    )
  )
})
output$totalscores_tooltip_skewness <- renderUI({
  span(
    class = "ttooltip",
    style = "color: #2286bf",
    "sample skewness,",
    span(
      class = "ttooltiptext",
      withMathJax("$$\\frac{\\frac{1}{n} \\sum_{p = 1}^n (X_p - \\bar{X})^3}{\\left[\\frac{1}{n - 1} \\sum_{p = 1}^n (X_p - \\bar{X})^2\\right]^{3/2}}$$")
    )
  )
})
output$totalscores_tooltip_kurtosis <- renderUI({
  span(
    class = "ttooltip",
    style = "color: #2286bf",
    "sample kurtosis.",
    span(
      class = "ttooltiptext",
      withMathJax("$$\\frac{\\frac{1}{n} \\sum_{p = 1}^n (X_p - \\bar{X})^4}{\\left[\\frac{1}{n} \\sum_{p = 1}^n (X_p - \\bar{X})^2\\right]^2}$$")
    )
  )
})

# ** Update slider input ####
observe({
  total_score <- total_score()
  updateSliderInput(
    session = session,
    inputId = "slider_totalscores_histogram",
    min = floor(min(total_score, na.rm = TRUE)),
    max = ceiling(max(total_score, na.rm = TRUE)),
    value = round(median(total_score, na.rm = TRUE))
  )
})

# ** Total scores summary table ####
totalscores_table_Input <- reactive({
  sc <- total_score()
  n <- length(sc)

  tab <- data.table(rbind(c(
    n,
    min(sc, na.rm = TRUE),
    max(sc, na.rm = TRUE),
    mean(sc, na.rm = TRUE),
    median(sc, na.rm = TRUE),
    sd(sc, na.rm = TRUE),
    ShinyItemAnalysis:::skewness(sc),
    ShinyItemAnalysis:::kurtosis(sc)
  )))
  colnames(tab) <- c("n", "Min", "Max", "Mean", "Median", "SD", "Skewness", "Kurtosis")
  tab$n <- as.integer(tab$n)
  tab
})

# ** Output total scores summary table ####
output$totalscores_table <- renderTable(
  {
    totalscores_table_Input()
  },
  digits = 2,
  include.rownames = FALSE,
  include.colnames = TRUE
)

# ** Histogram of total scores ####
totalscores_histogram_Input <- reactive({
  sc <- total_score()
  bin <- as.numeric(input$slider_totalscores_histogram)

  df <- data.frame(
    Score = sc,
    Group = ifelse(sc == bin, "gray",
      ifelse(sc < bin, "red", "blue")
    )
  )

  binwidth <- min(abs(diff(unique(sc))))
  cols <- c("red", "gray", "blue")[c("red", "gray", "blue") %in% unique(df$Group)]
  df$Group <- factor(df$Group, cols)

  ggplot(df, aes(x = Score, fill = Group)) +
    geom_histogram(aes(y = ..count.. / sum(..count..)), binwidth = binwidth, color = "black") +
    scale_fill_manual(values = cols) +
    labs(
      x = "Total score",
      y = "Proportion of respondents"
    ) +
    theme_app()
})

# ** Output histogram of total scores ####
output$totalscores_histogram <- renderPlotly({
  sc <- total_score()
  bin <- as.numeric(input$slider_totalscores_histogram)

  df <- data.frame(
    Score = sc,
    Group = ifelse(sc == bin, "gray",
      ifelse(sc < bin, "red", "blue")
    )
  )
  dfCount <- data.frame(table(df$Score))
  colnames(dfCount) <- c("Score", "Count")
  df <- merge(df, dfCount)

  binwidth <- min(abs(diff(unique(sc))))
  cols <- c("red", "gray", "blue")[c("red", "gray", "blue") %in% unique(df$Group)]
  df$Group <- factor(df$Group, cols)

  g <- totalscores_histogram_Input()
  p <- ggplotly(g)

  for (i in 1:length(p$x$data)) {
    p$x$data[[i]]$x <- p$x$data[[i]]$x[p$x$data[[i]]$y > 0]
    p$x$data[[i]]$y <- round(p$x$data[[i]]$y[p$x$data[[i]]$y > 0], 3)
    p$x$data[[i]]$text <- paste0(
      "Score: ", p$x$data[[i]]$x, "<br />",
      "Proportion: ", p$x$data[[i]]$y, "<br />",
      "Count: ", unique(df[df$Group == p$x$data[[i]]$name, ])$Count
    )
  }

  p %>% plotly::config(displayModeBar = FALSE)
})

# ** DB histogram of total scores ####
output$DB_totalscores_histogram <- downloadHandler(
  filename = function() {
    paste("fig_TotalScores_histogram.png", sep = "")
  },
  content = function(file) {
    ggsave(file,
      plot = totalscores_histogram_Input() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * STANDARD SCORES ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Tooltips for total scores ####
output$standardscores_tooltip_total <- renderUI({
  span(
    class = "ttooltip",
    style = "color: #2286bf",
    "sum of item scores.",
    span(
      class = "ttooltiptext",
      withMathJax("$$X_p = \\sum_{i = 1}^m Y_{pi}$$")
    )
  )
})

# ** Tooltips for Z-score ####
output$standardscores_tooltip_zscore <- renderUI({
  span(
    class = "ttooltip",
    style = "color: #2286bf",
    "a linear transformation of the total score",
    span(
      class = "ttooltiptext",
      withMathJax("$$Z_p = \\frac{X_p - \\bar{X}}{\\textrm{SD}(X)}$$")
    )
  )
})

# ** Tooltips for T-score ####
output$standardscores_tooltip_tscore <- renderUI({
  span(
    class = "ttooltip",
    style = "color: #2286bf",
    "a linear transformation of the Z-score",
    span(
      class = "ttooltiptext",
      withMathJax("$$T_p = 10 Z_p + 50$$")
    )
  )
})

# ** Table for scores ####
standardscores_table_Input <- reactive({
  sc <- total_score()
  # k <- ifelse(is.character(key()), length(key()), sum(key()))
  sc_max <- max(sc, na.rm = TRUE)

  # total score
  tosc <- sort(unique(sc))
  # percentile
  perc <- ecdf(sc)(tosc)
  # success rate
  sura <- (tosc / sc_max) * 100
  # Z score
  zsco <- sort(unique(z_score()))
  # T score
  tsco <- 50 + 10 * zsco

  tab <- round(data.table(tosc, perc, sura, zsco, tsco), 2)
  colnames(tab) <- c("Total score", "Percentile", "Success rate", "Z-score", "T-score")

  tab
})

# ** Output table for scores ####
output$standardscores_table <- renderTable(
  {
    standardscores_table_Input()
  },
  include.rownames = FALSE
)

# exportTestValues(summ_ts = totalscores_table_Input(),
#                  summ_ss = standardscores_table_Input())

# ** Download table with standard scores ** ####
output$DB_standardscores_table <- downloadHandler(
  filename = function() {
    paste0("standard_scores", ".csv")
  },
  content = function(file) {
    data <- standardscores_table_Input()
    write.csv(data, file)
  }
)
