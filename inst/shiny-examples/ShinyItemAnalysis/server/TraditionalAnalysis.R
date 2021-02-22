# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# TRADITIONAL ANALYSIS ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * ITEM ANALYSIS ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Double slider initialization for DD plot ######
observe({
  val <- input$itemanalysis_DDplot_groups_slider
  updateSliderInput(session, "itemanalysis_DDplot_range_slider",
    min = 1,
    max = val,
    step = 1,
    value = c(1, val)
  )
})

# * Double slider initialization for DD plot report ######
observe({
  val <- input$report_itemanalysis_DDplot_groups_slider
  updateSliderInput(session, "report_itemanalysis_DDplot_range_slider",
    min = 1,
    max = val,
    step = 1,
    value = c(1, val)
  )
})

# ** DD plot text ######
output$itemanalysis_DDplot_text <- renderUI({
  range1 <- input$itemanalysis_DDplot_range_slider[[1]]
  range2 <- input$itemanalysis_DDplot_range_slider[[2]]

  if (any(range1 != 1, range2 != 3, input$itemanalysis_DDplot_groups_slider != 3)) {
    HTML(paste0(
      "Discrimination is defined as a difference in average (scaled) item score between the ",
      "<b>", range1, "</b>",
      ifelse(range1 >= 4, "-th", switch(range1, "1" = "-st", "2" = "-nd", "3" = "-rd")),
      " and <b>", range2, "</b>",
      ifelse(range2 >= 4, "-th", switch(range2, "1" = "-st", "2" = "-nd", "3" = "-rd")),
      " group out of total number of ",
      "<b>", input$itemanalysis_DDplot_groups_slider, "</b>",
      " groups. "
    ))
  }
})

# ** DD plot ######
itemanalysis_DDplot <- reactive({
  correct <- ordinal()
  average.score <- (input$itemanalysis_DDplot_difficulty == "AVGS")

  validate(need(
    input$itemanalysis_DDplot_range_slider[[2]] <= input$itemanalysis_DDplot_groups_slider,
    ""
  ))

  DDplot(Data = correct,
    item.names = item_numbers(),
    k = input$itemanalysis_DDplot_groups_slider,
    l = input$itemanalysis_DDplot_range_slider[[1]],
    u = input$itemanalysis_DDplot_range_slider[[2]],
    discrim = input$itemanalysis_DDplot_discrimination,
    average.score = average.score,
    thr = switch(input$itemanalysis_DDplot_threshold,
      "TRUE" = input$itemanalysis_DDplot_threshold_value,
      "FALSE" = NULL
    )
  )
})

# ** DD plot for report ######
report_itemanalysis_DDplot <- reactive({
  correct <- ordinal()

  if (input$customizeCheck) {
    average.score <- (input$report_itemanalysis_DDplot_difficulty == "AVGS")

    DDplot(Data = correct,
      item.names = item_numbers(),
      k = input$report_itemanalysis_DDplot_groups_slider,
      l = input$report_itemanalysis_DDplot_range_slider[[1]],
      u = input$report_itemanalysis_DDplot_range_slider[[2]],
      discrim = input$report_itemanalysis_DDplot_discrimination
    )
  } else {
    itemanalysis_DDplot()
  }
})

# ** Output for DD plot with plotly ######
output$itemanalysis_DDplot <- renderPlotly({
  p <- itemanalysis_DDplot() %>%
    ggplotly(tooltip = c("item", "fill", "value", "yintercept"))

  # renaming/removing unnecessary text
  for (i in 1:2) {
    for (j in 1:length(p$x$data[[i]][["text"]])) {
      p$x$data[[i]][["text"]][j] <-
        str_remove(
          str_replace(
            str_remove_all(p$x$data[[i]][["text"]][j], "parameter: |value: "),
            "item",
            "Item"
          ),
          "(?<=\\.\\d{3}).*"
        )
    }
  }

  if (input$itemanalysis_DDplot_threshold) {
    p$x$data[[3]][["text"]] <-
      str_replace(p$x$data[[3]][["text"]], "yintercept", "Threshold")
  }

  p %>% plotly::config(displayModeBar = FALSE)
})

# ** DB Difficulty/Discrimination plot ######
output$itemanalysis_DDplot_download <- downloadHandler(
  filename = function() {
    "fig_DDplot.png"
  },
  content = function(file) {
    ggsave(file,
      plot = itemanalysis_DDplot() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Cronbach's alpha note ######
itemanalysis_cronbach_note <- reactive({
  cronbach <- list()

  cronbach$est <- round(psych::alpha(ordinal())$total[1], 2)
  cronbach$sd <- round(psych::alpha(ordinal())$total[8], 2)

  cronbach
})

output$itemanalysis_cronbach_note <- renderUI({
  withMathJax(HTML(
    paste0(
      "<sup>1</sup>Estimate (SD) of Cronbach's $\\alpha$ for the test as a whole is: ",
      itemanalysis_cronbach_note()$est,
      " (",
      itemanalysis_cronbach_note()$sd,
      ")."
    )
  ))
})

# ** Traditional item analysis table text ######
output$itemanalysis_table_text <- renderUI({
  range1 <- input$itemanalysis_DDplot_range_slider[[1]]
  range2 <- input$itemanalysis_DDplot_range_slider[[2]]
  num.groups <- input$itemanalysis_DDplot_groups_slider
  withMathJax(HTML(paste0(
    "<b>Explanation:<br>Diff.</b>&nbsp;",
    "&ndash; item difficulty estimated as an average item score divided by its range, ",
    "<b>Avg. score</b>&nbsp;",
    "&ndash; average item score, ",
    "<b>SD</b>&nbsp;",
    "&ndash; standard deviation, ",
    "<b>ULI</b>&nbsp;",
    "&ndash; Upper-Lower Index, ",
    if (num.groups != 3 | range1 != 1 | range2 != 3) {
      paste0(
        "<b>gULI</b>&nbsp;",
        "&ndash; generalized ULI, difference between the difficulty recorded in the ", range1,
        ifelse(range1 >= 4, "-th", switch(range1, "1" = "-st", "2" = "-nd", "3" = "-rd")),
        " and ", range2,
        ifelse(range2 >= 4, "-th", switch(range2, "1" = "-st", "2" = "-nd", "3" = "-rd")),
        " group out of total number of ", num.groups, " groups, "
      )
    },
    "<b>RIT</b>&nbsp;",
    "&ndash; Pearson correlation between item and total score, ",
    "<b>RIR</b>&nbsp;",
    "&ndash; Pearson correlation between item and rest of the items, ",
    "<b>Rel.</b>&nbsp;",
    "&ndash; item reliability index, ",
    "<b>Rel. drop</b>&nbsp;",
    "&ndash; as previous, but scored without the respective item, ",
    "<b>I-C cor.</b>&nbsp;",
    "&ndash; item-criterion correlation, ",
    "<b>Val. index</b>&nbsp;",
    "&ndash; item validity index, ",
    "<b>$\\alpha$ drop </b>&nbsp;",
    "&ndash; Cronbach\'s $\\alpha$ of test without given item (the value for the test as a whole is presented in the note below), ",
    "<b>Missed</b>&nbsp;",
    "&ndash; percentage of missed responses on the particular item, ",
    "<b>Not-reached</b>&nbsp;",
    "&ndash; percentage of respondents that did not reach the item nor the subsequent ones"
  )))
})

# ** Traditional item analysis table ######
itemanalysis_table <- reactive({
  k <- input$itemanalysis_DDplot_groups_slider
  l <- input$itemanalysis_DDplot_range_slider[[1]]
  u <- input$itemanalysis_DDplot_range_slider[[2]]

  item_crit_cor <- if (any(crit_wo_val() == "missing", na.rm = TRUE)) {
    "none"
  } else {
    unlist(crit_wo_val())
  }

  tab <-
    ItemAnalysis(Data = ordinal(),
      criterion = item_crit_cor,
      k, l, u,
      minscore = minimal(),
      maxscore = maximal()
    )

  tab <- tab[, !(colnames(tab) %in% c("Prop.max.score"))]

  if (item_crit_cor[1] == "none") {
    colnames(tab) <- c(
      "Diff.", "Avg. score", "SD", "Min", "Max", "obsMin", "obsMax",
      "gULI", "ULI", "RIT", "RIR",
      "Rel.", "Rel. drop",
      "Alpha drop",
      "Missed [%]", "Not-reached [%]"
    )
  } else {
    colnames(tab) <- c(
      "Diff.", "Avg. score", "SD", "Min", "Max", "obsMin", "obsMax",
      "gULI", "ULI", "RIT", "RIR",
      "I-C cor.", "Val. index",
      "Rel.", "Rel. drop",
      "Alpha drop",
      "Missed [%]", "Not-reached [%]"
    )
  }

  remove_gULI <- (k == 3 & l == 1 & u == 3)
  if (remove_gULI) {
    tab <- tab[, !(colnames(tab) %in% "gULI")]
  }

  row.names(tab) <- item_names()
  tab
})

# ** Traditional item analysis table for report ######
report_itemanalysis_table <- reactive({
  a <- nominal()
  k <- key()
  correct <- ordinal()

  range1 <- ifelse(input$customizeCheck,
    input$report_itemanalysis_DDplot_range_slider[[1]],
    input$itemanalysis_DDplot_range_slider[[1]]
  )
  range2 <- ifelse(input$customizeCheck,
    input$report_itemanalysis_DDplot_range_slider[[2]],
    input$itemanalysis_DDplot_range_slider[[2]]
  )
  num.groups <- ifelse(input$customizeCheck,
    input$report_itemanalysis_DDplot_groups_slider,
    input$itemanalysis_DDplot_groups_slider
  )

  tab <- ItemAnalysis(Data = correct)
  tab <- data.table(
    item_numbers(),
    tab[, c("Difficulty", "Mean", "SD", "ULI", "RIT", "RIR", "Alpha.drop")]
  )
  tab <- cbind(tab, gDiscrim(Data = correct, k = num.groups, l = range1, u = range2))
  colnames(tab) <- c(
    "Item", "Difficulty", "Average score", "SD", "Discrimination ULI",
    "Discrimination RIT", "Discrimination RIR", "Alpha Drop",
    "Customized Discrimination"
  )
  tab
})

# ** Output traditional item analysis table ######
output$itemanalysis_table_coef <- renderTable(
  {
    tab <- itemanalysis_table()
    colnames(tab)[which(colnames(tab) == "Alpha drop")] <- "%%mathit{\\alpha}%% drop%%mathit{\\mathrm{^1}}%%"
    tab
  },
  rownames = TRUE
)

# ** Download traditional item analysis table ######
output$itemanalysis_table_download <- downloadHandler(
  filename = function() {
    "Item_Analysis.csv"
  },
  content = function(file) {
    data <- itemanalysis_table()
    write.csv(data, file)
    write(
      paste0(
        "Note: Estimate (SD) of Cronbach's alpha for the test as a whole is: ",
        itemanalysis_cronbach_note()$est,
        " (",
        itemanalysis_cronbach_note()$sd,
        ")."
      ),
      file,
      append = TRUE
    )
  }
)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * DISTRACTORS ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Updating item slider ######
observe({
  item_count <- ncol(binary())

  updateSliderInput(
    session = session,
    inputId = "distractor_item_slider",
    max = item_count
  )
})

# ** Admissible groups for cut ####
distractor_admissible_groups <- reactive({
  sc <- total_score()

  sc_quant <- lapply(1:5, function(i) quantile(sc, seq(0, 1, by = 1 / i), na.rm = TRUE))
  sc_quant_unique <- sapply(sc_quant, function(i) !any(duplicated(i)))

  groups <- c(1:5)[sc_quant_unique]
  groups
})

# ** Status of changing cut ####
distractor_change_cut_indicator <- reactiveValues(change = FALSE)

# ** Updating cut slider ####
observeEvent(!(input$distractor_group_slider %in% distractor_admissible_groups()), {
  if (!(input$distractor_group_slider %in% distractor_admissible_groups())) {
    distractor_change_cut_indicator$change <- TRUE
    c <- max(distractor_admissible_groups(), na.rm = TRUE)
    updateSliderInput(session, "distractor_group_slider", value = c)
  }
})

# ** Warning for not unique cuts ####
output$distractor_groups_alert <- renderUI({
  if (distractor_change_cut_indicator$change) {
    txt <- paste0(
      '<font color = "orange">The cut of criterion variable was not unique. The maximum number of
                  groups for which criterion variable is unique is ',
      max(distractor_admissible_groups(), na.rm = TRUE), ".</font>"
    )
    HTML(txt)
  } else {
    txt <- ""
    HTML(txt)
  }
})

# ** Distractor text ######
output$distractor_text <- renderUI({
  txt1 <- paste("Respondents are divided into ")
  txt2 <- paste("<b>", input$distractor_group_slider, "</b>")
  txt3 <- paste("groups by their total score. For each group, we subsequently display a proportion
                 of respondents who have selected a given response.
                 In case of multiple-choice items, the correct answer should be selected more often by respondents with a higher total score
                 than by those with lower total scores, i.e.,")
  txt4 <- paste("<b>", "solid line should be increasing.", "</b>")
  txt5 <- paste("The distractor should work in the opposite direction, i.e.,")
  txt6 <- paste("<b>", "dotted lines should be decreasing.", "<b>")
  HTML(paste(txt1, txt2, txt3, txt4, txt5, txt6))
})

# ** Distractor plot ######
distractor_plot <- reactive({
  i <- input$distractor_item_slider

  plotDistractorAnalysis(
    Data = nominal(),
    key = key(),
    num.group = input$distractor_group_slider,
    item = i,
    item.name = item_names()[i],
    multiple.answers = input$distractor_type == "Combinations",
    criterion = total_score()
  ) + xlab("Group by total score")
})

# ** Output distractors plot ######
output$distractor_plot <- renderPlotly({
  g <- distractor_plot()
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

# ** DB distractors plot ######
output$distractor_plot_download <- downloadHandler(
  filename = function() {
    paste0("fig_DistractorPlot_", item_names()[input$distractor_item_slider], ".png")
  },
  content = function(file) {
    ggsave(file,
      plot = distractor_plot() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Distractor table with counts ######
distractor_table_counts <- reactive({
  num.group <- input$distractor_group_slider
  a <- nominal()
  k <- key()
  item <- input$distractor_item_slider
  sc <- total_score()

  DA <- DistractorAnalysis(Data = a, key = k, num.groups = num.group, criterion = sc)[[item]]
  # df <- dcast(as.data.frame(DA), response ~ score.level, sum, margins = T, value.var = "Freq")
  df <- DA %>%
    addmargins() %>%
    as.data.frame.matrix() %>%
    add_column(.before = 1, Response = as.factor(rownames(.)))
  colnames(df) <- c("Response", paste("Group", 1:ifelse(num.group > (ncol(df) - 2), ncol(df) - 2, num.group)), "Total")
  levels(df$Response)[nrow(df)] <- "Total"
  rownames(df) <- NULL
  df
})

# ** Output distractor table with counts ######
output$distractor_table_counts <- renderTable(
  {
    distractor_table_counts()
  },
  digits = 0
)

# ** Distractor table with proportions ######
distractor_table_proportions <- reactive({
  a <- nominal()
  k <- key()
  num.group <- input$distractor_group_slider
  item <- input$distractor_item_slider
  sc <- total_score()

  DA <- DistractorAnalysis(Data = a, key = k, num.groups = num.group, p.table = TRUE, criterion = sc)[[item]]
  # df <- dcast(as.data.frame(DA), response ~ score.level, sum, value.var = "Freq")
  df <- DA %>%
    as.data.frame.matrix() %>%
    add_column(.before = 1, Response = as.factor(rownames(.)))
  colnames(df) <- c("Response", paste("Group", 1:ifelse(num.group > (ncol(df) - 1), ncol(df) - 1, num.group)))
  rownames(df) <- NULL
  df
})

# ** Output distractor table with proportions ######
output$distractor_table_proportions <- renderTable({
  distractor_table_proportions()
})

# ** Item response patterns barplot ######
distractor_barplot_item_response_patterns <- reactive({
  a <- nominal()
  k <- key()
  num.group <- 1
  item <- input$distractor_item_slider
  sc <- total_score()

  DA <- DistractorAnalysis(Data = a, key = k, num.groups = num.group, p.table = TRUE, criterion = sc)[[item]]
  # df <- dcast(as.data.frame(DA), response ~ score.level, sum, value.var = "Freq")
  df <- DA %>%
    as.data.frame.matrix() %>%
    add_column(.before = 1, Response = as.factor(rownames(.)))
  colnames(df) <- c("Response", "Proportion")
  rownames(df) <- NULL

  ggplot(df, aes(x = Response, y = Proportion)) +
    geom_bar(stat = "identity") +
    xlab("Item response pattern") +
    ylab("Relative frequency") +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    theme_app() +
    ggtitle(item_names()[item])
})

# ** Output item response patterns barplot ######
output$distractor_barplot_item_response_patterns <- renderPlotly({
  g <- distractor_barplot_item_response_patterns()
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

# ** DB item response patterns barplot ######
output$distractor_barplot_item_response_patterns_download <- downloadHandler(
  filename = function() {
    paste0("fig_ItemResponsePatterns_", item_names()[input$distractor_item_slider], ".png")
  },
  content = function(file) {
    ggsave(file,
      plot = distractor_barplot_item_response_patterns() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Distractor histograms by group ######
distractor_histogram <- reactive({
  a <- nominal()
  k <- key()
  num.groups <- input$distractor_group_slider
  sc <- total_score()
  sc.level <- cut(sc, quantile(sc, seq(0, 1, by = 1 / num.groups), na.rm = TRUE), include.lowest = TRUE)

  df <- data.frame(Score = sc, Group = sc.level)
  col <- c("darkred", "red", "orange2", "gold1", "green3")
  col <- switch(input$distractor_group_slider,
    "1" = col[4],
    "2" = col[4:5],
    "3" = col[c(2, 4:5)],
    "4" = col[2:5],
    "5" = col
  )
  ggplot(df, aes(x = Score, fill = Group, group = Group)) +
    geom_histogram(binwidth = 1, color = "black") +
    scale_fill_manual("", values = col) +
    labs(
      x = "Total score",
      y = "Number of respondents"
    ) +
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(0, max(table(sc), na.rm = TRUE) + 0.01 * nrow(a))
    ) +
    scale_x_continuous(
      limits = c(-0.5 + min(sc, na.rm = TRUE), max(sc, na.rm = TRUE) + 0.5)
    ) +
    theme_app()
})

# ** Output distractors histograms by group ######
output$distractor_histogram <- renderPlotly({
  g <- distractor_histogram()
  p <- ggplotly(g)

  for (i in 1:length(p$x$data)) {
    text <- p$x$data[[i]]$text
    text <- lapply(strsplit(text, split = "<br />"), unique)
    text <- unlist(lapply(text, paste, collapse = "<br />"))
    text <- gsub("count", "Count", text)
    text <- gsub("Score", "Total score", text)
    p$x$data[[i]]$text <- text
  }

  p$elementId <- NULL
  p %>% plotly::config(displayModeBar = FALSE)
})

# ** DB distractors histograms by group ######
output$distractor_histogram_download <- downloadHandler(
  filename = function() {
    "fig_HistrogramByDistractorGroups.png"
  },
  content = function(file) {
    ggsave(file,
      plot = distractor_histogram() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Distractor analysis table by group ######
distractor_table_total_score_by_group <- reactive({
  sc <- total_score()
  num.group <- input$distractor_group_slider

  sc.level <- quantile(sc, seq(0, 1, by = 1 / num.group), na.rm = TRUE)

  tab <- table(cut(sc,
    sc.level,
    include.lowest = TRUE,
    labels = sc.level[-1]
  ))
  tab <- t(data.frame(tab))
  tab <- matrix(round(as.numeric(tab), 2), nrow = 2)

  rownames(tab) <- c("Max points", "Count")
  colnames(tab) <- paste("Group", 1:num.group)

  tab
})

# ** Output distractor analysis table by group ######
output$distractor_table_total_score_by_group <- renderTable(
  {
    distractor_table_total_score_by_group()
  },
  include.colnames = TRUE,
  include.rownames = TRUE,
  digits = 0
)

# ** Status of changing cut in reports ####
report_distractor_change_cut_indicator <- reactiveValues(change = FALSE)

# ** Updating report cut slider ####
observeEvent(list(input$customizeCheck, !(input$report_distractor_group_slider %in% distractor_admissible_groups())), {
  if (!(input$report_distractor_group_slider %in% distractor_admissible_groups())) {
    report_distractor_change_cut_indicator$change <- TRUE
    c <- max(distractor_admissible_groups(), na.rm = TRUE)
    updateSliderInput(session, "report_distractor_group_slider", value = c)
  }
})

# ** Warning for not unique cuts for reports ####
output$report_distractor_groups_alert <- renderUI({
  if (report_distractor_change_cut_indicator$change) {
    txt <- paste0('<font color = "orange">The cut of criterion variable was not unique. The maximum number of
                  groups, for which criterion variable is unique is ', max(distractor_admissible_groups(), na.rm = TRUE), ".</font>")
    HTML(txt)
  } else {
    txt <- ""
    HTML(txt)
  }
})

# ** Report distractors plot ######
report_distractor_plot <- reactive({
  a <- nominal()
  colnames(a) <- item_names()
  k <- key()
  sc <- total_score()

  if (!input$customizeCheck) {
    multiple.answers_report <- c(input$distractor_type == "Combinations")
    num.group <- input$distractor_group_slider
  } else {
    multiple.answers_report <- c(input$report_distractor_type == "Combinations")
    num.group <- input$report_distractor_group_slider
  }

  graflist <- list()

  for (i in 1:length(k)) {
    g <- plotDistractorAnalysis(
      Data = a, key = k, num.group = num.group,
      item = i,
      item.name = item_names()[i],
      multiple.answers = multiple.answers_report,
      criterion = sc
    ) +
      xlab("Group by total score")
    g <- g +
      ggtitle(paste("Distractor plot for item", item_numbers()[i])) +
      theme_app()
    graflist[[i]] <- g
  }
  graflist
})
