# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# TRADITIONAL ANALYSIS ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * ITEM ANALYSIS ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** Double slider inicialization for DD plot ######
observe({
  val <- input$DDplotNumGroupsSlider
  updateSliderInput(session, "DDplotRangeSlider",
    min = 1,
    max = val,
    step = 1,
    value = c(1, val)
  )
})

# ** DD plot text ######
output$DDplot_text <- renderUI({
  range1 <- input$DDplotRangeSlider[[1]]
  range2 <- input$DDplotRangeSlider[[2]]

  if (any(range1 != 1, range2 != 3, input$DDplotNumGroupsSlider != 3)) {
    HTML(paste(
      "Discrimination is defined as a difference in average (scaled) item score between the ",
      "<b>", range1, "</b>",
      ifelse(range1 >= 4, "-th", switch(range1, "1" = "-st", "2" = "-nd", "3" = "-rd")),
      " and <b>", range2, "</b>",
      ifelse(range2 >= 4, "-th", switch(range2, "1" = "-st", "2" = "-nd", "3" = "-rd")),
      " group out of total number of ",
      "<b>", input$DDplotNumGroupsSlider, "</b>",
      " groups. ",
      sep = ""
    ))
  }
})

# ** Difficulty/Discrimination plot ######
DDplot_Input <- reactive({
  correct <- ordinal()

  difc_type <- input$DDplotDiscriminationDifficulty
  average.score <- (difc_type == "AVGS")

  validate(need(
    input$DDplotRangeSlider[[2]] <= input$DDplotNumGroupsSlider,
    ""
  ))

  DDplot(correct,
    item.names = item_numbers(),
    k = input$DDplotNumGroupsSlider,
    l = input$DDplotRangeSlider[[1]], u = input$DDplotRangeSlider[[2]],
    discrim = input$DDplotDiscriminationSelect,
    average.score = average.score,
    thr = switch(input$DDplotThr_cb, "TRUE" = input$DDplotThr, "FALSE" = NULL)
  )
})

# ** Difficulty/Discrimination plot for report ######
DDplot_Input_report <- reactive({
  correct <- ordinal()
  if (input$customizeCheck) {
    difc_type <- input$DDplotDiscriminationDifficulty_report
    average.score <- (difc_type == "AVGS")

    DDplot(correct,
      item.names = item_numbers(),
      k = input$DDplotNumGroupsSlider_report,
      l = input$DDplotRangeSlider_report[[1]], u = input$DDplotRangeSlider_report[[2]],
      discrim = input$DDplotDiscriminationSelect_report
    )
  } else {
    DDplot_Input()
  }
})

# ** Output for Diif/Disr. plot with plotly ######
output$DDplot <- renderPlotly({
  p <- DDplot_Input() %>%
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

  if (input$DDplotThr_cb == TRUE) {
    p$x$data[[3]][["text"]] <-
      str_replace(p$x$data[[3]][["text"]], "yintercept", "Threshold")
  }

  p %>% plotly::config(displayModeBar = F)
})

# ** DB Difficulty/Discrimination plot ######
output$DB_DDplot <- downloadHandler(
  filename = function() {
    paste("fig_DDplot.png", sep = "")
  },
  content = function(file) {
    ggsave(file,
      plot = DDplot_Input() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Cronbach's alpha note ######

cronbach_note <- reactive({
  cronbach <- NULL

  cronbach$est <- round(psych::alpha(ordinal())$total[1], 2)

  cronbach$sd <- round(psych::alpha(ordinal())$total[8], 2)

  cronbach
})

output$cronbach_note <- renderUI({
  withMathJax(HTML(
    paste0(
      "<sup>1</sup>Estimate (SD) of Cronbach's $\\alpha$ for the test as a whole is: ",
      cronbach_note()$est,
      " (",
      cronbach_note()$sd,
      ")."
    )
  ))
})


# ** Traditional item analysis table text ######
output$itemanalysis_table_text <- renderUI({
  range1 <- input$DDplotRangeSlider[[1]]
  range2 <- input$DDplotRangeSlider[[2]]
  num.groups <- input$DDplotNumGroupsSlider
  withMathJax(HTML(paste0(
    "<b>Explanation:<br>Diff.</b>&nbsp;",
    "&ndash; item difficulty estimated as average item score divided by its range, ",
    "<b>Avg. score</b>&nbsp;",
    "&ndash; average item score, ",
    "<b>SD</b>&nbsp;",
    "&ndash; standard deviation, ",
    "<b>RIT</b>&nbsp;",
    "&ndash; Pearson correlation between item and total score, ",
    "<b>RIR</b>&nbsp;",
    "&ndash; Pearson correlation between item and rest of items, ",
    "<b>ULI</b>&nbsp;",
    "&ndash; Upper-Lower Index, ",
    "<b>$\\alpha$ drop </b>&nbsp;",
    "&ndash; Cronbach\'s $\\alpha$ of test without given item (the value for the test as a whole is presented in the note below), ",
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
    "<b>Rel.</b>&nbsp;",
    "&ndash; item reliability index, see Allen & Yen (1979; Ch. 6.4), ",
    "<b>Rel. drop</b>&nbsp;",
    "&ndash; as previous, but scored without the respective item, ",
    "<b>I-C cor.</b>&nbsp;",
    "&ndash; item-criterion correlation, ",
    "<b>Val. index</b>&nbsp;",
    "&ndash; validity index, as described by Allen & Yen (1979; Ch. 6.4), ",
    "<b>Missed</b>&nbsp;",
    "&ndash; proportion of missed responses on the particular item, ",
    "<b>Not-reached</b>&nbsp;",
    "&ndash; proportion of respondents that did not reached the item nor the subsequent ones"
  )))
})


# ** Traditional item analysis table - revised ######
itemanalysis_table_Input <- reactive({
  k <- input$DDplotNumGroupsSlider
  l <- input$DDplotRangeSlider[[1]]
  u <- input$DDplotRangeSlider[[2]]

  item_crit_cor <- if (any(crit_wo_val() == "missing", na.rm = TRUE)) {
    NULL
  } else {
    unlist(crit_wo_val())
  }

  tab <-
    ItemAnalysis(ordinal(),
      y = item_crit_cor,
      k, l, u
    )

  # rename colnames
  colnames(tab) <- c(
    "Diff.",
    "Avg. score",
    "SD",
    "min",
    "max",
    "obtMin",
    "obtMax",
    "cutScore",
    "gULI",
    "ULI",
    "RIT",
    "RIR",
    "I-C cor.",
    "Val. index",
    "Rel.",
    "Rel. drop",
    "Alpha drop",
    "Missed [%]",
    "Not-reached [%]"
  )

  # prepare cols selection
  ia_sel_vars <- c(
    "Diff.",
    "Avg. score",
    "SD",
    "ULI",
    if (k != 3 | l != 1 | u != 3) {
      "gULI"
    },
    "RIT",
    "RIR",
    "Alpha drop",
    "Rel.",
    "Rel. drop",
    if (!is.null(item_crit_cor)) {
      c(
        "I-C cor.",
        "Val. index"
      )
    },
    "Missed [%]",
    "Not-reached [%]"
  )

  tab <- tab[, ia_sel_vars]

  row.names(tab) <- item_names()

  tab
})


# ** Traditional item analysis table for report ######
itemanalysis_table_report_Input <- reactive({
  a <- nominal()
  k <- key()
  correct <- ordinal()

  range1 <- ifelse(input$customizeCheck,
    input$DDplotRangeSlider_report[[1]],
    input$DDplotRangeSlider[[1]]
  )
  range2 <- ifelse(input$customizeCheck,
    input$DDplotRangeSlider_report[[2]],
    input$DDplotRangeSlider[[2]]
  )
  num.groups <- ifelse(input$customizeCheck,
    input$DDplotNumGroupsSlider_report,
    input$DDplotNumGroupsSlider
  )

  tab <- ItemAnalysis(correct)
  tab <- data.table(
    item_numbers(),
    tab[, c("diff", "avgScore", "SD", "ULI", "RIT", "RIR", "alphaDrop")]
  )
  tab <- cbind(tab, gDiscrim(correct, k = num.groups, l = range1, u = range2))
  colnames(tab) <- c(
    "Item", "Difficulty", "Average score", "SD", "Discrimination ULI",
    "Discrimination RIT", "Discrimination RIR", "Alpha Drop",
    "Customized Discrimination"
  )
  tab
})


# ** Output traditional item analysis table ######
output$coef_itemanalysis_table <- renderTable(
  {
    tab <- itemanalysis_table_Input()
    colnames(tab)[which(colnames(tab) == "Alpha drop")] <- "%%mathit{\\alpha}%% drop%%mathit{\\mathrm{^1}}%%"
    tab
  },
  rownames = TRUE
)


# ** Download traditional item analysis table ######
output$download_itemanal_table <- downloadHandler(
  filename = function() {
    paste("Item_Analysis", ".csv", sep = "")
  },
  content = function(file) {
    data <- itemanalysis_table_Input()
    write.csv(data, file)
    write(
      paste0(
        "Note: Estimate (SD) of Cronbach's alpha for the test as a whole is: ",
        cronbach_note()$est,
        " (",
        cronbach_note()$sd,
        ")."
      ),
      file,
      append = T
    )
  }
)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * DISTRACTORS ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Admisible groups for cut ####
distractor_admisible_groups <- reactive({
  sc <- total_score()

  sc_quant <- lapply(1:5, function(i) quantile(sc, seq(0, 1, by = 1 / i), na.rm = TRUE))
  sc_quant_unique <- sapply(sc_quant, function(i) !any(duplicated(i)))

  groups <- c(1:5)[sc_quant_unique]
  groups
})

# ** Status of changing cut ####
distractor_change_cut_indicator <- reactiveValues(change = FALSE)

# ** Updating cut slider ####
observeEvent(!(input$distractor_group %in% distractor_admisible_groups()), {
  if (!(input$distractor_group %in% distractor_admisible_groups())) {
    distractor_change_cut_indicator$change <- TRUE
    c <- max(distractor_admisible_groups(), na.rm = T)
    updateSliderInput(session, "distractor_group", value = c)
  }
})

# ** Warning for not unique cuts ####
output$distractor_groups_alert <- renderUI({
  if (distractor_change_cut_indicator$change) {
    txt <- paste0(
      '<font color = "orange">The cut of criterion variable was not unique. The maximum number of
                  groups, for which criterion variable is unique is ',
      max(distractor_admisible_groups(), na.rm = T), ".</font>"
    )
    HTML(txt)
  } else {
    txt <- " "
    HTML(txt)
  }
})

# ** Distractor text ######
output$distractor_text <- renderUI({
  txt1 <- paste("Respondents are divided into ")
  txt2 <- paste("<b>", input$distractor_group, "</b>")
  txt3 <- paste("groups by their total score. Subsequently, we display percentage
                 of respondents in each group who selected given answer (correct answer or distractor).
                 The correct answer should be more often selected by respondents with higher total score
                 than by those with lower total score, i.e.")
  txt4 <- paste("<b>", "solid line should be increasing.", "</b>")
  txt5 <- paste("The distractor should work in opposite direction, i.e. ")
  txt6 <- paste("<b>", "dotted lines should be decreasing.", "<b>")
  HTML(paste(txt1, txt2, txt3, txt4, txt5, txt6))
})

# ** Distractors plot ######
distractor_plot_Input <- reactive({
  num.group <- input$distractor_group
  a <- nominal()
  k <- key()
  i <- input$distractorSlider
  sc <- total_score()
  multiple.answers <- c(input$type_combinations_distractor == "Combinations")

  plotDistractorAnalysis(
    data = a, key = k, num.group = num.group,
    item = i,
    item.name = item_names()[i],
    multiple.answers = multiple.answers,
    matching = sc
  ) +
    xlab("Group by total score")
})

# ** Output distractors plot ######
output$distractor_plot <- renderPlot({
  distractor_plot_Input()
})

# ** DB distractors plot ######
output$DB_distractor_plot <- downloadHandler(
  filename = function() {
    paste("fig_DistractorPlot_", item_names()[input$distractorSlider], ".png", sep = "")
  },
  content = function(file) {
    ggsave(file,
      plot = distractor_plot_Input() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Distractor table with counts ######
distractor_table_counts_Input <- reactive({
  num.group <- input$distractor_group
  a <- nominal()
  k <- key()
  item <- input$distractorSlider
  sc <- total_score()

  DA <- DistractorAnalysis(a, k, num.groups = num.group, matching = sc)[[item]]
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
output$distractor_table_counts <- renderTable({
  distractor_table_counts_Input()
})

# ** Distractor table with proportions ######
distractor_table_proportions_Input <- reactive({
  a <- nominal()
  k <- key()
  num.group <- input$distractor_group
  item <- input$distractorSlider
  sc <- total_score()

  DA <- DistractorAnalysis(a, k, num.groups = num.group, p.table = TRUE, matching = sc)[[item]]
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
  distractor_table_proportions_Input()
})

# ** Item response patterns barplot ######
distractor_barplot_item_response_patterns_Input <- reactive({
  a <- nominal()
  k <- key()
  num.group <- 1
  item <- input$distractorSlider
  sc <- total_score()

  DA <- DistractorAnalysis(a, k, num.groups = num.group, p.table = TRUE, matching = sc)[[item]]
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
output$distractor_barplot_item_response_patterns <- renderPlot({
  distractor_barplot_item_response_patterns_Input()
})

# ** DB item response patterns barplot ######
output$DB_distractor_barplot_item_response_patterns <- downloadHandler(
  filename = function() {
    paste("fig_ItemResponsePatterns_", item_names()[input$distractorSlider], ".png", sep = "")
  },
  content = function(file) {
    ggsave(file,
      plot = distractor_barplot_item_response_patterns_Input() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Distractors histograms by group ######
distractor_histogram_Input <- reactive({
  a <- nominal()
  k <- key()
  num.groups <- input$distractor_group
  sc <- total_score()
  sc.level <- cut(sc, quantile(sc, seq(0, 1, by = 1 / num.groups), na.rm = TRUE), include.lowest = T)

  df <- data.table(sc, gr = sc.level)
  col <- c("darkred", "red", "orange", "gold", "green3")
  col <- switch(input$distractor_group,
    "1" = col[4],
    "2" = col[4:5],
    "3" = col[c(2, 4:5)],
    "4" = col[2:5],
    "5" = col
  )
  ggplot(df, aes(x = sc)) +
    geom_histogram(aes(fill = gr, group = gr), binwidth = 1, color = "black") +
    scale_fill_manual("", values = col) +
    labs(
      x = "Total score",
      y = "Number of respondents"
    ) +
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(0, max(table(sc)) + 0.01 * nrow(a))
    ) +
    scale_x_continuous(limits = c(-0.5 + min(sc), max(sc) + 0.5)) +
    theme_app()
})

# ** Output distractors histograms by group ######
output$distractor_histogram <- renderPlot({
  distractor_histogram_Input()
})

# ** DB distractors histograms by group ######
output$DB_distractor_histogram <- downloadHandler(
  filename = function() {
    paste("fig_HistrogramByDistractorGroups.png", sep = "")
  },
  content = function(file) {
    ggsave(file,
      plot = distractor_histogram_Input() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Distractor analysis table by group ######
distractor_table_total_score_by_group_Input <- reactive({
  sc <- total_score()
  num.group <- input$distractor_group

  sc.level <- quantile(sc, seq(0, 1, by = 1 / num.group), na.rm = TRUE)

  tab <- table(cut(sc,
    sc.level,
    include.lowest = T,
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
    distractor_table_total_score_by_group_Input()
  },
  include.colnames = TRUE,
  include.rownames = TRUE
)

# ** Status of changing cut in reports ####
distractor_change_cut_indicator_report <- reactiveValues(change = FALSE)

# ** Updating report cut slider ####
observeEvent(list(input$customizeCheck, !(input$distractor_group_report %in% distractor_admisible_groups())), {
  if (!(input$distractor_group_report %in% distractor_admisible_groups())) {
    distractor_change_cut_indicator_report$change <- TRUE
    c <- max(distractor_admisible_groups(), na.rm = T)
    updateSliderInput(session, "distractor_group_report", value = c)
  }
})

# ** Warning for not unique cuts for reports ####
output$distractor_groups_alert_report <- renderUI({
  if (distractor_change_cut_indicator_report$change) {
    txt <- paste0('<font color = "orange">The cut of criterion variable was not unique. The maximum number of
                  groups, for which criterion variable is unique is ', max(distractor_admisible_groups(), na.rm = T), ".</font>")
    HTML(txt)
  } else {
    txt <- " "
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
    multiple.answers_report <- c(input$type_combinations_distractor == "Combinations")
    num.group <- input$distractor_group
  } else {
    multiple.answers_report <- c(input$type_combinations_distractor_report == "Combinations")
    num.group <- input$distractor_group_report
  }

  graflist <- list()

  for (i in 1:length(k)) {
    g <- plotDistractorAnalysis(
      data = a, key = k, num.group = num.group,
      item = i,
      item.name = item_names()[i],
      multiple.answers = multiple.answers_report,
      matching = sc
    ) +
      xlab("Group by total score")
    g <- g +
      ggtitle(paste("Distractor plot for item", item_numbers()[i])) +
      theme_app()
    graflist[[i]] <- g
  }
  graflist
})
