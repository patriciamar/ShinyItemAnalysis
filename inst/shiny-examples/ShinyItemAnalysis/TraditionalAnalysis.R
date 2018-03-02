#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# TRADITIONAL ANALYSIS ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * ITEM ANALYSIS ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** Double slider inicialization for DD plot ######
observe({
  val <- input$DDplotNumGroupsSlider
  updateSliderInput(session, "DDplotRangeSlider",
                    min = 1,
                    max = val,
                    step = 1,
                    value = c(1, min(3, val)))
})

# ** DD plot text ######
output$DDplot_text <- renderUI({
  range1 <- input$DDplotRangeSlider[[1]]
  range2 <- input$DDplotRangeSlider[[2]]

  if (any(range1!=1, range2!=3, input$DDplotNumGroupsSlider!=3)) {
    HTML(paste(
      "Discrimination is here a difference between the difficulty recorded in the ",
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

  correct <- correct_answ()
  DDplot(correct, item.names = item_numbers(),
         k = input$DDplotNumGroupsSlider,
         l = input$DDplotRangeSlider[[1]], u = input$DDplotRangeSlider[[2]])
})

# ** Difficulty/Discrimination plot for report######
DDplot_Input_report<-reactive({
  correct <- correct_answ()
  if (input$customizeCheck) {
    DDplot(correct, item.names = item_numbers(),
           k = input$DDplotNumGroupsSlider_report,
           l = input$DDplotRangeSlider_report[[1]], u = input$DDplotRangeSlider_report[[2]])
  } else {
    DDplot(correct, item.names = item_numbers(),
           k = input$DDplotNumGroupsSlider,
           l = input$DDplotRangeSlider[[1]], u = input$DDplotRangeSlider[[2]])
  }
})

# ** Output Difficulty/Discrimination plot ######
output$DDplot <- renderPlot({
  DDplot_Input()
})

# ** DB Difficulty/Discrimination plot ######
output$DB_DDplot <- downloadHandler(
  filename =  function() {
    paste("fig_DDplot.png", sep = "")
  },
  content = function(file) {
    ggsave(file,
           plot = DDplot_Input() + theme(text = element_text(size = 10)),
           device = "png",
           height = 5, width = 14, dpi = 300)
  }
)

# ** Cronbach's alpha table ######
cronbachalpha_table_Input <- reactive({
  correct <- correct_answ()
  tab <- c(psych::alpha(correct)$total[1], psych::alpha(correct)$total[8])

  tab <- as.data.table(tab)
  colnames(tab) <- c("Estimate", "SD")

  tab
})

# ** Output Cronbach's alpha table ######
output$cronbachalpha_table <- renderTable({
  cronbachalpha_table_Input()
},
include.rownames = F,
include.colnames = T)

# ** Traditional item analysis table text ######
output$itemanalysis_table_text <- renderUI({
  range1 <- input$DDplotRangeSlider[[1]]
  range2 <- input$DDplotRangeSlider[[2]]
  num.groups <- input$DDplotNumGroupsSlider
  HTML(paste(
    " <b> Explanation: Difficulty </b> ",
    " - Difficulty of item is estimated as percent of students who answered correctly to that item. ",
    " <b> SD </b> ",
    " - standard deviation, ",
    " <b> RIT </b> ",
    " - Pearson correlation between item and total score, ",
    " <b> RIR </b> ",
    " - Pearson correlation between item and rest of items, ",
    " <b> ULI </b> ",
    " - Upper-Lower Index, ",
    " <b> Alpha Drop </b> ",
    " - Cronbach\'s alpha of test without given item, ",
    " <b> Customized Discrimination </b> ",
    " - difference between the difficulty recorded in the ", range1,
    ifelse(range1 >= 4, "-th", switch(range1, "1" = "-st", "2" = "-nd", "3" = "-rd")),
    " and ", range2,
    ifelse(range2 >= 4, "-th", switch(range2, "1" = "-st", "2" = "-nd", "3" = "-rd")),
    " group out of total number of ", num.groups, " groups. ",
    sep = ""
  ))
})

# ** Traditional item analysis table ######
itemanalysis_table_Input <- reactive({
  a <- test_answers()
  k <- test_key()
  correct <- correct_answ()

  num.groups <- input$DDplotNumGroupsSlider
  range1 <- input$DDplotRangeSlider[[1]]
  range2 <- input$DDplotRangeSlider[[2]]

  alphadrop <- psych::alpha(correct)$alpha.drop[, 1]
  tab <- item.exam(correct, discr = TRUE)[, 1:5]
  tab <- data.table(item_numbers(),
                    tab[, c(4, 1, 5, 2, 3)],
                    alphadrop)
  tab <- cbind(tab, gDiscrim(correct, k = num.groups, l = range1, u = range2))
  colnames(tab) <- c("Item", "Difficulty", "SD", "Discrimination ULI",
                     "Discrimination RIT", "Discrimination RIR", "Alpha Drop",
                     "Customized Discrimination")
  tab
})

# ** Output traditional item analysis table ######
output$itemanalysis_table <- renderTable({
  itemanalysis_table_Input()
},
include.rownames = FALSE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * DISTRACTORS ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Distractor text ######
output$distractor_text <- renderUI({
  txt1 <- paste ('Respondents are divided into ')
  txt2 <- paste ("<b>", input$gr, "</b>")
  txt3 <- paste ("groups by their total score. Subsequently, we display percentage
                 of students in each group who selected given answer (correct answer or distractor).
                 The correct answer should be more often selected by strong students than by students
                 with lower total score, i.e."
  )
  txt4 <- paste ("<b>",'solid line should be increasing.',"</b>")
  txt5 <- paste('The distractor should work in opposite direction, i.e. ')
  txt6 <- paste ("<b>",'dotted lines should be decreasing.',"<b>")
  HTML(paste(txt1, txt2, txt3, txt4, txt5, txt6))
})

# ** Distractors plot ######
distractor_plot_Input <- reactive({
  a <- test_answers()
  k <- test_key()
  i <- input$distractorSlider

  multiple.answers <- c(input$type_combinations_distractor == "Combinations")
  plotDistractorAnalysis(data = a, key = k, num.group = input$gr,
                         item = i,
                         item.name = item_names()[i],
                         multiple.answers = multiple.answers)
})

# ** Output distractors plot ######
output$distractor_plot <- renderPlot({
  distractor_plot_Input()
})

# ** DB distractors plot ######
output$DB_distractor_plot <- downloadHandler(
  filename =  function() {
    paste("fig_DistractorPlot_", item_names()[input$distractorSlider], ".png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = distractor_plot_Input() + theme(text = element_text(size = 10)),
           device = "png",
           height = 3, width = 9, dpi = 300)
  }
)
# ** Report distractors plot ######
report_distractor_plot <- reactive({
  a <- test_answers()
  colnames(a) <- item_names()
  k <- test_key()

  if (!input$customizeCheck) {
    multiple.answers_report <- c(input$type_combinations_distractor == "Combinations")
    num.group <- input$gr
  } else {
    multiple.answers_report <- c(input$type_combinations_distractor_report == "Combinations")
    num.group <- input$distractorGroupSlider
  }

  graflist <- list()

  for (i in 1:length(k)) {
    g <- plotDistractorAnalysis(data = a, key = k, num.group = num.group,
                                item = i,
                                item.name = item_names()[i],
                                multiple.answers = multiple.answers_report)
    g = g +
      ggtitle(paste("Distractor plot for item", item_numbers()[i])) +
      theme(text = element_text(size = 14))
    g = ggplotGrob(g)
    graflist[[i]] = g
  }
  graflist
})


# ** Distractor table with counts ######
distractor_table_counts_Input <- reactive({
  a <- test_answers()
  k <- test_key()
  num.group <- input$gr
  item <- input$distractorSlider

  DA <- DistractorAnalysis(a, k, num.groups = num.group)[[item]]
  df <- dcast(as.data.frame(DA), response ~ score.level, sum, margins = T, value.var = "Freq")
  colnames(df) <- c("Response", paste("Group", 1:num.group), "Total")
  levels(df$Response)[nrow(df)] <- "Total"
  df
})

# ** Output distractor table with counts ######
output$distractor_table_counts <- renderTable({
  distractor_table_counts_Input()
})

# ** Distractor table with proportions ######
distractor_table_proportions_Input <- reactive({
  a <- test_answers()
  k <- test_key()
  num.group <- input$gr
  item <- input$distractorSlider

  DA <- DistractorAnalysis(a, k, num.groups = num.group, p.table = TRUE)[[item]]
  df <- dcast(as.data.frame(DA), response ~ score.level, sum, value.var = "Freq")
  colnames(df) <- c("Response", paste("Group", 1:num.group))
  df
})

# ** Output distractor table with proportions ######
output$distractor_table_proportions <- renderTable({
  distractor_table_proportions_Input()
})

# ** Item response patterns barplot ######
distractor_barplot_item_response_patterns_Input <- reactive({
  a <- test_answers()
  k <- test_key()
  num.group <- 1
  item <- input$distractorSlider

  DA <- DistractorAnalysis(a, k, num.groups = num.group, p.table = TRUE)[[item]]
  df <- dcast(as.data.frame(DA), response ~ score.level, sum, value.var = "Freq")
  colnames(df) <- c("Response", "Proportion")

  ggplot(df, aes(x = Response, y = Proportion)) +
    geom_bar(stat = "identity") +
    xlab("Item response pattern") +
    ylab("Relative frequency") +
    scale_y_continuous(limits =  c(0, 1), expand = c(0, 0)) +
    theme_bw() +
    theme(axis.line  = element_line(colour = "black"),
          text = element_text(size = 14),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.title = element_blank(),
          legend.position = c(0, 1),
          legend.justification = c(0, 1),
          legend.background = element_blank(),
          legend.key = element_rect(colour = "white"),
          plot.title = element_text(face = "bold")) +
    ggtitle(item_names()[item])

})
# ** Output item response patterns barplot ######
output$distractor_barplot_item_response_patterns <- renderPlot({
  distractor_barplot_item_response_patterns_Input()
})

# ** DB item response patterns barplot ######
output$DB_distractor_barplot_item_response_patterns <- downloadHandler(
  filename =  function() {
    paste("fig_ItemResponsePatterns_", item_names()[input$distractorSlider], ".png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = distractor_barplot_item_response_patterns_Input() + theme(text = element_text(size = 10)),
           device = "png",
           height = 3, width = 9, dpi = 300)
  }
)

# ** Distractors histograms by group ######
distractor_histogram_Input <- reactive({
  a <- test_answers()
  k <- test_key()
  sc <- scored_test()

  df <- data.table(sc,
                   gr = cut(sc, quantile(sc, seq(0, 1, by = 1/input$gr), na.rm = T),
                            include.lowest = T))
  col <- c("darkred", "red", "orange", "gold", "green3")
  col <- switch(input$gr,
                "1" = col[4],
                "2" = col[4:5],
                "3" = col[c(2, 4:5)],
                "4" = col[2:5],
                "5" = col)
  ggplot(df, aes(x = sc)) +
    geom_histogram(aes(fill = gr), binwidth = 1, color = "black") +
    scale_fill_manual("", breaks = df$gr, values = col) +
    labs(x = "Total score",
         y = "Number of students") +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(0, max(table(sc)) + 0.01 * nrow(a))) +
    scale_x_continuous(limits = c(-0.5, ncol(a) + 0.5)) +
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.position = "none",
          axis.line  = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          text = element_text(size = 14),
          plot.title = element_text(face = "bold"))
})

# ** Output distractors histograms by group ######
output$distractor_histogram <- renderPlot({
  distractor_histogram_Input()
})

# ** DB distractors histograms by group ######
output$DB_distractor_histogram <- downloadHandler(
  filename =  function() {
    paste("fig_HistrogramByDistractorGroups.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = distractor_histogram_Input() + theme(text = element_text(size = 10)),
           device = "png",
           height = 3, width = 9, dpi = 300)
  }
)


# ** Distractor analysis table by group ######
distractor_table_total_score_by_group_Input <- reactive({
  sc <- scored_test()
  num.group <- input$gr

  score.level <- quantile(sc, seq(0, 1, by = 1/num.group), na.rm = T)
  tab <- table(cut(sc,
                   score.level,
                   include.lowest = T,
                   labels = score.level[-1]))
  tab <- t(data.frame(tab))
  tab <- matrix(round(as.numeric(tab), 2), nrow = 2)

  rownames(tab) <- c('Max points', 'Count')
  colnames(tab) <- paste('Group', 1:num.group)

  tab
})
# ** Output distractor analysis table by group ######
output$distractor_table_total_score_by_group <- renderTable({
  distractor_table_total_score_by_group_Input()
},
include.colnames = TRUE,
include.rownames = TRUE)
