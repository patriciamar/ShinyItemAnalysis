#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# VALIDITY ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * CORRELATION STRUCTURE ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Polychoric correlation matrix ######
corr_structure <- reactive({
  data <- correct_answ()

  corP <- polychoric(data)
  corP
})

# ** Correlation plot ######
corr_plot_Input <- reactive({
  corP <- corr_structure()
  corP <- corP$rho
  tlcex <- max(ifelse(dim(corP)[1] < 30, 1, 0.9 - (dim(corP)[1] - 30)*0.05), 0.5)

  numclust <- input$corr_plot_clust
  clustmethod <- input$corr_plot_clustmethod

  if (clustmethod == "none"){
    corrplot(corP, tl.cex = tlcex)
  } else {
    corrplot(corP, tl.cex = tlcex, order = "hclust", hclust.method = clustmethod, addrect = numclust)
  }
})


# ** Output correlation plot ######
output$corr_plot <- renderPlot({
  corr_plot_Input()
})

# ** DB correlation plot ######
output$DB_corr_plot <- downloadHandler(
  filename =  function() {
    paste("fig_CorrelationPlot.png", sep = "")
  },
  content = function(file) {
    # in corrplot this must be plotted completely again!
    corP <- corr_structure()
    corP <- corP$rho
    tlcex <- max(ifelse(dim(corP)[1] < 30, 1, 0.9 - (dim(corP)[1] - 30)*0.05), 0.5)

    numclust <- input$corr_plot_clust
    clustmethod <- input$corr_plot_clustmethod

    png(file, height = 800, width = 800, res = 300, pointsize = 300/72)
    if (clustmethod == "none"){
      corrplot(corP, tl.cex = tlcex)
    } else {
      corrplot(corP, tl.cex = tlcex, order = "hclust", hclust.method = clustmethod, addrect = numclust)
    }
    dev.off()
  }
)

# ** Scree plot ######
scree_plot_Input <- reactive({
  corP <- corr_structure()
  ev <- eigen(corP$rho)$values
  df <- data.table(pos = 1:length(ev), ev)

  ggplot(data = df, aes(x = pos, y = ev)) +
    geom_point() +
    geom_line() +
    xlab("Component number") + ylab("Eigen value") +
    scale_x_continuous(breaks = 1:length(ev), expand = c(0.01, 0.01)) +
    theme_shiny
})

# ** Output scree plot ######
output$scree_plot <- renderPlot({
  scree_plot_Input()
})

# ** DB scree plot ######
output$DB_scree_plot <- downloadHandler(
  filename =  function() {
    paste("fig_ScreePlot.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = scree_plot_Input() + theme(text = element_text(size = 10)),
           device = "png",
           height = 4, width = 8, dpi = 300)
  }
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * PREDICTIVE VALIDITY ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Validity boxplot ######
validity_plot_boxplot_Input <- reactive({
  ts <- scored_test()
  cv <- unlist(criterion_variable())

  df <- data.table(ts, cv)
  df <- df[complete.cases(df), ]

  g <- ggplot(df, aes(y = ts, x = as.factor(cv), fill = as.factor(cv))) +
    geom_boxplot() +
    geom_jitter(shape = 16, position = position_jitter(0.2)) +
    scale_fill_brewer(palette = "Blues") +
    xlab("Criterion group") +
    ylab("Total score") +
    coord_flip() +
    theme_shiny
  g
})

# ** Validity scatterplot ######
validity_plot_scatter_Input <- reactive({
  ts <- scored_test()
  cv <- unlist(criterion_variable())

  size <- as.factor(cv)
  levels(size) <- table(cv)
  size <- as.numeric(as.character(size))

  df <- data.table(ts, cv, size)
  df <- df[complete.cases(df), ]

  g <- ggplot(df, aes(y = cv, x = ts, size = size)) +
    geom_point(color = "black") +
    geom_smooth(method = lm,
                se = FALSE,
                color = "red",
                show.legend = FALSE) +
    xlab("Total score") +
    ylab("Criterion variable") +
    theme_shiny +
    theme(legend.justification = c(1, 0),
          legend.position = c(1, 0))
  g
})

# ** Validity descriptive plot ######
validity_plot_Input <- reactive({
  cv <- criterion_variable()

  ## this is fixed value to recognize discrete variable
  k <- 6
  if (length(unique(cv)) <= length(cv)/k){
    g <- validity_plot_boxplot_Input()
  } else {
    g <- validity_plot_scatter_Input()
  }
  g
})

# ** Output validity descriptive plot ######
output$validity_plot <- renderPlot({
  validity_plot_Input()
})

# ** DB validity descriptive plot ######
output$DB_validity_plot <- downloadHandler(
  filename =  function() {
    cv <- criterion_variable()
    k <- 6
    type <- ifelse(length(unique(cv)) <= length(cv)/k, "boxplot", "scatterplot")
    paste("fig_CriterionVariable_", type, ".png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = validity_plot_Input() + theme(text = element_text(size = 10)),
           device = "png",
           height = 4, width = 8, dpi = 300)
  }
)

# ** Validity correlation table ######
validity_table_Input <- reactive({
  ts <- scored_test()
  cv <- criterion_variable()

  ct <- cor.test(ts, cv, method = "spearman", exact = F)
  tab <- c(round(ct$estimate, 2), round(ct$statistic, 2), round(ct$p.value, 3))
  names(tab) <- c(HTML("&rho;"), "S-value", "p-value")

  if (tab[3] == 0.00){
    tab[3] <- "<0.01"
  }

  tab
})

# * Output validity correlation table ######
output$validity_table <- renderTable({
  validity_table_Input()
},
include.rownames = TRUE,
include.colnames = FALSE,
sanitize.text.function = function(x) x)

# ** Interpretation ######
output$validity_table_interpretation <- renderUI({
  tab <- validity_table_Input()
  p.val <- tab["p-value"]
  rho <- tab[1]

  txt1 <- paste ("<b>", "Interpretation:","</b>")
  txt2 <- ifelse(rho > 0, "positively", "negatively")
  txt3 <- ifelse(p.val < 0.05,
                 paste("The p-value is less than 0.05, thus we reject the null hypotheses.
                       Total score and criterion variable are", txt2, "correlated."),
                 "The p-value is larger than 0.05, thus we don't reject the null hypotheses.
                 We cannot conclude that a significant correlation between total score
                 and criterion variable exists.")
  HTML(paste(txt1, txt3))
})

# ** Validity distractor text ######
output$validity_distractor_text <- renderUI({
  cv <- criterion_variable()

  ## this is fixed value to recognize discrete variable
  k <- 6
  if (length(unique(cv)) <= length(cv)/k){
    num.group <- length(levels(as.factor(cv)))
  } else {
    num.group <- input$validity_group
  }

  txt1 <- paste ('Respondents are divided into ')
  txt2 <- ifelse((length(unique(cv)) <= length(cv)/k),
                 paste("<b>", num.group, "</b> groups as it seems that criterion variable is discrete. "),
                 paste("<b>", num.group, "</b> groups by their criterion variable. "))
  txt3 <- paste ("Subsequently, we display percentage
                 of students in each group who selected given answer (correct answer or distractor).
                 The correct answer should be more often selected by strong students than by students
                 with lower total score, i.e.")
  txt4 <- paste ("<b>",'solid line should be increasing.',"</b>")
  txt5 <- paste('The distractor should work in opposite direction, i.e. ')
  txt6 <- paste ("<b>",'dotted lines should be decreasing.',"<b>")
  HTML(paste(txt1, txt2, txt3, txt4, txt5, txt6))
})

# ** Validity distractors plot ######
validity_distractor_plot_Input <- reactive({
  a <- test_answers()
  k <- test_key()
  i <- input$validitydistractorSlider
  cv <- criterion_variable()
  num.group <- input$validity_group

  multiple.answers <- c(input$type_validity_combinations_distractor == "Combinations")
  plotDistractorAnalysis(data = a, key = k, num.group = num.group,
                         item = i,
                         item.name = item_names()[i],
                         multiple.answers = multiple.answers,
                         matching = cv)
})

# ** Output validity distractors plot ######
output$validity_distractor_plot <- renderPlot({
  validity_distractor_plot_Input()
})

# ** DB validity distractors plot ######
output$DB_validity_distractor_plot <- downloadHandler(
  filename =  function() {
    paste("fig_DistractorsValidityPlot.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = validity_distractor_plot_Input() + theme(text = element_text(size = 10)),
           device = "png",
           height = 4, width = 8, dpi = 300)
  }
)

# ** Validity correlation table for items ######
validity_table_item_Input <- reactive({
  correct <- correct_answ()
  cv <- criterion_variable()
  i <- input$validitydistractorSlider

  ct <- cor.test(unlist(correct[, i, with = F]), cv, method = "spearman", exact = F)
  tab <- c(round(ct$estimate, 2), round(ct$statistic, 2), round(ct$p.value, 3))
  names(tab) <- c(HTML("&rho;"), "S-value", "p-value")
  if (tab[3] == 0.00){
    tab[3] <- "<0.01"
  }

  tab
})

# ** Output validity correlation table for items ######
output$validity_table_item <- renderTable({
  validity_table_item_Input()
},
include.rownames = TRUE,
include.colnames = FALSE,
sanitize.text.function = function(x) x)

# ** Interpretation ####
output$validity_table_item_interpretation <- renderUI({
  tab <- validity_table_item_Input()
  p.val <- tab["p-value"]
  rho <- tab[1]
  i <- input$validitydistractorSlider

  txt1 <- paste ("<b>", "Interpretation:","</b>")
  txt2 <- ifelse(rho > 0, "positively", "negatively")
  txt3 <- ifelse(p.val < 0.05,
                 paste("The p-value is less than 0.05, thus we reject the null hypotheses.
                         Scored item", i, "and criterion variable are", txt2, "correlated."),
                 paste("The p-value is larger than 0.05, thus we don't reject the null hypotheses.
                   We cannot conclude that a significant correlation between scored item", i,
                       "and criterion variable exists."))
  HTML(paste(txt1, txt3))
})
