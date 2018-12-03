#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# VALIDITY ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * CORRELATION STRUCTURE ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Polychoric correlation matrix ######
corr_structure <- reactive({
  data <- binary()

  # calculate correlations depending on selected method
  if (input$type_of_corr == 'spearman') {
    corP <- cor(data, method = 'spearman')
  } else if (input$type_of_corr == 'pearson') {
    corP <- cor(data, method = 'pearson')
  } else if (input$type_of_corr == 'polychoric') {
    corP <- polychoric(data)
    corP <- corP$rho
  }
  corP
})

# ** Correlation plot ######
corr_plot_Input <- reactive({
  corP <- corr_structure()

  tlcex <- max(ifelse(dim(corP)[1] < 30, 1, 0.9 - (dim(corP)[1] - 30)*0.05), 0.5)

  numclust <- input$corr_plot_clust
  clustmethod <- input$corr_plot_clustmethod

  # option to display correlation values
  if(input$show_corr %% 2 == 1 ) {
    updateActionButton(session, "show_corr", label = "Hide correlation values")
	  if (clustmethod == "none"){
	    corrplot(corP, tl.cex = tlcex, tl.pos = 'lt', method = 'number',
	             number.cex = 0.7, col = 'black', cl.pos = 'n')
    } else {
      corrplot(corP, tl.cex = tlcex, order = "hclust", hclust.method = clustmethod,
               addrect = numclust, tl.pos = 'lt', method = 'number',
               number.cex  = 0.7, col = 'black', cl.pos = 'n')
    }
   } else {
      updateActionButton(session,"show_corr", label = "Display correlation values")
    if (clustmethod == "none"){
	    corrplot(corP, tl.cex = tlcex, tl.pos = 'lt')
	  } else {
	    corrplot(corP, tl.cex = tlcex, order = "hclust", hclust.method = clustmethod,
	             addrect = numclust, tl.pos = 'lt')
	  }
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

    tlcex <- max(ifelse(dim(corP)[1] < 30, 1, 0.9 - (dim(corP)[1] - 30)*0.05), 0.5)

    numclust <- input$corr_plot_clust
    clustmethod <- input$corr_plot_clustmethod

    png(file, height = setting_figures$height, width = setting_figures$height,
        units = "in",
        res = setting_figures$dpi,
        pointsize = setting_figures$dpi/72)
    if (clustmethod == "none"){
      corrplot(corP, tl.cex = tlcex)
    } else {
      corrplot(corP, tl.cex = tlcex, order = "hclust", hclust.method = clustmethod,
               addrect = numclust)
    }
    dev.off()
  }
)


output$corr_matrix <- downloadHandler(
	filename = function() {
		paste("Correlation_matrix", ".csv", sep = "")
	},
	content = function(file) {
	  corP <- corr_structure()
	  write.csv(corP, file)
	})

# ** Dendrogram ######
dendrogram_plot_Input <- reactive({
  corP <- corr_structure()
  dist <- as.dist(1 - corP)

  clustmethod <- input$corr_plot_clustmethod
  numclust <- input$corr_plot_clust

  hc <- hclust(dist, method = clustmethod)

  if (numclust == 1){
    order <- hc$order
    label <- hc$labels[hc$order]
    times <- length(label)
  } else {
    plot(hc)
    rhc <- rect.hclust(hc, k = numclust)
    order <- unlist(rhc)
    label <- names(order)
    times <- sapply(rhc, length)
  }

  df <- data.frame(label = label,
                   num = order,
                   cluster = rep(paste("Cluster", 1:numclust), times))
  dendr <- dendro_data(hc, type = "rectangle")
  dfd <- merge(dendr$labels, df, by = "label")

  ggplot() +
    geom_segment(data = segment(dendr),
                 aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_text(data = dfd,
              aes(x = x, y = y, label = label, hjust = 0, color = cluster)) +
    coord_flip() + scale_y_reverse(expand = c(0.2, 0)) +
    ylab("Height") +
    theme_app() +
    theme(axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank())
})

# ** Output scree plot ######
output$dendrogram_plot <- renderPlot({
  dendrogram_plot_Input()
})

# ** DB scree plot ######
output$DB_dendrogram <- downloadHandler(
  filename =  function() {
    paste("fig_Dendrogram.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = dendrogram_plot_Input() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * FACTOR ANALYSIS ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Scree plot ######
scree_plot_Input <- reactive({
  corP <- corr_structure()

  ev <- eigen(corP)$values

  df <- data.table(pos = 1:length(ev), ev)

  ggplot(data = df, aes(x = pos, y = ev)) +
    geom_point() +
    geom_line() +
    xlab("Component number") + ylab("Eigen value") +
    scale_x_continuous(breaks = 1:length(ev), expand = c(0.01, 0.01)) +
    theme_app()
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
    ggsave(file, plot = scree_plot_Input() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * PREDICTIVE VALIDITY ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Validity boxplot ######
validity_plot_boxplot_Input <- reactive({
  ts <- total_score()
  cv <- unlist(criterion())

  df <- data.table(ts, cv)
  df <- df[complete.cases(df), ]

  g <- ggplot(df, aes(y = ts, x = as.factor(cv), fill = as.factor(cv))) +
    geom_boxplot() +
    geom_jitter(shape = 16, position = position_jitter(0.2)) +
    scale_fill_brewer(palette = "Blues") +
    xlab("Criterion group") +
    ylab("Total score") +
    coord_flip() +
    theme_app()
  g
})

# ** Validity scatterplot ######
validity_plot_scatter_Input <- reactive({
  ts <- total_score()
  cv <- unlist(criterion())

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
    theme_app() +
    theme(legend.justification = c(0.99, 0.01),
          legend.position = c(0.99, 0.01))
  g
})

# ** Validity descriptive plot ######
validity_plot_Input <- reactive({
  cv <- criterion()

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
    cv <- criterion()
    k <- 6
    type <- ifelse(length(unique(cv)) <= length(cv)/k, "boxplot", "scatterplot")
    paste("fig_CriterionVariable_", type, ".png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = validity_plot_Input() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# ** Validity correlation table ######
validity_table_Input <- reactive({
  ts <- total_score()
  cv <- criterion()

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
  cv <- criterion()

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
                 of respondents in each group who selected given answer (correct answer or distractor).
                 The correct answer should be more often selected by respondents with higher values of criterion variable
                 than by those with lower values, i.e.")
  txt4 <- paste ("<b>",'solid line should be increasing.',"</b>")
  txt5 <- paste('The distractor should work in opposite direction, i.e. ')
  txt6 <- paste ("<b>",'dotted lines should be decreasing.',"<b>")
  HTML(paste(txt1, txt2, txt3, txt4, txt5, txt6))
})

# ** Validity distractors plot ######
validity_distractor_plot_Input <- reactive({
  a <- nominal()
  k <- key()
  i <- input$validitydistractorSlider
  cv <- criterion()
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
    ggsave(file, plot = validity_distractor_plot_Input() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# ** Validity correlation table for items ######
validity_table_item_Input <- reactive({
  correct <- binary()
  cv <- criterion()
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
