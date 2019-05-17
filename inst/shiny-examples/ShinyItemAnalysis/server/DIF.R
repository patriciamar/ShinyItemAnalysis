#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# DIF/FAIRNESS ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * TOTAL SCORES ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Summary of Total Scores for Groups ######
resultsgroupInput <- reactive({
  sc_one  <- total_score()[group() == 1]
  sc_zero <- total_score()[group() == 0]
  tab <- data.frame(rbind(round(c(length(sc_one),
                                  min(sc_zero, na.rm = T),
                                  max(sc_zero, na.rm = T),
                                  mean(sc_zero, na.rm = T),
                                  median(sc_zero, na.rm = T),
                                  sd(sc_zero, na.rm = T),
                                  skewness(sc_zero, na.rm = T),
                                  kurtosis(sc_zero, na.rm = T)), 2),
                          round(c(length(sc_zero),
                                  min(sc_one, na.rm = T),
                                  max(sc_one, na.rm = T),
                                  mean(sc_one, na.rm = T),
                                  median(sc_one, na.rm = T),
                                  sd(sc_one, na.rm = T),
                                  skewness(sc_one, na.rm = T),
                                  kurtosis(sc_one, na.rm = T)), 2)))
  colnames(tab) <- c("N", "Min", "Max", "Mean", "Median", "SD", "Skewness", "Kurtosis")
  tab$N <- as.integer(tab$N)
  rownames(tab) <- c("Reference group (0)", "Focal group (1)")
  tab
})

output$resultsgroup <- renderTable({
  resultsgroupInput()
},
digits = 2,
include.rownames = T,
include.colnames = T)

# ** Histogram of total score for group = 1 (focal) ######
histbyscoregroup1Input <- reactive({
  data <- binary()
  sc  <- total_score()[group() == 1]
  bin <- as.numeric(input$inSlider2group)
  max.val <- max(prop.table(table(total_score()[group() == 0])),
                 prop.table(table(total_score()[group() == 1])))

  df <- data.table(score = sc,
                   gr = cut(sc,
                            breaks = unique(c(0, bin - 1, bin, ncol(data))),
                            include.lowest = T))

  if (bin < min(sc, na.rm = T)){
    col <- "blue"
  } else {
    if (bin == min(sc, na.rm = T)){
      col <- c("grey", "blue")
    } else {
      col <- c("red", "grey", "blue")
    }
  }

  g <- ggplot(df, aes(x = score)) +
    geom_histogram(aes(fill = gr, y = ..count../sum(..count..)), binwidth = 1, color = "black") +
    scale_fill_manual("", breaks = df$gr, values = col) +
    labs(x = "Total score",
         y = "Proportion of respondents") +
    scale_x_continuous(limits = c(-0.5, ncol(data) + 0.5)) +
    scale_y_continuous(limits = c(0, max.val)) +
    ggtitle("Focal group") +
    theme_app()
  g
})

output$histbyscoregroup1 <- renderPlotly ({
  sc <- total_score()[group() == 1]
  bin <- as.numeric(input$inSlider2group)
  data <- binary()

  if (min(sc, na.rm = TRUE) <= bin & bin <= max(sc, na.rm = TRUE)){
    breaks <- unique(c(min(sc, na.rm = TRUE) - 1, bin - 1, bin, max(sc, na.rm = TRUE)))
  } else {
    breaks <- c(0, ncol(data))
  }

  df <- data.table(score = sc,
                   gr = cut(sc,
                            breaks = breaks,
                            include.lowest = T))

  g <- histbyscoregroup1Input()
  p <- ggplotly(g)
  k <- length(levels(df$gr))
  m <- length(p$x$data[[1]]$text)
  ints <- breaks


  for(i in 1:k){
    t <- subset(df, df$gr == levels(df$gr)[i])
    t <- t[order(t$score)]

    t <- as.data.frame(table(t$score))
    lbnd <- ints[i] + 1
    hbnd <- ints[i + 1] + 1

    c <- 1
    for (j in lbnd:hbnd) {
      text <- strsplit(p$x$data[[i]]$text[j], "<br />")[[1]][1]
      text <- sub("/", "", text)
      text <- sub("countsum\\(count\\)", "Proportion", text)
      p$x$data[[i]]$text[j] <- paste(text, "<br />",
                                     "Number of respodents:",
                                     ifelse(c <= nrow(t) &
                                              t$Var1[c] %in% p$x$data[[i]]$x[lbnd:hbnd] &
                                              t$Var1[c] == p$x$data[[i]]$x[j], t$Freq[c], 0),
                                     "<br /> Score:", p$x$data[[i]]$x[j])
      c <- ifelse(t$Var1[c] != p$x$data[[i]]$x[j], c, c + 1)
    }
  }

  p %>% plotly::config(displayModeBar = F)
})

output$DP_histbyscoregroup1 <- downloadHandler(
  filename =  function() {
    paste("fig_HistogramForFocalGroup.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = histbyscoregroup1Input() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# ** Histogram of total score for group = 0 (reference) ######
histbyscoregroup0Input <- reactive ({
  data <- binary()
  sc  <- total_score()[group() == 0]
  bin <- as.numeric(input$inSlider2group)
  max.val <- max(prop.table(table(total_score()[group() == 0])),
                 prop.table(table(total_score()[group() == 1])))

  df <- data.table(score = sc,
                   gr = cut(sc,
                            breaks = unique(c(0, bin - 1, bin, ncol(data))),
                            include.lowest = T))

  if (bin < min(sc, na.rm = TRUE)){
    col <- "blue"
  } else {
    if (bin == min(sc, na.rm = TRUE)){
      col <- c("grey", "blue")
    } else {
      col <- c("red", "grey", "blue")
    }
  }

  g <- ggplot(df, aes(x = score)) +
    geom_histogram(aes(fill = gr, y = ..count../sum(..count..)), binwidth = 1, color = "black") +
    scale_fill_manual("", breaks = df$gr, values = col) +
    labs(x = "Total score",
         y = "Proportion of respondents") +
    scale_x_continuous(limits = c(-0.5, ncol(data) + 0.5)) +
    scale_y_continuous(limits = c(0, max.val)) +
    ggtitle("Reference group") +
    theme_app()
  g
})

output$histbyscoregroup0 <- renderPlotly ({
  sc <- total_score()[group() == 0]
  bin <- as.numeric(input$inSlider2group)
  data <- binary()

  if (min(sc, na.rm = TRUE) <= bin & bin <= max(sc, na.rm = TRUE)){
    breaks <- unique(c(min(sc, na.rm = TRUE) - 1, bin - 1, bin, max(sc, na.rm = TRUE)))
  } else {
    breaks <- c(0, ncol(data))
  }

  df <- data.table(score = sc,
                   gr = cut(sc,
                            breaks = breaks,
                            include.lowest = T))

  g <- histbyscoregroup0Input()
  p <- ggplotly(g)
  k <- length(levels(df$gr))
  m <- length(p$x$data[[1]]$text)
  ints <- breaks


  for(i in 1:k){
    t <- subset(df, df$gr == levels(df$gr)[i])
    t <- t[order(t$score)]

    t <- as.data.frame(table(t$score))
    lbnd <- ints[i] + 1
    hbnd <- ints[i + 1] + 1

    c <- 1
    for (j in lbnd:hbnd) {
      text <- strsplit(p$x$data[[i]]$text[j], "<br />")[[1]][1]
      text <- sub("/", "", text)
      text <- sub("countsum\\(count\\)", "Proportion", text)
      p$x$data[[i]]$text[j] <- paste(text, "<br />",
                                     "Number of respodents:",
                                     ifelse(c <= nrow(t) &
                                              t$Var1[c] %in% p$x$data[[i]]$x[lbnd:hbnd] &
                                              t$Var1[c] == p$x$data[[i]]$x[j], t$Freq[c], 0),
                                     "<br /> Score:", p$x$data[[i]]$x[j])
      c <- ifelse(t$Var1[c] != p$x$data[[i]]$x[j], c, c + 1)
    }
  }

  p %>% plotly::config(displayModeBar = F)
})

output$DP_histbyscoregroup0 <- downloadHandler(
  filename =  function() {
    paste("fig_HistogramForRefGroup.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = histbyscoregroup0Input() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# ** t-test to compare total scores ######
DIF_scores_ttest_Input <- reactive ({
  sc0 <- total_score()[group() == 0]
  sc1 <- total_score()[group() == 1]

  ttest <- t.test(sc0, sc1)

  tab <- c(paste0(sprintf("%.2f", mean(sc0, na.rm = T) - mean(sc1, na.rm = T)), " (",
                  sprintf("%.2f", ttest$conf.int[1]), ", ",
                  sprintf("%.2f", ttest$conf.int[2]), ")"),
           sprintf("%.2f", ttest$statistic),
           sprintf("%.2f", ttest$parameter),
           ifelse(ttest$p.value < 0.001, "< 0.001", sprintf("%.3f", ttest$p.value)))
  tab <- t(as.data.frame(tab))
  colnames(tab) <- c("Diff. (CI)", "t-value", "df", "p-value")
  tab
})

output$DIF_scores_ttest <- renderTable ({
  DIF_scores_ttest_Input()
})

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * DELTA PLOT ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

deltaGpurn <- reactive ({
  data <- data.table(binary(), group = group())
  switch(input$type_threshold,
         "Fixed" = deltaPlot(data, group = "group",
                             focal.name = 1,
                             thr = 1.5,
                             purify = input$puri_DP,
                             purType = input$puri_DP_type),
         "Normal"= deltaPlot(data, group = "group",
                             focal.name = 1,
                             thr = "norm",
                             purify = input$puri_DP,
                             purType = input$puri_DP_type)
  )
})

deltaGpurn_report <- reactive({
  data <- data.table(binary(), group = group())
  if (!input$customizeCheck){
    type_threshold_report = input$type_threshold
    purify_report = input$puri_DP
    purType_report = input$puri_DP_type
  } else {
    type_threshold_report = input$type_threshold_report
    purify_report = input$puri_DP_report
    purType_report = input$puri_DP_type_report
  }

  switch(type_threshold_report,
         "Fixed" = deltaPlot(data, group = "group",
                             focal.name = 1,
                             thr = 1.5,
                             purify = purify_report,
                             purType = purType_report),
         "Normal"= deltaPlot(data, group = "group",
                             focal.name = 1,
                             thr = "norm",
                             purify = purify_report,
                             purType = purType_report)
  )
})

# * Delta plot ######
deltaplotInput <- reactive({
  dp <- deltaGpurn()
  df <- data.frame(dp$Deltas)
  df$nam <- item_numbers()

  par <- dp$axis.par
  thr <- dp$thr

  if (length(par) > 2){
    par <- par[length(par)/2, ]
  }

  if (length(thr) > 1){
    thr <- thr[length(thr)]
  }

  p <- ggplot(df,
              aes(x = X1, y = X2, label = nam)) +
    geom_point() +
    geom_text(hjust = 0, nudge_x = 0.05, size = 6) +
    geom_abline(intercept = par[1], slope = par[2],
                size = 1) +
    geom_abline(intercept = par[1] + thr * sqrt(par[2]^2 + 1),
                slope = par[2],
                color = "red",
                linetype = "dashed",
                size = 1) +
    geom_abline(intercept = par[1] - thr * sqrt(par[2]^2 + 1),
                slope = par[2],
                color = "red",
                linetype = "dashed",
                size = 1) +
    labs(x = "Reference group",
         y = "Focal group") +
    xlim(min(dp$Deltas, na.rm = T) - 0.5, max(dp$Deltas, na.rm = T) + 0.5) +
    ylim(min(dp$Deltas, na.rm = T) - 0.5, max(dp$Deltas, na.rm = T) + 0.5) +
    theme_app()
  if (is.numeric(dp$DIFitems)){
    df2 <- df[dp$DIFitems, ]
    p <- p + geom_point(data = df2,
                        aes(x = X1, y = X2, label = nam),
                        size = 8, color = "black", shape = 1)
  }
  p <- p + ggtitle("Delta plot")
  p
})

deltaplotInput_report<-reactive({
  dp <- deltaGpurn_report()
  df <- data.frame(dp$Deltas)
  df$nam <- item_numbers()

  par <- dp$axis.par
  thr <- dp$thr

  if (length(par) > 2){
    par <- par[length(par)/2, ]
  }

  if (length(thr) > 1){
    thr <- thr[length(thr)]
  }

  p <- ggplot(df,
              aes(x = X1, y = X2, label = nam)) +
    geom_point() +
    geom_text(hjust = 0, nudge_x = 0.05) +
    geom_abline(intercept = par[1], slope = par[2],
                size = 1) +
    geom_abline(intercept = par[1] + thr * sqrt(par[2]^2 + 1),
                slope = par[2],
                color = "red",
                linetype = "dashed",
                size = 1) +
    geom_abline(intercept = par[1] - thr * sqrt(par[2]^2 + 1),
                slope = par[2],
                color = "red",
                linetype = "dashed",
                size = 1) +
    labs(x = "Reference group",
         y = "Focal group") +
    xlim(min(dp$Deltas, na.rm = T) - 0.5, max(dp$Deltas, na.rm = T) + 0.5) +
    ylim(min(dp$Deltas, na.rm = T) - 0.5, max(dp$Deltas, na.rm = T) + 0.5) +
    theme_app()

  if (is.numeric(dp$DIFitems)){
    df2 <- df[dp$DIFitems, ]
    p <- p + geom_point(data = df2,
                        aes(x = X1, y = X2, label = nam),
                        size = 6, color = "black", shape = 1)
  }
  p <- p + ggtitle("Delta plot")
  p
})

output$deltaplot <- renderPlot({
  deltaplotInput()
})

output$DP_deltaplot <- downloadHandler(
  filename =  function() {
    paste("fig_DeltaPlot.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = deltaplotInput() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# Output
output$dp_text_normal <- renderPrint({
  deltaGpurn()
})

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * MANTEL-HAENSZEL ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Model for print ######
model_DIF_MH <- reactive({
  group <- unlist(group())
  data <- data.frame(binary())

  mod <- difMH(Data = data, group = group, focal.name = 1,
               p.adjust.method = input$correction_method_MZ_print,
               purify = input$puri_MH)
  mod
})

# ** Model for tables ######
model_DIF_MH_tables <- reactive({
  group <- unlist(group())
  data <- data.frame(binary())

  mod <- difMH(Data = data, group = group, focal.name = 1)
  # no need for correction, estimates of OR are the same
  mod
})

# ** Output print ######
output$print_DIF_MH <- renderPrint({
  print(model_DIF_MH())
})


# ** Contingency tables ######
table_DIF_MH <- reactive({
  group <- unlist(group())
  data <- data.frame(binary())

  total <- apply(data, 1, sum)

  df <- data.frame(data[, input$difMHSlider_item], group)
  colnames(df) <- c("Answer", "Group")
  df$Answer <- relevel(factor(df$Answer, labels = c("Incorrect", "Correct")),
                       "Correct")
  df$Group <- factor(df$Group, labels = c("Reference group", "Focal group"))


  df <- df[total == input$difMHSlider_score, ]

  tab <- dcast(data.frame(xtabs(~ Group + Answer, data = df)),
               Group ~ Answer, value.var = "Freq", margins = T,
               fun = sum)

  colnames(tab)[4] <- tab$Group[3] <- levels(tab$Group)[3]  <- "Total"
  colnames(tab)[1] <- ""
  tab

})

# ** Contingency tables output ######
output$table_DIF_MH <- renderTable({
  table_DIF_MH()
})

# ** OR calculation ######
output$ORcalculation <- renderUI ({
  a <- table_DIF_MH()[1, 2]
  b <- table_DIF_MH()[1, 3]
  c <- table_DIF_MH()[2, 2]
  d <- table_DIF_MH()[2, 3]
  OR <- round((a*d)/(b*c), 2)

  alphaMH <- round(model_DIF_MH_tables()$alphaMH[input$difMHSlider_item], 2)

  txt <- ifelse((b * c == 0)|(a * d == 0), "Odds ratio cannot be calculated!",
                paste("For respondent who reached total score of", input$difMHSlider_score,
                      "the odds of answering item", item_numbers()[input$difMHSlider_item],
                      "correctly is",
                      ifelse(OR == 1, "is the same for both groups. ",
                             ifelse(OR > 1,
                                    paste(OR, "times higher in the reference group than in the focal group."),
                                    paste(OR, "times lower in the reference group than in the focal group.")))))

  txtMH <- paste("Mantel-Haenszel estimate of odds ratio accounting for all levels of total score is equal to",
                 alphaMH, ". The odds of answering item", item_numbers()[input$difMHSlider_item],
                 "correctly is",
                 ifelse(alphaMH == 1, "is the same for both groups. ",
                        ifelse(alphaMH > 1,
                               paste(alphaMH, "times higher in the reference group than in the focal group."),
                               paste(alphaMH, "times lower in the reference group than in the focal group."))))
  withMathJax(
    paste(sprintf(
      paste('$$\\mathrm{OR} = \\frac{%d \\cdot %d}{%d \\cdot %d} = %.2f \\\\$$', txt),
      a, d, b, c, OR
    ),
    sprintf(
      paste('$$\\mathrm{OR}_{MH} = %.2f \\\\$$', txtMH),
      alphaMH
    )
    )
  )
})

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * LOGISTIC ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Model for plot ######
model_DIF_logistic_plot <- reactive({
  group <- unlist(group())
  data <- data.frame(binary())

  mod <- difLogistic(Data = data, group = group, focal.name = 1,
                     type = input$type_plot_DIF_logistic,
                     p.adjust.method = input$correction_method_logItems,
                     purify = input$puri_LR_plot)
  mod
})

# ** Model for print ######
model_DIF_logistic_print <- reactive({
  group <- unlist(group())
  data <- data.frame(binary())

  mod <- difLogistic(Data = data, group = group, focal.name = 1,
                     type = input$type_print_DIF_logistic,
                     p.adjust.method = input$correction_method_logSummary,
                     purify = input$puri_LR)
  mod
})

model_DIF_logistic_print_report <- reactive({
  group <- unlist(group())
  data <- data.frame(binary())

  if (!input$customizeCheck) {
    type_report = input$type_print_DIF_logistic
    p.adjust.method_report = input$correction_method_logSummary
    purify_report = input$puri_LR
  } else {
    type_report = input$type_print_DIF_logistic_report
    p.adjust.method_report = input$correction_method_log_report
    purify_report = input$puri_LR_report
  }

  mod <- difLogistic(Data = data, group = group, focal.name = 1,
                     type = type_report,
                     p.adjust.method = p.adjust.method_report,
                     purify = purify_report)
  mod
})


# ** Output print ######
output$print_DIF_logistic <- renderPrint({
  print(model_DIF_logistic_print())
})

# ** Plot ######
plot_DIF_logisticInput <- reactive({
  group <- unlist(group())
  data <- data.frame(binary())
  item <- input$diflogSlider

  g <- plotDIFLogistic(data, group,
                       type = input$type_plot_DIF_logistic,
                       item = item,
                       item.name = item_names()[item],
                       IRT = F,
                       p.adjust.method = input$correction_method_logItems,
                       purify = input$puri_LR_plot)
  g
})

output$plot_DIF_logistic <- renderPlot({
  plot_DIF_logisticInput()
})

output$DP_plot_DIF_logistic <- downloadHandler(
  filename =  function() {
    paste("fig_DifLogisticRegression_", item_names()[input$diflogSlider], ".png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = plot_DIF_logisticInput() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# ** Table with coefficients ######
output$tab_coef_DIF_logistic <- renderTable({

  fit <- model_DIF_logistic_plot()
  i <- input$diflogSlider

  tab_coef <- fit$logitPar[i, ]
  tab_sd <- fit$logitSe[i, ]

  tab <- data.frame(tab_coef, tab_sd)
  rownames(tab) <- c('%%mathit{b}_0%%', '%%mathit{b}_1%%', '%%mathit{b}_2%%', '%%mathit{b}_3%%')
  colnames(tab) <- c("Estimate", "SD")

  tab
},
include.rownames = T,
include.colnames = T)

#output$ui_tab_coef_DIF_logistic <- renderUI({
#
#	tagList(
#
#		withMathJax(),
#		withMathJax(tableOutput("tab_coef_DIF_logistic"))
#
#	)
#
#})

DIF_logistic_plotReport <- reactive({
  group <- unlist(group())
  data <- data.frame(binary())

  if (!input$customizeCheck) {
    type_report = input$type_print_DIF_logistic
    p.adjust.method_report = input$correction_method_logItems
    purify_report = input$puri_LR
  } else {
    type_report = input$type_print_DIF_logistic_report
    p.adjust.method_report = input$correction_method_log_report
    purify_report = input$puri_LR_report
  }

  mod <- difLogistic(Data = data, group = group, focal.name = 1,
                     type = type_report,
                     p.adjust.method = p.adjust.method_report,
                     purify = purify_report)
  # mod$DIFitems
  graflist = list()
  if (mod$DIFitems[1] != "No DIF item detected") {
    for (i in 1:length(mod$DIFitems)) {
      g <- plotDIFLogistic(data, group,
                           type = type_report,
                           item =  mod$DIFitems[i],
                           item.name = item_names()[mod$DIFitems[i]],
                           IRT = F,
                           p.adjust.method = p.adjust.method_report,
                           purify = purify_report)
      g = g + ggtitle(paste0("DIF logistic plot for ", item_names()[mod$DIFitems[i]])) +
        theme(text = element_text(size = 12),
              plot.title = element_text(size = 12, face = "bold"))
      graflist[[i]] <- g
    }
  } else {
    graflist = NULL
  }
  graflist
})

# ** Warning for missing values ####
output$DIF_logistic_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})

# ** Warning for missing values ####
output$DIF_logistic_item_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * NLR DIF ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Model for print ######
model_DIF_NLR_print <- reactive({
  data <- data.frame(binary())
  group <- unlist(group())

  model <- input$DIF_NLR_model_print
  type <- paste0(input$DIF_NLR_type_print, collapse = "")
  adj.method <- input$DIF_NLR_correction_method_print
  purify <- input$DIF_NLR_purification_print

  fit <- difNLR(Data = data, group = group, focal.name = 1,
                model = model, type = type,
                p.adjust.method = adj.method, purify = purify,
                test = "LR")
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
                         "4PL" = c("a", "b", "c", "d"))
  # what parameters cannot be selected with choice of model
  disSelection <- setdiff(letters[1:4], enaSelection)

  # converting letters to numbers
  myLetters <- letters[1:26]
  disNum <- match(disSelection, myLetters)
  enaNum <- match(enaSelection, myLetters)

  # updating selected choices for type of DIF
  updateCheckboxGroupInput(session = session,
                           inputId = "DIF_NLR_type_print",
                           selected = enaSelection)

  # create object that identifies enabled and disabled options
  disElement <- paste0("#DIF_NLR_type_print :nth-child(", disNum,") label")
  enaElement <- paste0("#DIF_NLR_type_print :nth-child(", enaNum,") label")

  # disable checkbox options of group
  shinyjs::enable(selector = enaElement)
  shinyjs::disable(selector = disElement)
})

# ** Equation ####
output$DIF_NLR_equation_print <- renderUI({
  model <- input$DIF_NLR_model_print

  if (model == "Rasch"){
    txta <- ""
  } else {
    if (model == "1PL"){
      txta <- "a_j"
    } else {
      txta <- "a_{jG_i}"
    }
  }

  txtb <- "b_{jG_i}"

  txt2 <- paste0(txta, "\\left(Z_i - ", txtb, "\\right)")
  txt2 <- paste0("e^{", txt2, "}")
  txt2 <- paste0("\\frac{", txt2,"}{1 + ", txt2,"}")

  if (model %in% c("3PLcg", "4PLcgdg", "4PLcg")){
    txtc <- "c_j"
  } else {
    if (model %in% c("3PLc", "4PLdg", "4PL")){
      txtc <- "c_{jG_i}"
    } else {
      txtc <- ""
    }
  }

  if (model %in% c("3PLdg", "4PLcgdg", "4PLdg")){
    txtd <- "d_j"
  } else {
    if (model %in% c("3PLd", "4PLcg", "4PL")){
      txtd <- "d_{jG_i}"
    } else {
      txtd <- ""
    }
  }

  if (txtc == "" & txtd == ""){
    txt3 <- ""
  } else {
    if (txtd == ""){
      txt3 <- paste0(txtc, " + \\left(1 - ", txtc, "\\right) \\cdot ")
    } else {
      if (txtc == ""){
        txt3 <- txtd
      } else {
        txt3 <- paste0(txtc, " + \\left(", txtd, " - ", txtc, "\\right) \\cdot ")
      }
    }
  }

  txtp <- c(txta, txtb, txtc, txtd)
  txtp <- txtp[txtp != ""]

  txt1 <- paste0("\\mathrm{P}\\left(Y_{ij} = 1 | Z_i, G_i, ",
                paste(txtp, collapse = ", "),
                "\\right) = ")

  txt <- withMathJax(paste0("$$", txt1, txt3, txt2, "$$"))
  txt

})

# ** Output print ######
output$print_DIF_NLR <- renderPrint({
  print(model_DIF_NLR_print())
})

# ** Updating inputs for plot based on print ####
observeEvent(input$DIF_NLR_model_print,{
  if (all(input$DIF_NLR_model_plot != input$DIF_NLR_model_print)){
    updateSelectInput(session = session,
                      inputId = "DIF_NLR_model_plot",
                      selected = input$DIF_NLR_model_print)
  }
})

observeEvent(input$DIF_NLR_type_print,{
  delay(3000,
        if (length(input$DIF_NLR_type_plot) != length(input$DIF_NLR_type_print) ||
            all(input$DIF_NLR_type_plot != input$DIF_NLR_type_print)){
          updateCheckboxGroupInput(session = session,
                                   inputId = "DIF_NLR_type_plot",
                                   selected = input$DIF_NLR_type_print)
        })
})

observeEvent(input$DIF_NLR_correction_method_print,{
  if (all(input$DIF_NLR_correction_method_plot != input$DIF_NLR_correction_method_print)){
    updateSelectInput(session = session,
                      inputId = "DIF_NLR_correction_method_plot",
                      selected = input$DIF_NLR_correction_method_print)
  }
})

observeEvent(input$DIF_NLR_purification_print,{
  if (all(input$DIF_NLR_purification_plot != input$DIF_NLR_purification_print)){
    updateCheckboxInput(session = session,
                        inputId = "DIF_NLR_purification_plot",
                        value = input$DIF_NLR_purification_print)
  }
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
                         "4PL" = c("a", "b", "c", "d"))
  # what parameters cannot be selected with choice of model
  disSelection <- setdiff(letters[1:4], enaSelection)

  # converting letters to numbers
  myLetters <- letters[1:26]
  disNum <- match(disSelection, myLetters)
  enaNum <- match(enaSelection, myLetters)

  # updating selected choices for type of DIF
  updateCheckboxGroupInput(session = session,
                           inputId = "DIF_NLR_type_plot",
                           selected = enaSelection)

  # create object that identifies enabled and disabled options
  disElement <- paste0("#DIF_NLR_type_plot :nth-child(", disNum,") label")
  enaElement <- paste0("#DIF_NLR_type_plot :nth-child(", enaNum,") label")

  # disable checkbox options of group
  shinyjs::enable(selector = enaElement)
  shinyjs::disable(selector = disElement)
})

# ** Updating inputs for plot print on plot ####
observeEvent(input$DIF_NLR_model_plot,{
  if (all(input$DIF_NLR_model_plot != input$DIF_NLR_model_print)){
    updateSelectInput(session = session,
                      inputId = "DIF_NLR_model_print",
                      selected = input$DIF_NLR_model_plot)
  }
})

observeEvent(input$DIF_NLR_type_plot,{
  delay(3000,
        if (length(input$DIF_NLR_type_plot) != length(input$DIF_NLR_type_print) ||
            all(input$DIF_NLR_type_plot != input$DIF_NLR_type_print)){
          updateCheckboxGroupInput(session = session,
                                   inputId = "DIF_NLR_type_print",
                                   selected = input$DIF_NLR_type_plot)
  })
})

observeEvent(input$DIF_NLR_correction_method_plot,{
  if (all(input$DIF_NLR_correction_method_plot != input$DIF_NLR_correction_method_print)){
    updateSelectInput(session = session,
                      inputId = "DIF_NLR_correction_method_print",
                      selected = input$DIF_NLR_correction_method_plot)
  }
})
observeEvent(input$DIF_NLR_purification_plot,{
  if (all(input$DIF_NLR_purification_plot != input$DIF_NLR_purification_print)){
    updateCheckboxInput(session = session,
                        inputId = "DIF_NLR_purification_print",
                        value = input$DIF_NLR_purification_plot)
  }
})

# ** Model for plot ######
model_DIF_NLR_plot <- reactive({
  data <- data.frame(binary())
  group <- unlist(group())

  model <- input$DIF_NLR_model_print
  type <- paste0(input$DIF_NLR_type_print, collapse = "")
  adj.method <- input$DIF_NLR_correction_method_print
  purify <- input$DIF_NLR_purification_print

  fit <- difNLR(Data = data, group = group, focal.name = 1,
                model = model, type = type,
                p.adjust.method = adj.method, purify = purify)
  fit
})

# ** Plot ######
plot_DIF_NLRInput <- reactive({
  fit <- model_DIF_NLR_plot()
  item <- input$DIF_NLR_item_plot

  g <- plot(fit, item = item)[[1]] +
    theme_app() +
    theme(legend.box.just = "top",
          legend.position = c(0.01, 0.98),
          legend.justification = c(0, 1),
          legend.key.width = unit(1, "cm"),
          legend.box = "horizontal") +
    ggtitle(item_names()[item])
  g
})

# ** Output plot ######
output$plot_DIF_NLR <- renderPlot({
  plot_DIF_NLRInput()
})

# ** Plot download ######
output$DP_plot_DIF_NLR <- downloadHandler(
  filename =  function() {
    paste0("fig_DIFNonlinear_", item_names()[input$DIF_NLR_item_plot], ".png")
  },
  content = function(file) {
    ggsave(file, plot = plot_DIF_NLRInput() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# ** Equation ####
output$DIF_NLR_equation_plot <- renderUI({
  model <- input$DIF_NLR_model_plot

  if (model == "Rasch"){
    txta <- ""
  } else {
    if (model == "1PL"){
      txta <- "a_j"
    } else {
      txta <- "a_{jG_i}"
    }
  }

  txtb <- "b_{jG_i}"

  txt2 <- paste0(txta, "\\left(Z_i - ", txtb, "\\right)")
  txt2 <- paste0("e^{", txt2, "}")
  txt2 <- paste0("\\frac{", txt2,"}{1 + ", txt2,"}")

  if (model %in% c("3PLcg", "4PLcgdg", "4PLcg")){
    txtc <- "c_j"
  } else {
    if (model %in% c("3PLc", "4PLdg", "4PL")){
      txtc <- "c_{jG_i}"
    } else {
      txtc <- ""
    }
  }

  if (model %in% c("3PLdg", "4PLcgdg", "4PLdg")){
    txtd <- "d_j"
  } else {
    if (model %in% c("3PLd", "4PLcg", "4PL")){
      txtd <- "d_{jG_i}"
    } else {
      txtd <- ""
    }
  }

  if (txtc == "" & txtd == ""){
    txt3 <- ""
  } else {
    if (txtd == ""){
      txt3 <- paste0(txtc, " + \\left(1 - ", txtc, "\\right) \\cdot ")
    } else {
      if (txtc == ""){
        txt3 <- txtd
      } else {
        txt3 <- paste0(txtc, " + \\left(", txtd, " - ", txtc, "\\right) \\cdot ")
      }
    }
  }

  txtp <- c(txta, txtb, txtc, txtd)
  txtp <- txtp[txtp != ""]

  txt1 <- paste0("\\mathrm{P}\\left(Y_{ij} = 1 | Z_i, G_i, ",
                 paste(txtp, collapse = ", "),
                 "\\right) = ")

  txt <- withMathJax(paste0("$$", txt1, txt3, txt2, "$$"))
  txt

})

# ** Table of coefficients ######
output$tab_coef_DIF_NLR <- renderTable({
  item <- input$DIF_NLR_item_plot
  fit <- model_DIF_NLR_plot()

  tab_coef <- fit$nlrPAR[[item]]
  tab_sd <- fit$nlrSE[[item]]

  tab <- t(rbind(tab_coef, tab_sd))
  withMathJax()
  rownames(tab) <- c('%%mathit{a}%%', '%%mathit{b}%%', '%%mathit{a}_{DIF}%%', '%%mathit{b}_{DIF}%%', '%%mathit{c}%%')
  colnames(tab) <- c("Estimate", "SD")

  tab
},
include.rownames = T)

# ** Warning for missing values ####
output$DIF_NLR_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})

# ** Warning for missing values ####
output$DIF_NLR_item_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * IRT LORD ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Model for plot ######
model_DIF_IRT_Lord_plot <- reactive({
  group <- unlist(group())
  data <- data.frame(binary())

  if (input$type_plot_DIF_IRT_lord == "3PL"){
    guess <- itemPar3PL(data)[, 3]
  }

  mod <- switch(input$type_plot_DIF_IRT_lord,
                "1PL" = difLord(Data = data, group = group, focal.name = 1,
                                model = "1PL",
                                p.adjust.method = input$correction_method_DIF_IRT_lordItems,
                                purify = input$puri_Lord_plot),
                "2PL" = difLord(Data = data, group = group, focal.name = 1,
                                model = "2PL",
                                p.adjust.method = input$correction_method_DIF_IRT_lordItems,
                                purify = input$puri_Lord_plot),
                "3PL" = difLord(Data = data, group = group, focal.name = 1,
                                model = "3PL", c = guess,
                                p.adjust.method = input$correction_method_DIF_IRT_lordItems,
                                purify = input$puri_Lord_plot))
  mod
})

# ** Model for print ######
model_DIF_IRT_Lord_print <- reactive({
  group <- unlist(group())
  data <- data.frame(binary())

  if (input$type_print_DIF_IRT_lord == "3PL"){
    guess <- itemPar3PL(data)[, 3]
  }

  mod <- switch(input$type_print_DIF_IRT_lord,
                "1PL" = difLord(Data = data, group = group, focal.name = 1,
                                model = "1PL",
                                p.adjust.method = input$correction_method_DIF_IRT_lordSummary,
                                purify = input$puri_Lord),
                "2PL" = difLord(Data = data, group = group, focal.name = 1,
                                model = "2PL",
                                p.adjust.method = input$correction_method_DIF_IRT_lordSummary,
                                purify = input$puri_Lord),
                "3PL" = difLord(Data = data, group = group, focal.name = 1,
                                model = "3PL", c = guess,
                                p.adjust.method = input$correction_method_DIF_IRT_lordSummary,
                                purify = input$puri_Lord))
  mod
})

# ** Output print ######
output$print_DIF_IRT_Lord <- renderPrint({
  print(model_DIF_IRT_Lord_print())
})


# ** Plot ######
plot_DIF_IRT_LordInput <- reactive({
  fitLord <- model_DIF_IRT_Lord_plot()
  item <- input$difirt_lord_itemSlider

  g <- plotDIFirt(parameters = fitLord$itemParInit,
                  item = item,
                  item.name = item_names()[item])[[item]]
  g
})

output$plot_DIF_IRT_Lord <- renderPlot({
  plot_DIF_IRT_LordInput()
})

output$DP_plot_DIF_IRT_Lord <- downloadHandler(
  filename =  function() {
    paste("fig_DIFIRTLord_",item_names()[input$difirt_lord_itemSlider],".png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = plot_DIF_IRT_LordInput() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# ** Table with coefficients ######
tab_coef_DIF_IRT_Lord <- reactive({

  fitLord <- model_DIF_IRT_Lord_plot()
  m <- nrow(fitLord$itemParInit)/2

  mR <- fitLord$itemParInit[1:m, ]
  mF <- fitLord$itemParInit[(m+1):(2*m), ]
  mF <- itemRescale(mR, mF)

  par <- rbind(mR, mF)

  wh_coef <- switch(input$type_plot_DIF_IRT_lord,
                    "1PL" = 1,
                    "2PL" = 1:2,
                    "3PL" = c(1, 2, 6))
  wh_sd <- switch(input$type_plot_DIF_IRT_lord,
                  "1PL" = 2,
                  "2PL" = 3:4,
                  "3PL" = 3:4)

  item <- input$difirt_lord_itemSlider
  tab_coef <- c(par[c(item, m + item), wh_coef])
  tab_sd <- c(par[c(item, m + item), wh_sd])

  if (input$type_plot_DIF_IRT_lord == "3PL")
    tab_coef <- tab_coef[-6]

  if (input$type_plot_DIF_IRT_lord == "3PL")
    tab_sd <- c(tab_sd, NA)


  tab <- data.frame(tab_coef, tab_sd)
  rownames(tab) <- switch(input$type_plot_DIF_IRT_lord,
                          "1PL" = c("%%mathit{b}_{R}%%", "%%mathit{b}_{F}%%"),
                          "2PL" = c("%%mathit{a}_{R}%%", "%%mathit{a}_{F}%%", "%%mathit{b}_{R}%%", "%%mathit{b}_{F}%%"),
                          "3PL" = c("%%mathit{a}_{R}%%", "%%mathit{a}_{F}%%", "%%mathit{b}_{R}%%", "%%mathit{b}_{F}%%", "%%mathit{c}%%"))
  colnames(tab) <- c("Estimate", "SD")

  tab
})

# ** Interpretation ######
output$irtint_lord <- renderUI({
  type <- input$type_plot_DIF_IRT_lord
  withMathJax()
  txt <- switch(type,
                '1PL'= paste('As the parameters are estimated separately for groups, there is one
                             equation for each group. Parameters \\(b_{R}\\) and \\(b_{F}\\)
                             are difficulties for reference and focal group. '),
                '2PL'= paste('As the parameters are estimated
                             separately for groups, there is one equation for each group.
                             Parameters \\(a_{R}\\) and \\(b_{R}\\) are discrimination and
                             difficulty for reference group. Parameters \\(a_{F}\\) and
                             \\(b_{F}\\)
                             are discrimination and difficulty for focal group. '),
                '3PL'= paste('As the parameters are estimated
                             separately for groups, there is one equation for each group.
                             Parameters \\(a_{R}\\) and \\(b_{R}\\) are discrimination and
                             difficulty for reference group. Parameters  \\(a_{F}\\) and \\(b_{F}\\)
                             are discrimination and difficulty for focal group.
                             Parameter \\(c\\) is a common guessing parameter. '))
  withMathJax(HTML(txt))
})

# ** Equation ######
output$irteq_lord <- renderUI({
  type <- input$type_plot_DIF_IRT_lord
  eqR <- switch(type,
                '1PL' = paste('$$\\mathrm{P}\\left(Y_{ij} = 1 | \\theta_i, G_i = 0, b_{Rj}\\right) =
                              \\frac{e^{\\theta_i - b_{Rj}}}
                              {1+e^{\\theta_i - b_{Rj} }} $$'),
                '2PL' = paste('$$\\mathrm{P}\\left(Y_{ij} = 1 | \\theta_i, G_i = 0, a_{Rj}, b_{Rj}\\right) =
                              \\frac{e^{a_{Rj} \\left(\\theta_i - b_{Rj} \\right)}}
                              {1+e^{a_{Rj} \\left(\\theta_i - b_{Rj} \\right)}} $$'),
                '3PL' = paste('$$\\mathrm{P}\\left(Y_{ij} = 1 | \\theta_i, G_i = 0, a_{Rj}, b_{Rj}, c_j\\right) =
                              c_j + \\left(1 - c_j\\right) \\cdot \\frac{e^{a_{Rj}
                              \\left(\\theta_i - b_{Rj} \\right)}}
                              {1+e^{a_{Rj} \\left(\\theta_i - b_{Rj} \\right)}} $$'))

  eqF <- switch(type,
                '1PL' = paste('$$\\mathrm{P}\\left(Y_{ij} = 1 | \\theta_i, G_i = 1, b_{Fj}\\right) =
                              \\frac{e^{\\theta_i - b_{Fj}}}
                              {1+e^{\\theta_i - b_{Fj}}} $$'),
                '2PL' = paste('$$\\mathrm{P}\\left(Y_{ij} = 1 | \\theta_i, G_i = 1, a_{Fj}, b_{Fj}\\right) =
                              \\frac{e^{a_{Fj} \\left(\\theta_i - b_{Fj} \\right)}}
                              {1+e^{a_{Fj} \\left(\\theta_i - b_{Fj} \\right)}} $$'),
                '3PL' = paste('$$\\mathrm{P}\\left(Y_{ij} = 1 | \\theta_i, G_i = 1, a_{Fj}, b_{Fj}, c_j\\right) =
                              c_j + \\left(1 - c_j\\right) \\cdot \\frac{e^{a_{Fj}
                              \\left(\\theta_i - b_{Fj} \\right)}}
                              {1+e^{a_{Fj} \\left(\\theta_i - b_{Fj} \\right)}} $$'))
  withMathJax(paste(eqR, eqF))
})

# ** Table with coefficients output ######
output$tab_coef_DIF_IRT_Lord <- renderTable({
  tab_coef_DIF_IRT_Lord()
},
include.rownames = T,
include.colnames = T)

# ** Warning for missing values ####
output$DIF_IRT_LORD_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})

# ** Warning for missing values ####
output$DIF_IRT_LORD_item_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * IRT Raju ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Model for plot ######
model_DIF_IRT_Raju_plot <- reactive({
  group <- unlist(group())
  data <- data.frame(binary())

  if (input$type_plot_DIF_IRT_raju == "3PL"){
    guess <- itemPar3PL(data)[, 3]
  }

  mod <- switch(input$type_plot_DIF_IRT_raju,
                "1PL" = difRaju(Data = data, group = group, focal.name = 1,
                                model = "1PL",
                                p.adjust.method = input$correction_method_DIF_IRT_rajuItems,
                                purify = input$puri_Raju_plot),
                "2PL" = difRaju(Data = data, group = group, focal.name = 1,
                                model = "2PL",
                                p.adjust.method = input$correction_method_DIF_IRT_rajuItems,
                                purify = input$puri_Raju_plot),
                "3PL" = difRaju(Data = data, group = group, focal.name = 1,
                                model = "3PL", c = guess,
                                p.adjust.method = input$correction_method_DIF_IRT_rajuItems,
                                purify = input$puri_Raju_plot))
  mod
})

# ** Model for print ######
model_DIF_IRT_Raju_print <- reactive({
  group <- unlist(group())
  data <- data.frame(binary())

  if (input$type_print_DIF_IRT_raju == "3PL"){
    guess <- itemPar3PL(data)[, 3]
  }

  mod <- switch(input$type_print_DIF_IRT_raju,
                "1PL" = difRaju(Data = data, group = group, focal.name = 1,
                                model = "1PL",
                                p.adjust.method = input$correction_method_DIF_IRT_rajuSummary,
                                purify = input$puri_Raju),
                "2PL" = difRaju(Data = data, group = group, focal.name = 1,
                                model = "2PL",
                                p.adjust.method = input$correction_method_DIF_IRT_rajuSummary,
                                purify = input$puri_Raju),
                "3PL" = difRaju(Data = data, group = group, focal.name = 1,
                                model = "3PL", c = guess,
                                p.adjust.method = input$correction_method_DIF_IRT_rajuSummary,
                                purify = input$puri_Raju))
  mod
})

# ** Output print ######
output$print_DIF_IRT_Raju <- renderPrint({
  print(model_DIF_IRT_Raju_print())
})



# ** Plot ######
plot_DIF_IRT_RajuInput <- reactive({
  fitRaju <- model_DIF_IRT_Raju_plot()
  item <- input$difirt_raju_itemSlider

  g <- plotDIFirt(parameters = fitRaju$itemParInit, test = "Raju",
                  item = item, item.name = item_names()[item])[[item]]
  g
})

output$plot_DIF_IRT_Raju <- renderPlot({
  plot_DIF_IRT_RajuInput()
})

output$DP_plot_DIF_IRT_Raju <- downloadHandler(
  filename =  function() {
    paste("fig_DIFIRTRaju_",item_names()[input$difirt_raju_itemSlider],".png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = plot_DIF_IRT_RajuInput() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# ** Interpretation ######
output$irtint_raju <- renderUI({
  type <- input$type_plot_DIF_IRT_raju
  withMathJax()
  txt <- switch(type,
                '1PL'= paste('As the parameters are estimated separately for groups, there is one
                             equation for each group. Parameters \\(b_{R}\\) and  \\(b_{F}\\)
                             are difficulties for reference and focal group. '),
                '2PL'= paste('As the parameters are estimated
                             separately for groups, there is one equation for each group.
                             Parameters \\(a_{R}\\) and \\(b_{R}\\) are discrimination and
                             difficulty for reference group. Parameters \\(a_{F}\\) and
                             \\(b_{F}\\)
                             are discrimination and difficulty for focal group. '),
                '3PL'= paste('As the parameters are estimated
                             separately for groups, there is one equation for each group.
                             Parameters \\(a_{R}\\) and \\(b_{R}\\) are discrimination and
                             difficulty for reference group. Parameters \\(a_{F}\\) and \\(b_{F}\\)
                             are discrimination and difficulty for focal group.
                             Parameter \\(c\\) is a common guessing parameter. '))
  withMathJax(HTML(txt))
})


# ** Equation ######
output$irteq_raju <- renderUI({
  type <- input$type_plot_DIF_IRT_raju
  eqR <- switch(type,
                '1PL' = paste('$$\\mathrm{P}\\left(Y_{ij} = 1 | \\theta_i, G_i = 0, b_{Rj}\\right) =
                              \\frac{e^{\\theta_i - b_{Rj}}}
                              {1+e^{\\theta_i - b_{Rj} }} $$'),
                '2PL' = paste('$$\\mathrm{P}\\left(Y_{ij} = 1 | \\theta_i, G_i = 0, a_{Rj}, b_{Rj}\\right) =
                              \\frac{e^{a_{Rj} \\left(\\theta_i - b_{Rj} \\right)}}
                              {1+e^{a_{Rj} \\left(\\theta_i - b_{Rj} \\right)}} $$'),
                '3PL' = paste('$$\\mathrm{P}\\left(Y_{ij} = 1 | \\theta_i, G_i = 0, a_{Rj}, b_{Rj}, c_j\\right) =
                              c_j + \\left(1 - c_j\\right) \\cdot \\frac{e^{a_{Rj}
                              \\left(\\theta_i - b_{Rj} \\right)}}
                              {1+e^{a_{Rj} \\left(\\theta_i - b_{Rj} \\right)}} $$'))

  eqF <- switch(type,
                '1PL' = paste('$$\\mathrm{P}\\left(Y_{ij} = 1 | \\theta_i, G_i = 1, b_{Fj}\\right) =
                              \\frac{e^{\\theta_i - b_{Fj}}}
                              {1+e^{\\theta_i - b_{Fj}}} $$'),
                '2PL' = paste('$$\\mathrm{P}\\left(Y_{ij} = 1 | \\theta_i, G_i = 1, a_{Fj}, b_{Fj}\\right) =
                              \\frac{e^{a_{Fj} \\left(\\theta_i - b_{Fj} \\right)}}
                              {1+e^{a_{Fj} \\left(\\theta_i - b_{Fj} \\right)}} $$'),
                '3PL' = paste('$$\\mathrm{P}\\left(Y_{ij} = 1 | \\theta_i, G_i = 1, a_{Fj}, b_{Fj}, c_j\\right) =
                              c_j + \\left(1 - c_j\\right) \\cdot \\frac{e^{a_{Fj}
                              \\left(\\theta_i - b_{Fj} \\right)}}
                              {1+e^{a_{Fj} \\left(\\theta_i - b_{Fj} \\right)}} $$'))
  withMathJax(paste(eqR, eqF))


})

# ** Table with coefficients ######
tab_coef_DIF_IRT_Raju <- reactive({

  fitRaju <- model_DIF_IRT_Raju_plot()
  m <- nrow(fitRaju$itemParInit)/2

  mR <- fitRaju$itemParInit[1:m, ]
  mF <- fitRaju$itemParInit[(m+1):(2*m), ]
  mF <- itemRescale(mR, mF)

  par <- rbind(mR, mF)

  wh_coef <- switch(input$type_plot_DIF_IRT_raju,
                    "1PL" = 1,
                    "2PL" = 1:2,
                    "3PL" = c(1, 2, 6))
  wh_sd <- switch(input$type_plot_DIF_IRT_raju,
                  "1PL" = 2,
                  "2PL" = 3:4,
                  "3PL" = 3:4)
  item <- input$difirt_raju_itemSlider

  tab_coef <- c(par[c(item, m + item), wh_coef])
  tab_sd <- c(par[c(item, m + item), wh_sd])

  if (input$type_plot_DIF_IRT_raju == "3PL")
    tab_coef <- tab_coef[-6]

  if (input$type_plot_DIF_IRT_raju == "3PL")
    tab_sd <- c(tab_sd, NA)

  tab <- data.frame(tab_coef, tab_sd)
  rownames(tab) <- switch(input$type_plot_DIF_IRT_raju,
                          "1PL" = c("%%mathit{b}_{R}%%", "%%mathit{b}_{F}%%"),
                          "2PL" = c("%%mathit{a}_{R}%%", "%%mathit{a}_{F}%%", "%%mathit{b}_{R}%%", "%%mathit{b}_{F}%%"),
                          "3PL" = c("%%mathit{a}_{R}%%", "%%mathit{a}_{F}%%", "%%mathit{b}_{R}%%", "%%mathit{b}_{F}%%", "%%mathit{c}%%"))
  colnames(tab) <- c("Estimate", "SD")

  tab
})

# ** Table with coefficients output ######
output$tab_coef_DIF_IRT_Raju <- renderTable({
  tab_coef_DIF_IRT_Raju()
},
include.rownames = T,
include.colnames = T)

# ** Warning for missing values ####
output$DIF_Raju_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})

# ** Warning for missing values ####
output$DIF_Raju_item_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * SIBTEST ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Model for print ####
DIF_SIBTEST_model <- reactive({
  # data
  group <- unlist(group())
  a <- data.frame(binary())
  colnames(a) <- item_names()

  # inputs
  type <- input$DIF_SIBTEST_type
  purify <- input$DIF_SIBTEST_purification
  adj.method <- input$DIF_SIBTEST_correction_method

  # model
  fit <- difSIBTEST(Data = a, group = group, focal.name = 1,
                    type = type,
                    purify = purify, p.adjust.method = adj.method)

  fit
})

# ** Output print ######
output$DIF_SIBTEST_print <- renderPrint({
  print(DIF_SIBTEST_model())
})

# ** Warning for missing values ####
output$DIF_SIBTEST_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})

# ** Warning for missing values ####
output$DIF_SIBTEST_item_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * DDF ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Model for print ####
model_DDF_print <- reactive({
  group <- unlist(group())
  a <- data.frame(nominal())
  colnames(a) <- item_names()
  k <- key()

  adj.method <- input$correction_method_print_DDF
  type <- input$type_print_DDF
  purify <- input$puri_DDF_print

  fit <- ddfMLR(Data = a, group = group, focal.name = 1,
                key = k, p.adjust.method = adj.method,
                type = type, purify = purify)

  fit
})

model_DDF_print_report <- reactive({
  group <- unlist(group())
  a <- data.frame(nominal())
  colnames(a) <- item_names()
  k <- key()

  if (!input$customizeCheck) {
    adj.method <- input$correction_method_print_DDF
    type <- input$type_print_DDF
    purify <- input$puri_DDF_print
  } else {
    adj.method <- input$correction_method_DDF_report
    type <- input$type_DDF_report
    purify <- input$puri_DDF_report
  }

  fit <- ddfMLR(Data = a, group = group, focal.name = 1,
                key = k, p.adjust.method = adj.method,
                type = type, purify = purify)

  fit
})

# ** Output print ######
output$print_DDF <- renderPrint({
  print(model_DDF_print())
})

# ** Model for plot ######
model_DDF_plot <- reactive({
  group <- unlist(group())
  a <- data.frame(nominal())
  colnames(a) <- item_names()
  k <- key()

  adj.method <- input$correction_method_plot_DDF
  type <- input$type_plot_DDF
  purify <- input$puri_DDF_plot

  fit <- ddfMLR(Data = a, group = group, focal.name = 1,
                key = k, p.adjust.method = adj.method,
                type = type, purify = purify)

  fit
})

# ** Plot ######
plot_DDFInput <- reactive({
  fit <- model_DDF_plot()
  item <- input$ddfSlider

  g <- plot(fit, item = item)[[1]]
  g <- g +
    theme_app() +
    theme(legend.box.just = "top",
          legend.position = c(0.01, 0.98),
          legend.justification = c(0, 1),
          legend.key.width = unit(1, "cm"),
          legend.box = "horizontal") +
    ggtitle(item_names()[item])
  g
})

plot_DDFReportInput <- reactive({
  group <- unlist(group())
  a <- data.frame(nominal())
  colnames(a) <- item_names()
  k <- key()

  if (!input$customizeCheck) {
    adj.method_report <- input$correction_method_plot_DDF
    type_report <- input$type_plot_DDF
    purify_report <- input$puri_DDF_plot
  } else {
    adj.method_report <- input$correction_method_DDF_report
    type_report <- input$type_DDF_report
    purify_report <- input$puri_DDF_report
  }

  mod <- ddfMLR(Data = a, group = group, focal.name = 1,
                key = k, p.adjust.method = adj.method_report,
                type = type_report, purify = purify_report)

  graflist = list()
  if (mod$DDFitems[[1]] != "No DDF item detected"){
  for (i in 1:length(mod$DDFitems)) {
    g <- plot(mod, item = mod$DDFitems[i])[[1]] +
      theme(text = element_text(size = 12),
            plot.title = element_text(size = 12, face = "bold", vjust = 1.5)) +
      ggtitle(paste("\nDDF multinomial plot for item ", item_numbers()[mod$DDFitems[i]]))
    graflist[[i]] <- g
  }
  } else {
   graflist = NULL
  }
  graflist
})


# ** Output Plot ######
output$plot_DDF <- renderPlot({
  plot_DDFInput()
})

# ** Plot download ######
output$DP_plot_DDF <- downloadHandler(
  filename =  function() {
    paste("fig_DDF_",item_names()[input$ddfSlider],".png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = plot_DDFInput() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# ** Table of coefficients ######
output$tab_coef_DDF <- renderTable({
  item <- input$ddfSlider
  fit <- model_DDF_plot()

  tab_coef <- fit$mlrPAR[[item]]
  tab_se <- fit$mlrSE[[item]]
  tab_se <- matrix(tab_se, ncol = ncol(tab_coef), byrow = T)

  if (ncol(tab_coef) == 2){
    tab_coef <- data.frame(tab_coef, 0, 0)
    tab_se <- data.frame(tab_se, 0, 0)
  } else {
    if (ncol(tab_coef) == 3){
      tab_coef <- data.frame(tab_coef, 0)
      tab_se <- data.frame(tab_se, 0)
    }
  }


  colnames(tab_se) <- colnames(tab_coef) <- c("b0", "b1", "b2", "b3")
  rownames(tab_se) <- rownames(tab_coef)

  tab_coef <- data.frame(tab_coef, answ = rownames(tab_coef))
  tab_se <- data.frame(tab_se, answ = rownames(tab_se))

  df1 <- melt(tab_coef, id = "answ")
  df2 <- melt(tab_se, id = "answ")
  tab <- data.frame(df1$value,
                    df2$value)
  rownames(tab) <- paste('%%mathit{',substr(df1$variable, 1, 1),'}_{',
                         df1$answ,
                         substr(df1$variable, 2, 2),'}%%', sep = "")
  colnames(tab) <- c("Estimate", "SD")
  tab

},
include.rownames = T)


# ** Equation ######
output$DDFeq <- renderUI ({
  item <- input$ddfSlider
  key <- key()

  cor_option <- key[item]
  withMathJax(
    sprintf(
      '$$\\text{For item } %s \\text{ are corresponding equations of multinomial model given by: } \\\\
           \\mathrm{P}(Y_{i} = %s|Z_i, G_i, b_{l0}, b_{l1}, b_{l2}, b_{l3}, l = 1,\\dots,K-1) =
           \\frac{1}{1 + \\sum_l e^{\\left( b_{l0} + b_{l1} Z_i + b_{l2} G_i + b_{l3} Z_i:G_i\\right)}}, \\\\
\\mathrm{P}(Y_{i} = k|Z_i, G_i, b_{l0}, b_{l1}, b_{l2}, b_{l3}, l = 1,\\dots,K-1) =
           \\frac{e^{\\left( b_{k0} + b_{k1} Z_i + b_{k2} G_i + b_{k3} Z_i:G_i\\right)}}
                 {1 + \\sum_l e^{\\left( b_{l0} + b_{l1} Z_i + b_{l2} G_i + b_{l3} Z_i:G_i\\right)}}, \\\\
        \\text{where } %s \\text{ is the correct answer and } k \\text{ is one of the wrong options.}$$',
      item, cor_option, cor_option, cor_option, cor_option
    )
  )
})

output$method_comparison_table <- renderTable({

	delta <- deltaGpurn()$DIFitems
	DIF_MH <-model_DIF_MH()$DIFitems
	DIF_LOG <- model_DIF_logistic_print()$DIFitems
	DIF_NLR <- model_DIF_NLR_print()$DIFitems
	DIF_IRT <- model_DIF_IRT_Lord_print()$DIFitems
	DIF_RAJU <- model_DIF_IRT_Raju_print()$DIFitems
	DIF_SIBTEST <- DIF_SIBTEST_model()$DIFitems
	DFF <- model_DDF_print()$DIFitems

	k <- length(item_names())

	v1 <- rep(0, k)
	v2 <- rep(0, k)
	v3 <- rep(0, k)
	v4 <- rep(0, k)
	v5 <- rep(0, k)
	v6 <- rep(0, k)
	v7 <- rep(0, k)
	v8 <- rep(0, k)

	invisible(ifelse(delta == 'no DIF item detected', TRUE, v1[delta] <- 1))
	invisible(ifelse(DIF_MH == 'no DIF item detected', TRUE, v2[DIF_MH] <- 1))
	invisible(ifelse(DIF_LOG == 'no DIF item detected', TRUE, v3[DIF_LOG] <- 1))
	invisible(ifelse(DIF_NLR == 'no DIF item detected', TRUE, v4[DIF_NLR] <- 1))
	invisible(ifelse(DIF_IRT == 'no DIF item detected', TRUE, v5[DIF_IRT] <- 1))
	invisible(ifelse(DIF_RAJU == 'no DIF item detected', TRUE, v6[DIF_RAJU] <- 1))
	invisible(ifelse(DIF_SIBTEST == 'no DIF item detected', TRUE, v7[DIF_SIBTEST] <- 1))
	invisible(ifelse(DFF == 'no DDF item detected', TRUE, v8[DFF] <- 1))

	tab <- as.data.frame(cbind(v1, v2, v3, v4, v5, v6, v7, v8))
	tab <- as.data.frame(apply(tab, c(1, 2), as.integer))
	rownames(tab) <- item_names()
	colnames(tab) <- c("Delta", "MH", "Logistic", "GLogistic", "Lord", "Raju", "SIBTEST", "DFF")

	n <- nrow(tab)
	k <- ncol(tab)

	rDIF <- apply(tab, 1, sum)
	cDIF <- apply(tab, 2, sum)
	cDIF[k + 1] <- 0

	tab <- cbind(tab, as.integer(rDIF))
	tab <- rbind(tab, as.integer(cDIF))

	rownames(tab)[n + 1] <- "Total"
	colnames(tab)[k + 1] <- "Total"

	tab
},
include.rownames = T,
include.colnames = T)

# ** Warning for missing values ####
output$DIF_DDF_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})

# ** Warning for missing values ####
output$DIF_DDF_item_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})

