#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# DIF/FAIRNESS ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * TOTAL SCORES ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Summary of Total Scores for Groups ######
resultsgroupInput<-reactive({
  sc_one  <- scored_test()[DIF_groups() == 1]
  sc_zero <- scored_test()[DIF_groups() == 0]
  tab <- t(data.frame(round(c(min(sc_zero, na.rm = T),
                              max(sc_zero, na.rm = T),
                              mean(sc_zero, na.rm = T),
                              median(sc_zero, na.rm = T),
                              sd(sc_zero, na.rm = T),
                              skewness(sc_zero, na.rm = T),
                              kurtosis(sc_zero, na.rm = T)), 2),
                      round(c(min(sc_one, na.rm = T),
                              max(sc_one, na.rm = T),
                              mean(sc_one, na.rm = T),
                              median(sc_one, na.rm = T),
                              sd(sc_one, na.rm = T),
                              skewness(sc_one, na.rm = T),
                              kurtosis(sc_one, na.rm = T)), 2)))
  colnames(tab) <- c("Min", "Max", "Mean", "Median", "SD", "Skewness", "Kurtosis")
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

  a <- test_answers()
  k <- test_key()
  sc  <- scored_test()[DIF_groups() == 1]


  bin <- as.numeric(input$inSlider2group)

  df <- data.frame(sc,
                   gr = cut(sc,
                            breaks = unique(c(0, bin - 1, bin, ncol(a))),
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

  g <- ggplot(df, aes(x = sc)) +
    geom_histogram(aes(fill = gr), binwidth = 1, color = "black") +
    scale_fill_manual("", breaks = df$gr, values = col) +
    labs(x = "Total score",
         y = "Number of respondents") +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(0, max(table(sc)) + 0.01 * nrow(a))) +
    scale_x_continuous(limits = c(-0.5, ncol(a) + 0.5)) +
    theme_shiny +
    ggtitle("Histogram of total scores for focal group")
  g
})

output$histbyscoregroup1 <- renderPlot ({
  histbyscoregroup1Input()
})

output$DP_histbyscoregroup1 <- downloadHandler(
  filename =  function() {
    paste("fig_HistogramForFocalGroup.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = histbyscoregroup1Input() +
             theme(text = element_text(size = 10)),
           device = "png",
           height = 4, width = 8, dpi = 300)
  }
)

# ** Histogram of total score for group = 0 (reference) ######
histbyscoregroup0Input <- reactive ({

  a <- test_answers()
  k <- test_key()
  sc  <- scored_test()[DIF_groups() == 0]

  bin <- as.numeric(input$inSlider2group)

  df <- data.frame(sc,
                   gr = cut(sc,
                            breaks = unique(c(0, bin - 1, bin, ncol(a))),
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

  g <- ggplot(df, aes(x = sc)) +
    geom_histogram(aes(fill = gr), binwidth = 1, color = "black") +
    scale_fill_manual("", breaks = df$gr, values = col) +
    labs(x = "Total score",
         y = "Number of respondents") +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(0, max(table(sc)) + 0.01 * nrow(a))) +
    scale_x_continuous(limits = c(-0.5, ncol(a) + 0.5)) +
    theme_shiny +
    ggtitle("Histogram of total scores for reference group")
  g
})

output$histbyscoregroup0 <- renderPlot ({
  histbyscoregroup0Input()
})

output$DP_histbyscoregroup0 <- downloadHandler(
  filename =  function() {
    paste("fig_HistogramForRefGroup.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = histbyscoregroup0Input() +
             theme(text = element_text(size = 10)),
           device = "png",
           height = 4, width = 8, dpi = 300)
  }
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * DELTA PLOT ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

deltaGpurn <- reactive ({
  switch(input$type_threshold,
         "Fixed" = deltaPlot(DPdata(), group = "group",
                             focal.name = 1,
                             thr = 1.5,
                             purify = input$puri_DP,
                             purType = input$puri_DP_type),
         "Normal"= deltaPlot(DPdata(), group = "group",
                             focal.name = 1,
                             thr = "norm",
                             purify = input$puri_DP,
                             purType = input$puri_DP_type)
  )
})

deltaGpurn_report <- reactive({
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
         "Fixed" = deltaPlot(DPdata(), group = "group",
                             focal.name = 1,
                             thr = 1.5,
                             purify = purify_report,
                             purType = purType_report),
         "Normal"= deltaPlot(DPdata(), group = "group",
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
    theme_shiny
  if (is.numeric(dp$DIFitems)){
    df2 <- df[dp$DIFitems, ]
    p <- p + geom_point(data = df2,
                        aes(x = X1, y = X2, label = nam),
                        size = 6, color = "black", shape = 1)
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
    theme_shiny

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
             theme(text = element_text(size = 10)),
           device = "png",
           height = 4, width = 8, dpi = 300)
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
  group <- unlist(DIF_groups())
  data <- data.frame(correct_answ())

  mod <- difMH(Data = data, group = group, focal.name = 1,
               p.adjust.method = input$correction_method_MZ_print,
               purify = input$puri_MH)
  mod
})

# ** Model for tables ######
model_DIF_MH_tables <- reactive({
  group <- unlist(DIF_groups())
  data <- data.frame(correct_answ())

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
  group <- unlist(DIF_groups())
  data <- data.frame(correct_answ())

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
  group <- unlist(DIF_groups())
  data <- data.frame(correct_answ())

  mod <- difLogistic(Data = data, group = group, focal.name = 1,
                     type = input$type_plot_DIF_logistic,
                     p.adjust.method = input$correction_method_logItems,
                     purify = input$puri_LR_plot)
  mod
})

# ** Model for print ######
model_DIF_logistic_print <- reactive({
  group <- unlist(DIF_groups())
  data <- data.frame(correct_answ())

  mod <- difLogistic(Data = data, group = group, focal.name = 1,
                     type = input$type_print_DIF_logistic,
                     p.adjust.method = input$correction_method_logSummary,
                     purify = input$puri_LR)
  mod
})

model_DIF_logistic_print_report <- reactive({
  group <- unlist(DIF_groups())
  data <- data.frame(correct_answ())

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
  group <- unlist(DIF_groups())
  data <- data.frame(correct_answ())

  type <- input$type_plot_DIF_logistic
  g <- plotDIFLogistic(data, group,
                       type = input$type_plot_DIF_logistic,
                       item =  input$diflogSlider,
                       IRT = F,
                       p.adjust.method = input$correction_method_logItems,
                       purify = input$puri_LR_plot)
  g <- g + theme_shiny
  g
})

output$plot_DIF_logistic <- renderPlot({
  plot_DIF_logisticInput()
})

output$DP_plot_DIF_logistic <- downloadHandler(
  filename =  function() {
    paste("fig_DifLogisticRegression_",item_names()[input$diflogSlider],".png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = plot_DIF_logisticInput() +
             theme(text = element_text(size = 10)),
           device = "png",
           height = 4, width = 10, dpi = 300)
  }
)

# ** Table with coefficients ######
output$tab_coef_DIF_logistic <- renderTable({

  fit <- model_DIF_logistic_plot()
  i <- input$diflogSlider

  tab_coef <- fit$logitPar[i, ]
  tab_sd <- fit$logitSe[i, ]

  tab <- data.frame(tab_coef, tab_sd)

  rownames(tab) <- c('b0', 'b1', 'b2', 'b3')
  colnames(tab) <- c("Estimate", "SD")

  tab
},
include.rownames = T,
include.colnames = T)

DIF_logistic_plotReport <- reactive({
  group <- unlist(DIF_groups())
  data <- data.frame(correct_answ())

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
                           IRT = F,
                           p.adjust.method = p.adjust.method_report,
                           purify = purify_report)
      g <- g + theme_shiny
      g = g + ggtitle(paste0("DIF logistic plot for item ", item_numbers()[mod$DIFitems[i]])) +
        theme(text = element_text(size = 12),
              plot.title = element_text(size = 12, face = "bold"))
      graflist[[i]] <- g
    }
  } else {
    graflist = NULL
  }
  graflist
})

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * LOGISTIC IRT Z ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Model for plot ######
model_DIF_logistic_IRT_Z_plot <- reactive({
  group <- unlist(DIF_groups())
  data <- data.frame(correct_answ())

  mod <- difLogistic(Data = data, group = group, focal.name = 1,
                     type = input$type_plot_DIF_logistic_IRT_Z,
                     match = scale(scored_test()),
                     p.adjust.method = input$correction_method_logZItems,
                     all.cov = T,
                     purify = F)
  mod
})

# ** Model for print ######
model_DIF_logistic_IRT_Z_print <- reactive({
  group <- unlist(DIF_groups())
  data <- data.frame(correct_answ())

  mod <- difLogistic(Data = data, group = group, focal.name = 1,
                     type = input$type_print_DIF_logistic_IRT_Z,
                     match = scale(scored_test()),
                     p.adjust.method = input$correction_method_logZSummary,
                     all.cov = T,
                     purify = F)
  mod
})

# ** Output print ######
output$print_DIF_logistic_IRT_Z <- renderPrint({
  print(model_DIF_logistic_IRT_Z_print())
})

# ** Plot ######
plot_DIF_logistic_IRT_ZInput <- reactive ({
  group <- unlist(DIF_groups())
  data <- data.frame(correct_answ())

  type <- input$type_plot_DIF_logistic
  g <- plotDIFLogistic(data, group,
                       type = input$type_plot_DIF_logistic_IRT_Z,
                       item =  input$diflog_irtSlider,
                       IRT = T,
                       p.adjust.method = input$correction_method_logZItems,
                       purify = F)
  g <- g + theme_shiny
  g
})

output$plot_DIF_logistic_IRT_Z <- renderPlot({
  plot_DIF_logistic_IRT_ZInput()
})

output$DP_plot_DIF_logistic_IRT_Z <- downloadHandler(
  filename =  function() {
    paste("fig_DIFLogisticIRTZ_",item_names()[input$diflog_irtSlider],".png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = plot_DIF_logistic_IRT_ZInput() +
             theme(text = element_text(size = 10)),
           device = "png",
           height = 4, width = 8, dpi = 300)
  }
)

output$tab_coef_DIF_logistic_IRT_Z <- renderTable({

  fit <- model_DIF_logistic_IRT_Z_plot()
  i <- input$diflog_irtSlider

  tab_coef_old <- fit$logitPar[i, ]

  tab_coef <- c()
  # a = b1, b = -b0/b1, adif = b3, bdif = -(b1b2-b0b3)/(b1(b1+b3))
  tab_coef[1] <- tab_coef_old[2]
  tab_coef[2] <- -(tab_coef_old[1] / tab_coef_old[2])
  tab_coef[3] <- tab_coef_old[4]
  tab_coef[4] <- -(tab_coef_old[2] * tab_coef_old[3] + tab_coef_old[1] * tab_coef_old[4] ) /
    (tab_coef_old[2] * (tab_coef_old[2] + tab_coef_old[4]))

  # delta method
  g <- list( ~ x2,  ~ -x1/x2, ~ x4, ~ -((x2 * x3 - x1 * x4) / (x2 * (x2 + x4))))
  if (is.character(fit$DIFitems) | !(i %in% fit$DIFitems)){
    d <- dim(fit$cov.M1[[i]])[1]
    cov <- matrix(0, ncol = 4, nrow = 4)
    cov[1:d, 1:d] <-  fit$cov.M1[[i]]
  } else {
    d <- dim(fit$cov.M0[[i]])[1]
    cov <- matrix(0, ncol = 4, nrow = 4)
    cov[1:d, 1:d] <-  fit$cov.M0[[i]]
  }
  cov <- as.matrix(cov)
  syms <- paste("x", 1:4, sep = "")
  for (i in 1:4) assign(syms[i], tab_coef_old[i])
  gdashmu <- t(sapply(g, function(form) {
    as.numeric(attr(eval(deriv(form, syms)), "gradient"))
    # in some shiny v. , envir = parent.frame() in eval() needs to be added
  }))
  new.covar <- gdashmu %*% cov %*% t(gdashmu)
  tab_sd <- sqrt(diag(new.covar))

  tab <- data.frame(tab_coef, tab_sd)

  rownames(tab) <- c('a', 'b', 'aDIF', 'bDIF')
  colnames(tab) <- c("Estimate", "SD")

  tab
},
include.rownames = T,
include.colnames = T)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * NLR DIF ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Model for print ######
model_DIF_NLR_print <- reactive({
  group <- unlist(DIF_groups())
  data <- data.frame(correct_answ())

  type <- input$type_print_DIF_NLR
  adj.method <- input$correction_method_nlrSummary
  model <- "3PLcg"
  purify <- input$puri_NLR_print

  fit <- difNLR(Data = data, group = group, focal.name = 1,
                model = model, type = type,
                p.adjust.method = adj.method, purify = purify)
  fit
})

# ** Output print ######
output$print_DIF_NLR <- renderPrint({
  print(model_DIF_NLR_print())
})

# ** Model for plot ######
model_DIF_NLR_plot <- reactive({
  group <- unlist(DIF_groups())
  data <- data.frame(correct_answ())

  type <- input$type_plot_DIF_NLR
  adj.method <- input$correction_method_nlrItems
  model <- "3PLcg"
  purify <- input$puri_NLR_plot

  fit <- difNLR(Data = data, group = group, focal.name = 1,
                model = model, type = type,
                p.adjust.method = adj.method, purify = purify)
  fit
})

# ** Plot ######
plot_DIF_NLRInput <- reactive({
  fit <- model_DIF_NLR_plot()
  item <- input$difnlrSlider

  g <- plot(fit, item = item)[[1]] +
    theme_shiny +
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
    paste("fig_DIFNonlinear_",item_names()[input$difnlrSlider],".png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = plot_DIF_NLRInput() +
             theme(text = element_text(size = 10)),
           device = "png",
           height = 4, width = 8, dpi = 300)
  }
)

# ** Table of coefficients ######
output$tab_coef_DIF_NLR <- renderTable({
  item <- input$difnlrSlider
  fit <- model_DIF_NLR_plot()

  tab_coef <- fit$nlrPAR[[item]][c('a', 'b', 'aDif', 'bDif', 'c')]
  tab_sd <- fit$nlrSE[[item]][c('a', 'b', 'aDif', 'bDif', 'c')]

  tab <- t(rbind(tab_coef, tab_sd))
  rownames(tab) <- c('a', 'b', 'aDIF', 'bDIF', 'c')
  colnames(tab) <- c("Estimate", "SD")

  tab
},
include.rownames = T)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * IRT LORD ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Model for plot ######
model_DIF_IRT_Lord_plot <- reactive({
  group <- unlist(DIF_groups())
  data <- data.frame(correct_answ())

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
  group <- unlist(DIF_groups())
  data <- data.frame(correct_answ())

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
  g <- g + theme_shiny
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
             theme(text = element_text(size = 10)),
           device = "png",
           height = 4, width = 8, dpi = 300)
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
                          "1PL" = c("bR", "bF"),
                          "2PL" = c("aR", "aF", "bR", "bF"),
                          "3PL" = c("aR", "aF", "bR", "bF", "c"))
  colnames(tab) <- c("Estimate", "SD")

  tab
})

# ** Interpretation ######
output$irtint_lord <- renderUI({
  type <- input$type_plot_DIF_IRT_lord
  txt <- switch(type,
                '1PL'= paste('As the parameters are estimated separately for groups, there is one
                             equation for each group. Parameters <b> bR </b> and <b> bF </b>
                             are difficulties for reference and focal group. '),
                '2PL'= paste('As the parameters are estimated
                             separately for groups, there is one equation for each group.
                             Parameters <b> aR </b> and <b> bR </b> are discrimination and
                             difficulty for reference group. Parameters <b> aF </b> and
                             <b> bF </b>
                             are discrimination and difficulty for focal group. '),
                '3PL'= paste('As the parameters are estimated
                             separately for groups, there is one equation for each group.
                             Parameters <b> aR </b> and <b> bR </b> are discrimination and
                             difficulty for reference group. Parameters <b> aF </b> and <b> bF </b>
                             are discrimination and difficulty for focal group.
                             Parameter <b> c </b> is a common guessing parameter. '))
  HTML(txt)
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

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * IRT Raju ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Model for plot ######
model_DIF_IRT_Raju_plot <- reactive({
  group <- unlist(DIF_groups())
  data <- data.frame(correct_answ())

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
  group <- unlist(DIF_groups())
  data <- data.frame(correct_answ())

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
  g <- g + theme_shiny
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
             theme(text = element_text(size = 10)),
           device = "png",
           height = 4, width = 8, dpi = 300)
  }
)

# ** Interpretation ######
output$irtint_raju <- renderUI({
  type <- input$type_plot_DIF_IRT_raju
  txt <- switch(type,
                '1PL'= paste('As the parameters are estimated separately for groups, there is one
                             equation for each group. Parameters <b> bR </b> and <b> bF </b>
                             are difficulties for reference and focal group. '),
                '2PL'= paste('As the parameters are estimated
                             separately for groups, there is one equation for each group.
                             Parameters <b> aR </b> and <b> bR </b> are discrimination and
                             difficulty for reference group. Parameters <b> aF </b> and
                             <b> bF </b>
                             are discrimination and difficulty for focal group. '),
                '3PL'= paste('As the parameters are estimated
                             separately for groups, there is one equation for each group.
                             Parameters <b> aR </b> and <b> bR </b> are discrimination and
                             difficulty for reference group. Parameters <b> aF </b> and <b> bF </b>
                             are discrimination and difficulty for focal group.
                             Parameter <b> c </b> is a common guessing parameter. '))
  HTML(txt)
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
                          "1PL" = c("bR", "bF"),
                          "2PL" = c("aR", "aF", "bR", "bF"),
                          "3PL" = c("aR", "aF", "bR", "bF", "c"))
  colnames(tab) <- c("Estimate", "SD")

  tab
})

# ** Table with coefficients output ######
output$tab_coef_DIF_IRT_Raju <- renderTable({
  tab_coef_DIF_IRT_Raju()
},
include.rownames = T,
include.colnames = T)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * DDF ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Model for print ####
model_DDF_print <- reactive({
  group <- unlist(DIF_groups())
  a <- data.frame(test_answers())
  colnames(a) <- item_names()
  k <- test_key()

  adj.method <- input$correction_method_print_DDF
  type <- input$type_print_DDF
  purify <- input$puri_DDF_print

  fit <- ddfMLR(Data = a, group = group, focal.name = 1,
                key = k, p.adjust.method = adj.method,
                type = type, purify = purify)

  fit
})

model_DDF_print_report <- reactive({
  group <- unlist(DIF_groups())
  a <- data.frame(test_answers())
  colnames(a) <- item_names()
  k <- test_key()

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
  group <- unlist(DIF_groups())
  a <- data.frame(test_answers())
  colnames(a) <- item_names()
  k <- test_key()

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
    theme_shiny +
    ggtitle(item_names()[item])
  g
})

plot_DDFReportInput <- reactive({
  group <- unlist(DIF_groups())
  a <- data.frame(test_answers())
  colnames(a) <- item_names()
  k <- test_key()

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
             theme(text = element_text(size = 10)),
           device = "png",
           height = 4, width = 8, dpi = 300)
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

  rownames(tab) <- paste(substr(df1$variable, 1, 1),
                         df1$answ,
                         substr(df1$variable, 2, 2), sep = "")
  colnames(tab) <- c("Estimate", "SD")
  tab
},
include.rownames = T)

# ** Equation ######
output$DDFeq <- renderUI ({
  item <- input$ddfSlider
  key <- test_key()

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
