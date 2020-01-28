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
  tab <- data.frame(rbind(round(c(length(sc_zero),
                                  min(sc_zero, na.rm = T),
                                  max(sc_zero, na.rm = T),
                                  mean(sc_zero, na.rm = T),
                                  median(sc_zero, na.rm = T),
                                  sd(sc_zero, na.rm = T),
                                  skewness(sc_zero, na.rm = T),
                                  kurtosis(sc_zero, na.rm = T)), 2),
                          round(c(length(sc_one),
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

# ** Update slider for histogram ####
observe({
  updateSliderInput(session = session, inputId = "inSlider2group",
                    min = min(total_score(), na.rm = T) ,
                    max = max(c(max(total_score(), na.rm = T), ncol(binary()))),
                    value = round(median(total_score(), na.rm = T)))
})

# ** Histogram of total score for group = 1 (focal) ######
histbyscoregroup1Input <- reactive({
  data <- binary()
  sc  <- total_score()[group() == 1]
  sc_lim <- total_score()
  bin <- as.numeric(input$inSlider2group)
  max.val <- max(prop.table(table(total_score()[group() == 0])),
                 prop.table(table(total_score()[group() == 1])))

  if (length(unique(c(min(sc, na.rm = T), bin - 1, bin, max(sc, na.rm = T)))) == 3 & bin != min(sc, na.rm = T)) {
    breaks <- unique(c(min(sc, na.rm = T), bin - 1, bin,bin + 1, max(sc, na.rm = T)))
  } else {
    breaks <- unique(c(min(sc, na.rm = T), bin - 1, bin, max(sc, na.rm = T)))
  }

  df <- data.table(score = sc,
                   gr = cut(sc,
                            breaks = breaks,
                            include.lowest = T))

  if (bin < min(sc, na.rm = T)){
    col <- c("blue",'blue')
  } else if (!(bin %in% unique(sc))) {
    col <- c('red','blue')
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
    scale_x_continuous(limits = c(min(sc_lim, na.rm = T) - 0.5, max(sc_lim, na.rm = T) + 0.5)) +
    #scale_y_continuous(limits = c(0, max.val)) +
    ggtitle("Focal group") +
    theme_app()
  g
})

output$histbyscoregroup1 <- renderPlotly ({
  sc <- total_score()[group() == 1]

  bin <- as.numeric(input$inSlider2group)
  data <- binary()

  if (min(sc, na.rm = TRUE) <= bin & bin <= max(sc, na.rm = TRUE)){
    if (bin %in% unique(sc)) {
      breaks <- unique(c(min(sc, na.rm = T), bin - 1,bin, max(sc, na.rm = T)))
    } else {
      breaks <- unique(c(min(sc, na.rm = T), bin - 1, max(sc, na.rm = T)))
    }
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
  l <- length(p$x$data)
  for(i in 1:k){
    if (i <= l) {
      t <- subset(df, df$gr == levels(df$gr)[i])
      t <- t[order(t$score)]
      t <- as.data.frame(table(t$score))
      lbnd <- ifelse(i == 1, ints[i], ints[i] + 1)
      hbnd <- ints[i+1]
      idx <- which(p$x$data[[i]]$x %in% lbnd:hbnd)
      c <- 1
      for (j in idx) {
        text <- strsplit(p$x$data[[i]]$text[j], "<br />")[[1]][1]
        text <- sub("/", "", text)
        text <- sub("countsum\\(count\\)", "Proportion", text)
        p$x$data[[i]]$text[j] <- paste(text, "<br />",
                                       "Number of respodents:",
                                       ifelse(c <= nrow(t) &
                                                t$Var1[c] %in% lbnd:hbnd &
                                                t$Var1[c] == p$x$data[[i]]$x[j], t$Freq[c], 0),
                                       "<br /> Score:", p$x$data[[i]]$x[j])
        c <- ifelse(t$Var1[c] != p$x$data[[i]]$x[j], c, c + 1)
      }
    } else {
      break
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
  sc_lim <- total_score()
  bin <- as.numeric(input$inSlider2group)
  max.val <- max(prop.table(table(total_score()[group() == 0])),
                 prop.table(table(total_score()[group() == 1])))

  if (length(unique(c(min(sc, na.rm = T), bin - 1, bin, max(sc, na.rm = T)))) == 3 & bin != min(sc, na.rm = T)) {
    breaks <- unique(c(min(sc, na.rm = T), bin - 1, bin,bin + 1, max(sc, na.rm = T)))
  } else {
    breaks <- unique(c(min(sc, na.rm = T), bin - 1, bin, max(sc, na.rm = T)))
  }

  df <- data.table(score = sc,
                   gr = cut(sc,
                            breaks = breaks,
                            include.lowest = T))

  if (bin < min(sc, na.rm = TRUE)){
    col <- c("blue",'blue')
  } else if (!(bin %in% unique(sc))) {
    col <- c('red','blue')
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
    scale_x_continuous(limits = c(min(sc_lim, na.rm = T) - 0.5, max(sc_lim, na.rm = T) + 0.5)) +
    #scale_y_continuous(limits = c(0, max.val)) +
    ggtitle("Reference group") +
    theme_app()
  g
})

output$histbyscoregroup0 <- renderPlotly ({

  sc <- total_score()[group() == 0]
  bin <- as.numeric(input$inSlider2group)
  data <- binary()

  if (min(sc, na.rm = TRUE) <= bin & bin <= max(sc, na.rm = TRUE)){
    if (bin %in% unique(sc)) {
      breaks <- unique(c(min(sc, na.rm = T), bin - 1,bin, max(sc, na.rm = T)))
    } else {
      breaks <- unique(c(min(sc, na.rm = T), bin - 1, max(sc, na.rm = T)))
    }
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
  l <- length(p$x$data)

  for(i in 1:k){
    if (i <= l) {
      t <- subset(df, df$gr == levels(df$gr)[i])
      t <- t[order(t$score)]
      t <- as.data.frame(table(t$score))
      lbnd <- ifelse(i == 1, ints[i], ints[i] + 1)
      hbnd <- ints[i+1]
      idx <- which(p$x$data[[i]]$x %in% lbnd:hbnd)
      c <- 1
      for (j in idx) {
        text <- strsplit(p$x$data[[i]]$text[j], "<br />")[[1]][1]
        text <- sub("/", "", text)
        text <- sub("countsum\\(count\\)", "Proportion", text)
        p$x$data[[i]]$text[j] <- paste(text, "<br />",
                                       "Number of respodents:",
                                       ifelse(c <= nrow(t) &
                                                t$Var1[c] %in% lbnd:hbnd &
                                                t$Var1[c] == p$x$data[[i]]$x[j], t$Freq[c], 0),
                                       "<br /> Score:", p$x$data[[i]]$x[j])
        c <- ifelse(t$Var1[c] != p$x$data[[i]]$x[j], c, c + 1)
      }
    } else {
      break
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

# Central DIF matching variable (DMV) presence control ######
dif_present <-
  reactive({
    !(length(dataset$DIFmatching) == 1 &
        any(dataset$DIFmatching == "missing"))
  })

# create vectors with DMV and purification inputs names
# (to feed "lapplies" in respective method section)
match_logistic = c("DIF_logistic_summary_matching", "DIF_logistic_items_matching")
puri_logistic = c("DIF_logistic_summary_purification", "DIF_logistic_items_purification")

match_NLR = c("DIF_NLR_summary_matching", "DIF_NLR_items_matching")
puri_NLR = c("DIF_NLR_purification_print", "DIF_NLR_purification_plot")

match_cum = c("DIF_cum_summary_matching", "DIF_cum_items_matching")
puri_cum = c("DIF_cum_purification_summary", "DIF_cum_purification_items")

match_adj = c("DIF_adj_summary_matching", "DIF_adj_items_matching")
puri_adj = c("DIF_adj_purification_summary", "DIF_adj_purification_items")

match_multi = c("DIF_multi_summary_matching", "DIF_multi_items_matching")
puri_multi = c("DDF_multi_purification_summary", "DDF_multi_purification_items")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * MANTEL-HAENSZEL ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** UPDATING INPUTS ######

# ** Updating item and score sliders ######
observe({

  item_count = ncol(binary())

  updateSliderInput(session = session,
                    inputId = "DIF_MH_items_item",
                    max = item_count)
    updateSliderInput(session = session,
                      inputId = "DIF_MH_items_score",
                      max = item_count,
                      value = median(total_score(), na.rm = T))
})

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** MODEL ######

# ** Model ######
DIF_MH_model <- reactive({
  group <- unlist(group())
  data <- data.frame(binary())

  fit <- .difMH_edited(Data = data, group = group, focal.name = 1,
                       p.adjust.method = input$DIF_MH_summary_correction,
                       purify = input$DIF_MH_summary_purification)
  fit
})

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** SUMMARY ######

# ** Output print ######
output$DIF_MH_summary_print <- renderPrint({
  print(DIF_MH_model())
})

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** ITEMS ######

# ** Contingency tables ######
DIF_MH_items_table <- reactive({
  group <- unlist(group())
  data <- data.frame(binary())
  total <- total_score()
  item <- input$DIF_MH_items_item
  score <- input$DIF_MH_items_score

  df <- data.frame(data[, item], group)
  colnames(df) <- c("Answer", "Group")
  df$Answer <- relevel(factor(df$Answer, labels = c("Incorrect", "Correct")),
                       "Correct")
  df$Group <- factor(df$Group, labels = c("Reference group", "Focal group"))

  df <- df[total == score, ]

  tab <- dcast(data.frame(xtabs(~ Group + Answer, data = df)),
               Group ~ Answer, value.var = "Freq", margins = T,
               fun = sum)

  colnames(tab)[4] <- tab$Group[3] <- levels(tab$Group)[3]  <- "Total"
  colnames(tab)[1] <- ""
  tab
})

# ** Contingency tables output ######
output$DIF_MH_items_table <- renderTable({
  DIF_MH_items_table()
})

# ** OR calculation ######
output$DIF_MH_items_interpretation <- renderUI({
  tab <- DIF_MH_items_table()
  a <- tab[1, 2]
  b <- tab[1, 3]
  c <- tab[2, 2]
  d <- tab[2, 3]
  OR <- round((a*d)/(b*c), 2)

  item <- input$DIF_MH_items_item
  score <- input$DIF_MH_items_score

  alphaMH <- round(DIF_MH_model()$alphaMH[item], 2)
  deltaMH <- -2.35 * log(alphaMH)
  effect_size <- symnum(abs(deltaMH), cutpoints = c(0, 1, 1.5, Inf), symbols = LETTERS[1:3])

  txt <- ifelse((b * c == 0)|(a * d == 0), "Odds ratio cannot be calculated!",
                paste0("For respondent who reached total score of ", score,
                       " the odds of answering item ", item_numbers()[item],
                       " correctly is ",
                       ifelse(OR == 1, " the same for both groups. ",
                              ifelse(OR > 1,
                                     paste0(OR, " times higher in the reference group than in the focal group."),
                                     paste0(OR, " times lower in the reference group than in the focal group.")))))

  txtMH <- paste0("Mantel-Haenszel estimate of odds ratio accounting for all levels of total score is equal to ",
                  alphaMH, ". The odds of answering item ", item_numbers()[item],
                  " correctly is ",
                  ifelse(alphaMH == 1, " the same for both groups. ",
                         ifelse(alphaMH > 1,
                                paste0(alphaMH, " times higher in the reference group than in the focal group."),
                                paste0(alphaMH, " times lower in the reference group than in the focal group."))))
  txtDelta <- paste0("Mantel-Haenszel D-DIF index is equal to ", round(deltaMH, 2), ". This indicates category ", effect_size,
                     " DIF effect size - ",
                     switch(effect_size,
                            "A" = "negligible",
                            "B" = "moderate",
                            "C" = "large"), ".")
  withMathJax(HTML(paste(sprintf(paste('$$\\mathrm{OR} = \\frac{%d \\cdot %d}{%d \\cdot %d} = %.2f$$', txt),
                                 a, d, b, c, OR), "<br/><br/>",
                         sprintf(paste('$$\\alpha_{\\mathrm{MH}} = %.2f$$', txtMH),
                                 alphaMH), "<br/><br/>",
                         sprintf(paste('$$\\Delta_{\\mathrm{MH}} = -2.35 \\cdot \\log(\\alpha_{\\mathrm{MH}}) = %.2f$$', txtDelta),
                                 deltaMH))))
})

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** REPORT ######

# ** Model for report ######
report_DIF_MH_model <- reactive({
  group <- unlist(group())
  data <- data.frame(binary())

  if (!input$customizeCheck) {
    fit <- DIF_MH_model()
  } else {
    p.adjust.method_report = input$correction_method_MH_report
    purify_report = input$puri_MH_report

    fit <- .difMH_edited(Data = data, group = group, focal.name = 1,
                         p.adjust.method = p.adjust.method_report,
                         purify = purify_report)
  }
  fit
})

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * LOGISTIC ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
        selected = "score"
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
        selected = "score"
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
match = match_logistic, puri = puri_logistic)

DIF_logistic <- reactiveValues(type = NULL,
                               correction = NULL,
                               purification = NULL,
                               matching = NULL)

# ** Updating type ######
observeEvent(input$DIF_logistic_summary_type, {
  DIF_logistic$type <- input$DIF_logistic_summary_type
})
observeEvent(input$DIF_logistic_items_type, {
  DIF_logistic$type <- input$DIF_logistic_items_type
})
observeEvent(DIF_logistic$type, {
  if (DIF_logistic$type != input$DIF_logistic_summary_type) {
    updateCheckboxGroupInput(session = session,
                             inputId = "DIF_logistic_summary_type",
                             selected = DIF_logistic$type)
  }
  if (DIF_logistic$type != input$DIF_logistic_items_type) {
    updateCheckboxGroupInput(session = session,
                             inputId = "DIF_logistic_items_type",
                             selected = DIF_logistic$type)
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
    updateSelectInput(session = session,
                      inputId = "DIF_logistic_summary_correction",
                      selected = DIF_logistic$correction)
  }
  if (DIF_logistic$correction != input$DIF_logistic_items_correction) {
    updateSelectInput(session = session,
                      inputId = "DIF_logistic_items_correction",
                      selected = DIF_logistic$correction)
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
    updateCheckboxInput(session = session,
                        inputId = "DIF_logistic_summary_purification",
                        value = DIF_logistic$purification)
  }
  if (DIF_logistic$purification != input$DIF_logistic_items_purification) {
    updateCheckboxInput(session = session,
                        inputId = "DIF_logistic_items_purification",
                        value = DIF_logistic$purification)
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
    updateCheckboxInput(session = session,
                        inputId = "DIF_logistic_summary_matching",
                        value = DIF_logistic$matching)
  }
  if (DIF_logistic$matching != input$DIF_logistic_items_matching) {
    updateCheckboxInput(session = session,
                        inputId = "DIF_logistic_items_matching",
                        value = DIF_logistic$matching)
  }
})

# ** Updating item slider ######
observe({
  item_count = ncol(binary())
  updateSliderInput(session = session,
                    inputId = "DIF_logistic_items_item",
                    max = item_count)
})

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

  fit <- tryCatch(.difLogistic_edited(Data = data, group = group, match = match, focal.name = 1,
                                      type = input$DIF_logistic_summary_type,
                                      p.adjust.method = input$DIF_logistic_summary_correction,
                                      purify = input$DIF_logistic_summary_purification),
                  error = function(e) e)

  validate(need(class(fit) == "Logistic",
                paste0('This method cannot be used on this data. Error returned: ', fit$message)))

  fit
})

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** SUMMARY ######

# ** Output print ######
output$DIF_logistic_summary_print <- renderPrint({
  print(DIF_logistic_model())
})

# ** Warning for missing values ####
output$DIF_logistic_summary_NA_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

  g <- plotDIFLogistic(fit, item = item, match = match, item.name = item_names()[item],
                       Data = data, group = group)
  g
})
output$DIF_logistic_items_plot <- renderPlot({
  DIF_logistic_items_plot()
})

# ** DB for plot ######
output$DB_DIF_logistic_items_plot <- downloadHandler(
  filename =  function() {
    paste0("fig_DIF_logistic_", item_names()[input$DIF_logistic_items_item], ".png")
  },
  content = function(file) {
    ggsave(file, plot = DIF_logistic_items_plot() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# ** Table with coefficients ######
output$DIF_logistic_items_coef_tab <- renderTable({

  fit <- DIF_logistic_model()
  item <- input$DIF_logistic_items_item

  tab_coef <- fit$logitPar[item, ]
  tab_sd <- fit$logitSe[item, ]

  tab <- data.frame(tab_coef, tab_sd)
  rownames(tab) <- c('%%mathit{b}_0%%', '%%mathit{b}_1%%', '%%mathit{b}_2%%', '%%mathit{b}_3%%')
  colnames(tab) <- c("Estimate", "SD")

  tab
},
include.rownames = T,
include.colnames = T)

# ** Warning for missing values ####
output$DIF_logistic_items_NA_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** REPORT ######

# ** Model for report ######
report_DIF_logistic_model <- reactive({
  group <- unlist(group())
  data <- data.frame(binary())

  if (!input$customizeCheck) {
    fit <- DIF_logistic_model()
  } else {
    type_report = input$type_print_DIF_logistic_report
    correction_report = input$correction_method_log_report
    purify_report = input$puri_LR_report

    fit <- .difLogistic_edited(Data = data, group = group, focal.name = 1,
                               type = type_report,
                               p.adjust.method = correction_report,
                               purify = purify_report)
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
                           Data = data, group = group)
      g <- g + ggtitle(paste0("DIF logistic plot for ", item_names()[item])) +
        theme(text = element_text(size = 12),
              plot.title = element_text(size = 12, face = "bold"))
      graflist[[i]] <- g
      i <- i + 1
    }
  } else {
    graflist <- NULL
  }
  graflist
})

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * NLR DIF ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
match = match_NLR, puri = puri_NLR)

# ** Model for print ######
model_DIF_NLR_print <- reactive({
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

  fit <- tryCatch(difNLR(Data = data, group = group, focal.name = 1, match = match,
                         model = model, type = type,
                         p.adjust.method = adj.method, purify = purify,
                         test = "LR"),
                  error = function(e) e)

  validate(need(class(fit) == "difNLR",
                paste0('This method cannot be used on this data. Error returned: ', fit$message)))

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

observeEvent(input$DIF_NLR_summary_matching,{
  if (all(input$DIF_NLR_items_matching != input$DIF_NLR_summary_matching)){
    updateSelectInput(session = session,
                      inputId = "DIF_NLR_items_matching",
                      selected = input$DIF_NLR_summary_matching)
  }
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

observeEvent(input$DIF_NLR_items_matching,{
  if (all(input$DIF_NLR_items_matching != input$DIF_NLR_summary_matching)){
    updateSelectInput(session = session,
                      inputId = "DIF_NLR_summary_matching",
                      selected = input$DIF_NLR_items_matching)
  }
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

  if (input$DIF_NLR_summary_matching == "zscore") {
    match <- "zscore"
  } else if (input$DIF_NLR_summary_matching == "zuploaded") {
    match <- scale(apply(as.data.frame(unlist(DIFmatching())), 1, sum))
  }

  fit <- tryCatch(difNLR(Data = data, group = group, focal.name = 1, match = match,
                         model = model, type = type,
                         p.adjust.method = adj.method, purify = purify,
                         test = "LR"),
                  error = function(e) e)

  validate(need(class(fit) == "difNLR",
                paste0('This method cannot be used on this data. Error returned: ', fit$message)))

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

# ** DB for plot ######
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

  fit <- tryCatch(switch(input$type_plot_DIF_IRT_lord,
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
                                         purify = input$puri_Lord_plot)),
                  error = function(e) e)

  validate(need(class(fit) == 'Lord',
                paste0('This method cannot be used on this data. Error returned: ', fit$message)))
  fit
})

# ** Model for print ######
model_DIF_IRT_Lord_print <- reactive({
  group <- unlist(group())
  data <- data.frame(binary())

  if (input$type_print_DIF_IRT_lord == "3PL"){
    guess <- itemPar3PL(data)[, 3]
  }

  fit <- tryCatch(switch(input$type_print_DIF_IRT_lord,
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
                                         purify = input$puri_Lord)),
                  error = function(e) e)

  validate(need(class(fit) == 'Lord',
                paste0('This method cannot be used on this data. Error returned: ', fit$message)))
  fit
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

# ** DB for plot ######
output$DP_plot_DIF_IRT_Lord <- downloadHandler(
  filename =  function() {
    paste("fig_DIFIRTLord_",item_names()[input$difirt_lord_itemSlider], ".png", sep = "")
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

  fit <- tryCatch(switch(input$type_plot_DIF_IRT_raju,
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
                                         purify = input$puri_Raju_plot)),
                  error = function(e) e)

  validate(need(class(fit) == 'Raj',
                paste0('This method cannot be used on this data. Error returned: ', fit$message)))
  fit
})

# ** Model for print ######
model_DIF_IRT_Raju_print <- reactive({
  group <- unlist(group())
  data <- data.frame(binary())

  if (input$type_print_DIF_IRT_raju == "3PL"){
    guess <- itemPar3PL(data)[, 3]
  }

  fit <- tryCatch(switch(input$type_print_DIF_IRT_raju,
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
                                         purify = input$puri_Raju)),
                  error = function(e) e)

  validate(need(class(fit) == 'Raj',
                paste0('This method cannot be used on this data. Error returned: ', fit$message)))
  fit
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
    paste0("fig_DIFIRTRaju_", item_names()[input$difirt_raju_itemSlider], ".png")
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
  data <- data.frame(binary())

  # inputs
  type <- input$DIF_SIBTEST_type
  purify <- input$DIF_SIBTEST_purification
  adj.method <- input$DIF_SIBTEST_correction

  # model
  fit <- .difSIBTEST_edited(Data = data, group = group, focal.name = 1,
                            type = type,
                            purify = purify, p.adjust.method = adj.method)
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

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * METHOD COMPARISON ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output$method_comparison_table <- renderTable({

  group <- group()

  l_methods <- list()
  l_methods[['Delta']] <- try(deltaGpurn()$DIFitems)
  l_methods[['MH']] <- try(DIF_MH_model()$DIFitems) #♠ repaired to be compatible
  l_methods[['LOG']] <- try(DIF_logistic_model()$DIFitems)
  l_methods[['NLR']] <- try(model_DIF_NLR_print()$DIFitems)
  l_methods[['IRT']] <- try(model_DIF_IRT_Lord_print()$DIFitems)
  l_methods[['RAJU']] <- try(model_DIF_IRT_Raju_print()$DIFitems)
  l_methods[['SIBTEST']] <- try(DIF_SIBTEST_model()$DIFitems)
  # l_methods[['DFF']] <- try(DDF_multi_model()$DDFitems)

  k <- length(item_names())
  idx <- lapply(l_methods, class)
  idx <- which(unlist(idx) != 'try-error')

  v <- matrix(NA, ncol = length(l_methods), nrow = k)
  v[, idx] <- 0

  # there is need to handle Delta method and DDF differently
  for (j in idx) {
    if (names(l_methods)[j] == "Delta"){
      if (all(l_methods[[j]] != 'no DIF item detected')) v[as.numeric(paste(l_methods[[j]])), j] <- 1
    } else {
      # if (names(l_methods)[j] == "DDF"){
      #   if (all(l_methods[[j]] != 'No DDF item detected')) v[as.numeric(paste(l_methods[[j]])), j] <- 1
      # } else {
      if (all(l_methods[[j]] != 'No DIF item detected')) v[as.numeric(paste(l_methods[[j]])), j] <- 1
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
include.colnames = T)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * CUMULATIVE ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
match = match_cum, puri = puri_cum)

DIF_cum <- reactiveValues(type = NULL,
                          correction = NULL,
                          purification = NULL,
                          matching = NULL)

# ** Updating type ######
observeEvent(input$DIF_cum_type_summary, {
  DIF_cum$type <- input$DIF_cum_type_summary
})
observeEvent(input$DIF_cum_type_items, {
  DIF_cum$type <- input$DIF_cum_type_items
})
observeEvent(DIF_cum$type, {
  if (DIF_cum$type != input$DIF_cum_type_summary) {
    updateCheckboxGroupInput(session = session,
                             inputId = "DIF_cum_type_summary",
                             selected = DIF_cum$type)
  }
  if (DIF_cum$type != input$DIF_cum_type_items) {
    updateCheckboxGroupInput(session = session,
                             inputId = "DIF_cum_type_items",
                             selected = DIF_cum$type)
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
    updateSelectInput(session = session,
                      inputId = "DIF_cum_correction_summary",
                      selected = DIF_cum$correction)
  }
  if (DIF_cum$correction != input$DIF_cum_correction_items) {
    updateSelectInput(session = session,
                      inputId = "DIF_cum_correction_items",
                      selected = DIF_cum$correction)
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
    updateCheckboxInput(session = session,
                        inputId = "DIF_cum_purification_summary",
                        value = DIF_cum$purification)
  }
  if (DIF_cum$purification != input$DIF_cum_purification_items) {
    updateCheckboxInput(session = session,
                        inputId = "DIF_cum_purification_items",
                        value = DIF_cum$purification)
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
    updateCheckboxInput(session = session,
                        inputId = "DIF_cum_summary_matching",
                        value = DIF_cum$matching)
  }
  if (DIF_cum$matching != input$DIF_cum_items_matching) {
    updateCheckboxInput(session = session,
                        inputId = "DIF_cum_items_matching",
                        value = DIF_cum$matching)
  }
})

# ** Updating item slider ######
observe({
  item_count = ncol(ordinal())
  updateSliderInput(session = session,
                    inputId = "DIF_cum_items",
                    max = item_count)
})

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

  fit <- difORD(data, group, focal.name = 1, model = "cumulative", match = match,
                type = type, purify = puri, p.adjust.method = corr,
                parametrization = "classic")
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
  txt2 <- paste0("b_{0ik} + b_{i1} ", txt1, " + b_{i2} G_p + b_{i3} ", txt1, " G_p")
  txt3 <- paste0(txt1, ", b_{i0k}, b_{i1}, b_{i2}, b_{i3}")

  txt <- paste0("$$P(Y_{ip} \\geq k|", txt3, ") = \\frac{e^{", txt2, "}}{1 + e^{", txt2, "}}$$")

  txt
})

output$DIF_cum_equation1_summary <- renderUI({
  withMathJax(HTML(DIF_cum_equation1_summary_Input()))
})

# ** Equation - category probability ######
DIF_cum_equation2_summary_Input <- reactive({
  # txt1 <- ifelse(input$DIF_cum_matching_summary == "score", "X_p", "Z_p")
  txt1 <- "Z_p"
  txt3 <- paste0(txt1, ", b_{i0k}, b_{i1}, b_{i2}, b_{i3}")
  txt4 <- paste0(txt1, ", b_{i0k+1}, b_{i1}, b_{i2}, b_{i3}")

  txt <- paste0("$$P(Y_{ip} = k|", txt3, ") = P(Y_{ip} \\geq k|", txt3, ") - P(Y_{ip} \\geq k + 1|", txt4, ")$$")

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

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** ITEMS ######

# ** Plot - cumulative ######
DIF_cum_plot_cumulative_Input <- reactive({
  fit <- DIF_cum_model()
  item <- input$DIF_cum_items

  g <- plot(fit, item = item, plot.type = "cumulative")[[1]] +
    theme_app() +
    ggtitle(item_names()[item]) +
    theme(legend.box.just = "top",
          legend.justification = c("right", "bottom"),
          legend.position = c(0.98, 0.02),
          legend.box = "horizontal",
          legend.margin = margin(0, 0, 0, 0, unit = "cm"))
  g
})

output$DIF_cum_plot_cumulative <- renderPlot({
  DIF_cum_plot_cumulative_Input()
})

# ** DB for plot - cumulative ######
output$DB_DIF_cum_plot_cumulative <- downloadHandler(
  filename =  function() {
    paste0("fig_DIF_cum_cumulative_", item_names()[input$DIF_cum_items], ".png")
  },
  content = function(file) {
    ggsave(file,
           plot = DIF_cum_plot_cumulative_Input() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height,
           width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# ** Plot - category ######
DIF_cum_plot_category_Input <- reactive({
  fit <- DIF_cum_model()
  item <- input$DIF_cum_items

  g <- plot(fit, item = item, plot.type = "category")[[1]] +
    theme_app() +
    ggtitle(item_names()[item]) +
    theme(legend.box.just = "top",
          legend.justification = c("left", "top"),
          legend.position = c(0.02, 0.98),
          legend.box = "horizontal",
          legend.margin = margin(0, 0, 0, 0, unit = "cm"))
  g
})

output$DIF_cum_plot_category <- renderPlot({
  DIF_cum_plot_category_Input()
})

# ** DB for plot - cumulative ######
output$DB_DIF_cum_plot_category <- downloadHandler(
  filename =  function() {
    paste0("fig_DIF_cum_category_", item_names()[input$DIF_cum_items], ".png")
  },
  content = function(file) {
    ggsave(file,
           plot = DIF_cum_plot_category_Input() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height,
           width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# ** Table of coefficients ######
DIF_cum_coef_tab_Input <- reactive({
  fit <- DIF_cum_model()
  item <- input$DIF_cum_items
  cat <- sort(unique(data.frame(ordinal())[, item]))[-1]

  tab <- matrix(0, nrow = length(cat) + 3, ncol = 2)
  rownames(tab) <- c(paste0("(Intercept):", 1:length(cat)), "x", "group", "x:group")
  colnames(tab) <- c("estimate", "SE")

  tmp <- t(coef(fit, SE = T)[[item]])
  tab[rownames(tmp), ] <- tmp
  colnames(tab) <- c("Estimate", "SE")
  rownames(tab) <- c(paste0("%%mathit{b}_{0", cat, "}%%"),
                     "%%mathit{b}_{1}%%",
                     "%%mathit{b}_{2}%%",
                     "%%mathit{b}_{3}%%")

  tab
})

output$DIF_cum_coef_tab <- renderTable({
  DIF_cum_coef_tab_Input()
},
include.rownames = T,
include.colnames = T)

# ** Equation - cumulative probability ######
DIF_cum_equation1_items_Input <- reactive({
  # txt1 <- ifelse(input$DIF_cum_matching_summary == "score", "X_p", "Z_p")
  txt1 <- "Z_p"
  txt2 <- paste0("b_{0k} + b_{1} ", txt1, " + b_{2} G_p + b_{3} ", txt1, " G_p")
  txt3 <- paste0(txt1, ", b_{0k}, b_{1}, b_{2}, b_{3}")

  txt <- paste0("$$P(Y_{p} \\geq k|", txt3, ") = \\frac{e^{", txt2, "}}{1 + e^{", txt2, "}}$$")

  txt
})

output$DIF_cum_equation1_items <- renderUI({
  withMathJax(HTML(DIF_cum_equation1_items_Input()))
})

# ** Equation - category probability ######
DIF_cum_equation2_items_Input <- reactive({
  # txt1 <- ifelse(input$DIF_cum_matching_summary == "score", "X_p", "Z_p")
  txt1 <- "Z_p"
  txt3 <- paste0(txt1, ", b_{0k}, b_{1}, b_{2}, b_{3}")
  txt4 <- paste0(txt1, ", b_{0k+1}, b_{1}, b_{2}, b_{3}")

  txt <- paste0("$$P(Y_{p} = k|", txt3, ") = P(Y_{p} \\geq k|", txt3, ") - P(Y_{p} \\geq k + 1|", txt4, ")$$")

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

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * ADJACENT ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
match = match_adj, puri = puri_adj)

DIF_adj <- reactiveValues(type = NULL,
                          correction = NULL,
                          purification = NULL,
                          matching = NULL)

# ** Updating type ######
observeEvent(input$DIF_adj_type_summary, {
  DIF_adj$type <- input$DIF_adj_type_summary
})
observeEvent(input$DIF_adj_type_items, {
  DIF_adj$type <- input$DIF_adj_type_items
})
observeEvent(DIF_adj$type, {
  if (DIF_adj$type != input$DIF_adj_type_summary) {
    updateCheckboxGroupInput(session = session,
                             inputId = "DIF_adj_type_summary",
                             selected = DIF_adj$type)
  }
  if (DIF_adj$type != input$DIF_adj_type_items) {
    updateCheckboxGroupInput(session = session,
                             inputId = "DIF_adj_type_items",
                             selected = DIF_adj$type)
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
    updateCheckboxInput(session = session,
                        inputId = "DIF_adj_summary_matching",
                        value = DIF_adj$matching)
  }
  if (DIF_adj$matching != input$DIF_adj_items_matching) {
    updateCheckboxInput(session = session,
                        inputId = "DIF_adj_items_matching",
                        value = DIF_adj$matching)
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
    updateSelectInput(session = session,
                      inputId = "DIF_adj_correction_summary",
                      selected = DIF_adj$correction)
  }
  if (DIF_adj$correction != input$DIF_adj_correction_items) {
    updateSelectInput(session = session,
                      inputId = "DIF_adj_correction_items",
                      selected = DIF_adj$correction)
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
    updateCheckboxInput(session = session,
                        inputId = "DIF_adj_purification_summary",
                        value = DIF_adj$purification)
  }
  if (DIF_adj$purification != input$DIF_adj_purification_items) {
    updateCheckboxInput(session = session,
                        inputId = "DIF_adj_purification_items",
                        value = DIF_adj$purification)
  }
})

# ** Updating item slider ######
observe({
  item_count = ncol(ordinal())
  updateSliderInput(session = session,
                    inputId = "DIF_adj_items",
                    max = item_count)
})

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

  fit <- difORD(data, group, focal.name = 1, model = "adjacent", match = match,
                type = type, purify = puri, p.adjust.method = corr,
                parametrization = "classic")
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
  txt3 <- paste0(txt1, ", b_{0i1}, ..., b_{0iK}, b_{i1}, b_{i2}, b_{i3}")

  txt <- paste0("$$P(Y_{ip} = k|", txt3, ") = \\frac{e^{\\sum_{t = 0}^{k}", txt2, "}}{\\sum_{r = 0}^{K}e^{\\sum_{t = 0}^{r}", txt2, "}}$$")

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

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** ITEMS ######

# ** Plot ######
DIF_adj_plot_Input <- reactive({
  fit <- DIF_adj_model()
  item <- input$DIF_adj_items

  g <- plot(fit, item = item)[[1]] +
    theme_app() +
    ggtitle(item_names()[item]) +
    theme(legend.box.just = "top",
          legend.justification = c("left", "top"),
          legend.position = c(0.02, 0.98),
          legend.box = "horizontal",
          legend.margin = margin(0, 0, 0, 0, unit = "cm"))
  g
})

output$DIF_adj_plot <- renderPlot({
  DIF_adj_plot_Input()
})

# ** DB for plot ######
output$DB_DIF_adj_plot <- downloadHandler(
  filename =  function() {
    paste0("fig_DIF_adj_", item_names()[input$DIF_adj_items], ".png")
  },
  content = function(file) {
    ggsave(file,
           plot = DIF_adj_plot_Input() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height,
           width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# ** Table of coefficients ######
DIF_adj_coef_tab_Input <- reactive({
  fit <- DIF_adj_model()
  item <- input$DIF_adj_items
  cat <- sort(unique(data.frame(ordinal())[, item]))[-1]

  tab <- matrix(0, nrow = length(cat) + 3, ncol = 2)
  rownames(tab) <- c(paste0("(Intercept):", 1:length(cat)), "x", "group", "x:group")
  colnames(tab) <- c("estimate", "SE")

  tmp <- t(coef(fit, SE = T)[[item]])
  tab[rownames(tmp), ] <- tmp
  colnames(tab) <- c("Estimate", "SE")
  rownames(tab) <- c(paste0("%%mathit{b}_{0", cat, "}%%"), "%%mathit{b}_{1}%%",
                     "%%mathit{b}_{2}%%", "%%mathit{b}_{3}%%")

  tab
})

output$DIF_adj_coef_tab <- renderTable({
  DIF_adj_coef_tab_Input()
},
include.rownames = T,
include.colnames = T)

# ** Equation ######
DIF_adj_equation_items_Input <- reactive({
  # txt1 <- ifelse(input$DIF_adj_matching_summary == "score", "X_p", "Z_p")
  txt1 <- "Z_p"
  txt2 <- paste0("b_{0t} + b_{1} ", txt1, " + b_{2} G_p + b_{3} ", txt1, " G_p")
  txt3 <- paste0(txt1, ", b_{01}, ..., b_{0K}, b_{1}, b_{2}, b_{3}")

  txt <- paste0("$$P(Y_{p} = k|", txt3, ") = \\frac{e^{\\sum_{t = 0}^{k}", txt2, "}}{\\sum_{r = 0}^{K}e^{\\sum_{t = 0}^{r}", txt2, "}}$$")

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

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * MULTINOMIAL ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
match = match_multi, puri = puri_multi)

DDF_multi <- reactiveValues(type = NULL,
                            correction = NULL,
                            purification = NULL,
                            matching = NULL)

# ** Updating type ######
observeEvent(input$DDF_multi_type_summary, {
  DDF_multi$type <- input$DDF_multi_type_summary
})
observeEvent(input$DDF_multi_type_items, {
  DDF_multi$type <- input$DDF_multi_type_items
})
observeEvent(DDF_multi$type, {
  if (DDF_multi$type != input$DDF_multi_type_summary) {
    updateCheckboxGroupInput(session = session,
                             inputId = "DDF_multi_type_summary",
                             selected = DDF_multi$type)
  }
  if (DDF_multi$type != input$DDF_multi_type_items) {
    updateCheckboxGroupInput(session = session,
                             inputId = "DDF_multi_type_items",
                             selected = DDF_multi$type)
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
    updateSelectInput(session = session,
                      inputId = "DDF_multi_correction_summary",
                      selected = DDF_multi$correction)
  }
  if (DDF_multi$correction != input$DDF_multi_correction_items) {
    updateSelectInput(session = session,
                      inputId = "DDF_multi_correction_items",
                      selected = DDF_multi$correction)
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
    updateCheckboxInput(session = session,
                        inputId = "DIF_multi_summary_matching",
                        value = DDF_multi$matching)
  }
  if (DDF_multi$matching != input$DIF_multi_items_matching) {
    updateCheckboxInput(session = session,
                        inputId = "DIF_multi_items_matching",
                        value = DDF_multi$matching)
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
    updateCheckboxInput(session = session,
                        inputId = "DDF_multi_purification_summary",
                        value = DDF_multi$purification)
  }
  if (DDF_multi$purification != input$DDF_multi_purification_items) {
    updateCheckboxInput(session = session,
                        inputId = "DDF_multi_purification_items",
                        value = DDF_multi$purification)
  }
})

# ** Updating item slider ######
observe({
  item_count = ncol(ordinal())
  updateSliderInput(session = session,
                    inputId = "DDF_multi_items",
                    max = item_count)
})

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

  fit <- tryCatch(ddfMLR(Data = data, group = group, focal.name = 1, match = match,
                         key = key, p.adjust.method = corr,
                         type = type, purify = puri, parametrization = "classic"),
                  error = function(e) e)

  validate(need(class(fit) == 'ddfMLR',
                paste0('This method cannot be used on this data. Error returned: ', fit$message)))

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

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** ITEMS ######

# ** Plot ######
DDF_multi_plot_Input <- reactive({
  fit <- DDF_multi_model()
  item <- input$DDF_multi_items

  g <- plot(fit, item = item)[[1]] +
    theme_app() +
    ggtitle(item_names()[item]) +
    theme(legend.box.just = "top",
          legend.justification = c("left", "top"),
          legend.position = c(0.02, 0.98),
          legend.box = "horizontal",
          legend.margin = margin(0, 0, 0, 0, unit = "cm"))
  g
})

# ** Output plot ######
output$DDF_multi_plot <- renderPlot({
  DDF_multi_plot_Input()
})

# ** DB for plot ######
output$DB_DDF_multi_plot <- downloadHandler(
  filename =  function() {
    paste0("fig_DDF_multi_", item_names()[input$DDF_multi_items], ".png")
  },
  content = function(file) {
    ggsave(file,
           plot = DDF_multi_plot_Input() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height,
           width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# ** Table of coefficients ######
output$DDF_multi_coef_tab <- renderTable({
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
  rownames(tab_se) <- rownames(tab_coef) <- nams

  tab_coef <- data.frame(tab_coef, answ = rownames(tab_coef))
  tab_se <- data.frame(tab_se, answ = rownames(tab_se))

  df1 <- melt(tab_coef, id = "answ")
  df2 <- melt(tab_se, id = "answ")
  tab <- data.frame(df1$value,
                    df2$value)
  rownames(tab) <- paste0('%%mathit{', substr(df1$variable, 1, 1), '}_{',
                          df1$answ,
                          substr(df1$variable, 2, 2), '}%%')
  colnames(tab) <- c("Estimate", "SD")
  tab

},
include.rownames = T)

# ** Equation ######
output$DDF_multi_equation_items <- renderUI ({
  item <- input$DDF_multi_items
  key <- key()

  cor_option <- key[item]
  withMathJax(
    sprintf(
      '$$\\text{For item } %s \\text{ are corresponding equations of multinomial model given by: } \\\\
      \\mathrm{P}(Y_{p} = %s|Z_p, G_p, b_{l0}, b_{l1}, b_{l2}, b_{l3}, l = 1, \\dots, K-1) =
      \\frac{1}{1 + \\sum_l e^{\\left( b_{l0} + b_{l1} Z_p + b_{l2} G_p + b_{l3} Z_p:G_p\\right)}}, \\\\
      \\mathrm{P}(Y_{p} = k|Z_p, G_p, b_{l0}, b_{l1}, b_{l2}, b_{l3}, l = 1, \\dots, K-1) =
      \\frac{e^{\\left( b_{k0} + b_{k1} Z_p + b_{k2} G_p + b_{k3} Z_p:G_p\\right)}}
      {1 + \\sum_l e^{\\left( b_{l0} + b_{l1} Z_p + b_{l2} G_p + b_{l3} Z_p:G_p\\right)}}, \\\\
      \\text{where } %s \\text{ is the correct answer and } k \\text{ is one of the wrong options. }$$',
      item, cor_option, cor_option, cor_option, cor_option
    )
  )
})

# ** Warning for missing values ####
output$DDF_multi_NA_warning_items <- renderUI({
  txt <- na_score()
  HTML(txt)
})

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

    fit <- tryCatch(ddfMLR(Data = data, group = group, focal.name = 1, #match = match,
                           key = key, p.adjust.method = corr,
                           type = type, purify = puri, parametrization = "classic"),
                    error = function(e) e)

    validate(need(class(fit) == 'ddfMLR',
                  paste0('This method cannot be used on this data. Error returned: ', fit$message)))
  }

  fit
})

# ** Plot for report ######
DDF_multi_plot_report <- reactive({
  fit <- DDF_multi_model_report()

  graflist <- list()
  if (fit$DDFitems[[1]] != "No DDF item detected"){
    for (i in 1:length(fit$DDFitems)) {
      g <- plot(fit, item = fit$DDFitems[i])[[1]] +
        theme(text = element_text(size = 12),
              plot.title = element_text(size = 12, face = "bold", vjust = 1.5)) +
        ggtitle(paste("\nDDF multinomial plot for item ", item_numbers()[fit$DDFitems[i]]))
      graflist[[i]] <- g
    }
  } else {
    graflist <- NULL
  }
  graflist
})

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

  ccirt <- function(theta, a, b, c, d){
    return(c + (d - c)/(1 + exp(-a*(theta - b))))
  }

  probR <- ccirt(theta0, a = aR, b = bR, c = cR, d = dR)
  probF <- ccirt(theta0, a = aF, b = bF, c = cF, d = dF)

  df <- data.frame(Reference = ccirt(seq(-4, 4, 0.01), aR, bR, cR, dR),
                   Focal = ccirt(seq(-4, 4, 0.01), aF, bF, cF, dF),
                   Ability = seq(-4, 4, 0.01))
  df <- melt(df, id.vars = "Ability", variable.name = "Group", value.name = "Probability")

  g <- ggplot(data = df, aes(x = Ability, y = Probability, col = Group, linetype = Group)) +
    geom_line(size = 0.8) +
    geom_segment(aes(y = probR, yend = probR,
                     x = -4, xend = theta0),
                 color = "gray", linetype = "dotdash") +
    geom_segment(aes(y = probF, yend = probF,
                     x = -4, xend = theta0),
                 color = "gray", linetype = "dotdash") +
    geom_segment(aes(y = 0,
                     yend = max(probR, probF),
                     x = theta0, xend = theta0),
                 color = "gray", linetype = "dotdash") +
    xlim(-4, 4) +
    xlab("Ability") +
    ylab("Probability of correct answer") +
    ylim(0, 1) +
    scale_color_manual(name = "",
                       values = c("blue", "#e6b800"),
                       labels = c(paste(paste(letters[1:2], "=", c(aR, bR)),
                                        collapse = ", "),
                                  paste(paste(paste(letters[1:2], "=", c(aF, bF))),
                                        collapse = ", "))) +
    scale_linetype_manual(name = "",
                          values = c("solid", "dashed"),
                          labels = c(paste(paste(letters[1:2], "=", c(aR, bR)),
                                           collapse = ", "),
                                     paste(paste(paste(letters[1:2], "=", c(aF, bF))),
                                           collapse = ", "))) +
    theme_app()
  g

})
# ** Plotly for training ######
output$DIF_training_plot <- renderPlotly({
  g <- DIF_training_plot_Input()

  p <- ggplotly(g)
  theta0 <- input$DIF_training_parameter_theta

  aR <- input$DIF_training_parameter_aR
  bR <- input$DIF_training_parameter_bR
  cR <- 0
  dR <- 1

  aF <- input$DIF_training_parameter_aF
  bF <- input$DIF_training_parameter_bF
  cF <- 0
  dF <- 1

  theta0 <- input$DIF_training_parameter_theta

  ccirt <- function(theta, a, b, c, d){
    return(c + (d - c)/(1 + exp(-a*(theta - b))))
  }

  probR <- ccirt(theta0, a = aR, b = bR, c = cR, d = dR)
  probF <- ccirt(theta0, a = aF, b = bF, c = cF, d = dF)

  # Reference group, probabilities
  text <- gsub("Group: Reference<br />Group: Reference", "Group: Reference", p$x$data[[1]]$text)
  p$x$data[[1]]$text <- text

  # Focal group, probabilities
  text <- gsub("Group: Focal<br />Group: Focal", "Group: Focal", p$x$data[[2]]$text)
  p$x$data[[2]]$text <- text

  # Reference group, selected theta
  text <- paste("Ability:     ", theta0, "<br />",
                "Probability: ", probR, "<br />",
                "Group:       ", "Reference")
  p$x$data[[3]]$text <- text

  # Focal group, selected theta
  text <- paste("Ability:     ", theta0, "<br />",
                "Probability: ", probF, "<br />",
                "Group:       ", "Focal")
  p$x$data[[4]]$text <- text

  # Selected theta
  text <- paste("Ability:     ", theta0, "<br />",
                "Probability: ", probR, "<br />",
                "Group:       ", "Reference", "<br />",
                "Probability: ", probF, "<br />",
                "Group:       ", "Focal")
  p$x$data[[5]]$text <- text

  p$elementId <- NULL

  p %>%  plotly::config(displayModeBar = F)
})

# ** DB for plot ######
output$DB_DIF_training_plot <- downloadHandler(
  filename =  function() {
    paste("fig_DIFtraining.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = DIF_training_plot_Input() +
             theme(legend.position = c(0.97, 0.03),
                   legend.justification = c(0.97, 0.03)) +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height,
           width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# ** Exercise 1 ######
# *** Correct answers for Exercise 1 ######
DIF_training_correct_answers_1 <- reactive({
  ccirt <- function(theta, a, b, c, d){
    return(1/(1 + exp(-a*(theta - b))))
  }
  # Exercise 1.1
  # correct parameters for reference and focal group
  aR <- 1; bR <- 0
  aF <- 1; bF <- 1
  parR <- c(aR, bR)
  parF <- c(aF, bF)

  # Exercise 1.3
  # probability calculation
  theta0 <- c(-2, 0, 2)

  probR <- ccirt(theta0, aR, bR)
  probF <- ccirt(theta0, aF, bF)

  correct_answers <- list(Ex1_1 = list(parR = parR,
                                       parF = parF),
                          Ex1_2 = "uniform",
                          Ex1_3 = list(probR = probR,
                                       probF = probF),
                          Ex1_4 = "reference")
  correct_answers
})

# ** Evaluation of answers for Exercise 1 ######
DIF_training_answers_check_1 <- eventReactive(input$DIF_training_1_submit, {
  correct_answers <- DIF_training_correct_answers_1()

  # Exercise 1.1
  aR <- input$DIF_training_parameter_aR; bR <- input$DIF_training_parameter_bR
  aF <- input$DIF_training_parameter_aF; bF <- input$DIF_training_parameter_bF
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

  check <- list(check1_1 = check1_1,
                check1_2 = check1_2,
                check1_3R = check1_3R,
                check1_3F = check1_3F,
                check1_4 = check1_4)
  res <- sum(sapply(check, sum))/sum(sapply(check, length))
  ans <- lapply(check, function(x) ifelse(is.na(x),
                                          "<b><font color = 'red'>!</font></b>",
                                          ifelse(x,
                                                 "<font color='green'>&#10004;</font>",
                                                 "<font color='red'>&#10006;</font>")))
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
                     paste0("<font color='red'>", round(100*res), "% correct. Try again.</font>"))))
})

# ** Exercise 2 ######
# *** Correct answers for Exercise 2 ######
DIF_training_correct_answers_2 <- reactive({
  ccirt <- function(theta, a, b, c, d){
    return(1/(1 + exp(-a*(theta - b))))
  }
  # Exercise 2.1
  # correct parameters for reference and focal group
  aR <- 0.8; bR <- -0.5
  aF <- 1.5; bF <- 1
  parR <- c(aR, bR)
  parF <- c(aF, bF)

  # Exercise 2.3
  # probability calculation
  theta0 <- c(-1, 0, 1)

  probR <- ccirt(theta0, aR, bR)
  probF <- ccirt(theta0, aF, bF)

  correct_answers <- list(Ex2_1 = list(parR = parR,
                                       parF = parF),
                          Ex2_2 = "nonuniform",
                          Ex2_3 = list(probR = probR,
                                       probF = probF),
                          Ex2_4 = "depends")
  correct_answers
})

# ** Evaluation of answers for Exercise 2 ######
DIF_training_answers_check_2 <- eventReactive(input$DIF_training_2_submit, {
  correct_answers <- DIF_training_correct_answers_2()

  # Exercise 2.1
  aR <- input$DIF_training_parameter_aR; bR <- input$DIF_training_parameter_bR
  aF <- input$DIF_training_parameter_aF; bF <- input$DIF_training_parameter_bF
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

  check <- list(check2_1 = check2_1,
                check2_2 = check2_2,
                check2_3R = check2_3R,
                check2_3F = check2_3F,
                check2_4 = check2_4)
  res <- sum(sapply(check, sum))/sum(sapply(check, length))
  ans <- lapply(check, function(x) ifelse(is.na(x),
                                          "<b><font color = 'red'>!</font></b>",
                                          ifelse(x,
                                                 "<font color='green'>&#10004;</font>",
                                                 "<font color='red'>&#10006;</font>")))
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
                     paste0("<font color='red'>", round(100*res), "% correct. Try again.</font>"))))
})
