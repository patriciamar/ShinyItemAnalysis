#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SUMMARY  ###########
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * TOTAL SCORES #####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Total scores summary table ######
totalscores_table_Input <- reactive({
  sc <- total_score()

  tab <- t(data.table(c(min(sc, na.rm = T),
                        max(sc, na.rm = T),
                        mean(sc, na.rm = T),
                        median(sc, na.rm = T),
                        sd(sc, na.rm = T),
                        skewness(sc, na.rm = T),
                        kurtosis(sc, na.rm = T))))
  colnames(tab) <- c("Min", "Max", "Mean", "Median", "SD", "Skewness", "Kurtosis")
  tab
})

# ** Output total scores summary table ######
output$totalscores_table <- renderTable({
  totalscores_table_Input()
},
digits = 2,
include.rownames = F,
include.colnames = T
)

# ** Histogram of total scores ######
totalscores_histogram_Input<- reactive({
  sc <- total_score()
  bin <- as.numeric(input$slider_totalscores_histogram)
  data <- binary()

  if (length(unique(c(min(sc, na.rm = T), bin - 1, bin, max(sc, na.rm = T)))) == 3 & bin != min(sc, na.rm = T)) {
    breaks <- unique(c(min(sc, na.rm = T), bin - 1, bin, bin + 1, max(sc, na.rm = T)))
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
    scale_x_continuous(limits = c(min(sc, na.rm = T) - 0.5, max(sc, na.rm = T) + 0.5)) +
    theme_app()
  g
})

# ** Output histogram of total scores ######
output$totalscores_histogram <- renderPlotly ({

  sc <- total_score()
  bin <- as.numeric(input$slider_totalscores_histogram)
  data <- binary()

  if (min(sc, na.rm = T) <= bin & bin <= max(sc, na.rm = T)){
    if (bin %in% unique(sc)) {
      breaks <- unique(c(min(sc, na.rm = T), bin - 1, bin, max(sc, na.rm = T)))
    } else {
      breaks <- unique(c(min(sc, na.rm = T), bin - 1, max(sc, na.rm = T)))
    }
  } else {
    breaks <- c(0, max(sc, na.rm = T))
  }

  df <- data.table(score = sc,
                   gr = cut(sc,
                            breaks = breaks,
                            include.lowest = T))

  g <- totalscores_histogram_Input()
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

# ** DB histogram of total scores ####
output$DB_totalscores_histogram <- downloadHandler(
  filename =  function() {
    paste("fig_TotalScores_histogram.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = totalscores_histogram_Input() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * STANDARD SCORES #####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Table for scores ######
scores_tables_Input <- reactive({
  sc <- total_score()
  k <- ifelse(is.character(key()), length(key()), sum(key()))

  # total score
  tosc <- sort(unique(sc))
  # percentile
  perc <- cumsum(prop.table(table(sc)))
  # succes rate
  sura <- (tosc / k) * 100
  # Z score
  zsco <- sort(unique(z_score()))
  # T score
  tsco <- 50 + 10 * zsco

  tab <- round(data.table(tosc, perc, sura, zsco, tsco), 2)
  colnames(tab) <- c("Total score", "Percentile", "Success rate", "Z-score", "T-score")

  tab
})

# ** Output table for scores ######
output$scores_tables <- renderTable({
  scores_tables_Input()
},
include.rownames = FALSE)

#exportTestValues(summ_ts = totalscores_table_Input(),
#                 summ_ss = scores_tables_Input())

# ** Download table with standard scores ** ####
output$download_standard_scores <- downloadHandler(
  filename = function() {
    paste("Standard_scores", ".csv", sep = "")
  },
  content = function(file) {
    data <- scores_tables_Input()
    write.csv(data, file)
  }
)
