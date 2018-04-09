#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SUMMARY  ###########
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * TOTAL SCORES #####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Total scores summary table ######
totalscores_table_Input <- reactive({
  sc <- scored_test()

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
  a <- test_answers()
  k <- test_key()
  sc <- scored_test()

  bin <- as.numeric(input$slider_totalscores_histogram)

  df <- data.table(sc,
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

  ggplot(df, aes(x = sc)) +
    geom_histogram(aes(fill = gr), binwidth = 1, color = "black") +
    scale_fill_manual("", breaks = df$gr, values = col) +
    labs(x = "Total score",
         y = "Number of students") +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(0, max(table(sc)) + 0.01 * nrow(a))) +
    scale_x_continuous(limits = c(-0.5, ncol(a) + 0.5)) +
    theme_shiny
})

# ** Output histogram of total scores ######
output$totalscores_histogram <- renderPlot ({
  totalscores_histogram_Input()
})

# ** DB histogram of total scores ####
output$DB_totalscores_histogram <- downloadHandler(
  filename =  function() {
    paste("fig_TotalScores_histogram.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = totalscores_histogram_Input() + theme(text = element_text(size = 10)),
           device = "png",
           height = 4, width = 8, dpi = 300)
  }
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * STANDARD SCORES #####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Table for scores ######
scores_tables_Input <- reactive({
  a  <- test_answers()
  k  <- test_key()
  sc <- scored_test()

  # total score
  tosc <- sort(unique(sc))
  # percentile
  perc <- cumsum(prop.table(table(sc)))
  # succes rate
  sura <- (tosc / length(k)) * 100
  # Z score
  zsco <- sort(unique(scale(sc)))
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
