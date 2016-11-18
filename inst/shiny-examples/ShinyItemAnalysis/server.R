######################
# GLOBAL LIBRARY #####
######################

library(CTT)
library(deltaPlotR)
library(difNLR)
library(difR)
library(foreign)
library(ggplot2)
library(gridExtra)
library(ltm)
library(moments)
library(nnet)
library(psych)
library(psychometric)
library(reshape2)

###########
# DATA ####
###########

data('GMAT', package = 'difNLR')
data('GMATtest', package = 'difNLR')
data('GMATkey', package = 'difNLR')
test=get("GMATtest")
key=get("GMATkey")

# * sandbox data choosing #####

#do.call(data, args=list(input$dataSelect, package="difNLR"))
#do.call(data, args=list(paste0(input$dataSelect,"key"), package="difNLR"))
#do.call(data, args=list(paste0(input$dataSelect,"test"), package="difNLR"))
#
#test=get(paste0(input$dataselect,"test"))
#key=get(paste0(input$dataSelect, "key"))

##################
# FUNCTIONS ######
##################

# Difficulty/Discrimination plot
source("DDplot.R")

# Distractors analysis
source("DistractorAnalysis.R")
source("plotDistractorAnalysis.R")

# DIF logistic regression plot
source("plotDIFLogistic.R")

# DIF IRT regression plot
source("plotDIFirt.R")




#####################
# SERVER SCRIPT #####
#####################

function(input, output, session) {

  ######################
  ### hits counter #####
  ######################
  output$counter <- renderText({
    if (!file.exists("counter.Rdata"))
      {counter <- 0}
    else {load(file = "counter.Rdata")}
    counter <- counter + 1
    save(counter, file = "counter.Rdata")
    paste0("Hits:", counter)
  })

  #########################
  # DATA ADJUSTMENT #######
  #########################

  # CHOOSE DATA #####

  output$dataSelect <- renderUI({
      selectInput("dataSelect", "Select dataset",
                c("GMAT" = "GMAT",
                  "GMAT2" = "GMAT2",
                  "Medical" = "difMedical"
                 ),
                selected="GMAT")
    })


  # LOAD ABCD DATA #####
  test_answers <- reactive ({
    do.call(data, args=list(paste0(input$dataSelect,"test"), package="difNLR"))
    test=get(paste0(input$dataSelect,"test"))

    ifelse (is.null(input$data), answ <- test[ , 1:20],
            answ <- read.csv(input$data$datapath, header = input$header,
                             sep = input$sep, quote = input$quote))
    answ
    })

  # LOAD KEY #####
  test_key <- reactive({
    do.call(data, args=list(paste0(input$dataSelect,"key"), package="difNLR"))
    key=get(paste0(input$dataSelect,"key"))

    ifelse (is.null(input$key), k <- key,
            {k <- read.csv(input$key$datapath, header = FALSE)
             k <- k[[1]]
            })
    k
    })

  # LOAD GROUPS #####
  DIF_groups <- reactive({
    do.call(data, args=list(paste0(input$dataSelect,"test"), package="difNLR"))
    test=get(paste0(input$dataSelect,"test"))

    ifelse (is.null(input$key), k <- test[ , 21],
            {k <- read.csv(input$groups$datapath,header = TRUE)
             k <- k[[1]]})
    as.vector(k)
    })

  # TOTAL SCORE CALCULATION #####
  scored_test <- reactive({
    sc <- score(test_answers(), test_key())$score
    sc
    })

  # CORRECT ANSWER CLASSIFICATION #####
  correct_answ <- reactive({
    correct <- score(test_answers(), test_key(), output.scored = TRUE)$scored
    correct
    })

  # DATA #####
   DPdata <- reactive ({
     dataset <- data.frame(correct_answ(), DIF_groups())
     colnames(dataset)[ncol(dataset)] <- 'group'
     dataset
     })

  # DATA HEAD ######
   output$headdata <- renderDataTable({

      test=test_answers()
      name <- c()
      for (i in 1:ncol(test)) {
        name[i] <- paste("i", i, sep = "")
      }
      colnames(test) <- name
      test

     }, options=list(scrollX=TRUE, pageLength=10))

  # KEY CONTROL #######
  output$key <- renderDataTable({

    key_table=as.data.frame(t(as.data.frame(test_key())))
    name <- c()
    for (i in 1:ncol(key_table)) {
      name[i] <- paste("i", i, sep = "")
    }
    colnames(key_table) <- name
    key_table

    }, options=list(scrollX=TRUE))

  # SCORE 0-1 #####
  output$sc01 <- renderDataTable({
    a <- test_answers()
    k <- test_key()

    sc <- data.frame(scored_test())
    colnames(sc) <- "Score"
    correct <- correct_answ()
    name <- c()
    for (i in 1:ncol(a)) {
      name[i] <- paste("i", i, sep = "")
    }
    colnames(correct) <- name

    out <- (cbind(correct,sc))
    out
    }, options=list(scrollX=TRUE, pageLength=10))

  # Item slider ####
  #output$slider <- renderUI({
  #  a <- test_answers()
  #
  #  sliderInput(
  #    "inSlider", "Item", animate = TRUE, min = 1, max = ncol(a), value = 1, step = 1
  #  )
  #})

  ##### ITEM SLIDERS #####
  #output$logregSlider <- renderUI({
  #  a <- test_answers()
  #  sliderInput("logregSlider", "Item", animate = TRUE, min = 1, max = ncol(a), value = 1, step = 1)
  #})
  output$distractorSlider <- renderUI({
    a <- test_answers()
    sliderInput("distractorSlider", "Item", animate = TRUE, min = 1, max = ncol(a), value = 1, step = 1)
  })

  output$logregSlider <- renderUI({
    a <- test_answers()
    sliderInput("logregSlider", "Item", animate = TRUE, min = 1, max = ncol(a), value = 1, step = 1)
  })

  output$zlogregSlider <- renderUI({
    a <- test_answers()
    sliderInput("zlogregSlider", "Item", animate = TRUE, min = 1, max = ncol(a), value = 1, step = 1)
  })
  output$zlogreg_irtSlider <- renderUI({
    a <- test_answers()
    sliderInput("zlogreg_irtSlider", "Item", animate = TRUE, min = 1, max = ncol(a), value = 1, step = 1)
  })
  output$nlsSlider <- renderUI({
    a <- test_answers()
    sliderInput("nlsSlider", "Item", animate = TRUE, min = 1, max = ncol(a), value = 1, step = 1)
  })
  output$multiSlider <- renderUI({
    a <- test_answers()
    sliderInput("multiSlider", "Item", animate = TRUE, min = 1, max = ncol(a), value = 1, step = 1)
  })

  output$diflogSlider <- renderUI({
    a <- test_answers()
    sliderInput("diflogSlider", "Item", animate = TRUE, min = 1, max = ncol(a), value = 1, step = 1)
  })
  output$diflog_irtSlider <- renderUI({
    a <- test_answers()
    sliderInput("diflog_irtSlider", "Item", animate = TRUE, min = 1, max = ncol(a), value = 1, step = 1)
  })
  output$difnlrSlider <- renderUI({
    a <- test_answers()
    sliderInput("difnlrSlider", "Item", animate = TRUE, min = 1, max = ncol(a), value = 1, step = 1)
  })

  output$difirt_lord_itemSlider <- renderUI({
    a <- test_answers()
    sliderInput("difirt_lord_itemSlider", "Item", animate = TRUE, min = 1, max = ncol(a), value = 1, step = 1)
  })

  output$difirt_raju_itemSlider <- renderUI({
    a <- test_answers()
    sliderInput("difirt_raju_itemSlider", "Item", animate = TRUE, min = 1, max = ncol(a), value = 1, step = 1)
  })


  ##### Slider update #####

  sliderUpdate=reactiveValues()

  observe({
    #invalidateLater(1000000, session)

    #if (isnull(input$logregSlider)) {updateSliderInput(session, "logregSlider", value = sliderUpdate$value)}

   #sliderUpdate$entryValues=c(input$inSlider, input$distractorSlider, input$logregSlider,input$zlogregSlider,input$zlogreg_irtSlider,input$nlsSlider, input$multiSlider,
     #                         input$diflogSlider, input$diflog_irtSlider, input$difnlrSlider)
   #sliderUpdate$value=as.numeric(names(table(isolate(sliderUpdate$entryValues))[min(table(isolate(sliderUpdate$entryValues)))==table(isolate(sliderUpdate$entryValues))]))

    #updateSliderInput(session, "inSlider", value = sliderUpdate$value)
    #updateSliderInput(session, "distractorSlider", value = sliderUpdate$value)
    #updateSliderInput(session, "logregSlider", value = sliderUpdate$value)
    #updateSliderInput(session, "zlogregSlider", value = sliderUpdate$value)
    #updateSliderInput(session, "zlogreg_irtSlider", value = sliderUpdate$value)
    #updateSliderInput(session, "nlsSlider", value = sliderUpdate$value)
    #updateSliderInput(session, "multiSlider", value = sliderUpdate$value)
    #updateSliderInput(session, "inSlider", value = sliderUpdate$value)
    #updateSliderInput(session, "diflogSlider", value = sliderUpdate$value)
    #updateSliderInput(session, "diflog_irtSlider", value = sliderUpdate$value)
    #updateSliderInput(session, "difnlrSlider", value = sliderUpdate$value)

   # sliderUpdate$outputValues=c(input$logregSlider,input$zlogregSlider,input$zlogreg_irtSlider,input$nlsSlider, input$multiSlider,
    #                           input$inSlider, input$diflogSlider, input$diflog_irtSlider, input$difnlrSlider)

  })

  ########################
  # SLIDER FOR STUDENTS PAGE ######
  ########################
  output$slider2 <- renderUI({
    sliderInput(
      "inSlider2", "Cut-Score", min = 0, max = ncol(test_answers()),
      value = round(median(scored_test())), step = 1
    )
  })

  # OTHER SLIDERS ####
  # * Mantel-Haenszel for score ####
  output$difMHSlider_score <- renderUI({
    sliderInput(
      "difMHSlider_score", "Cut-Score", min = 0, max = ncol(test_answers()),
      value = round(median(scored_test())), step = 1
    )
  })
  # * Mantel-Haenszel for item ####
  output$difMHSlider_item <- renderUI({
    a <- test_answers()
    sliderInput("difMHSlider_item", "Item", animate = TRUE,
                min = 1, max = ncol(a), value = 1, step = 1)
  })


  ########################
  # SUMMARY  ####
  ########################
  # * TOTAL SCORES #####
  # ** Summary table #####
  output$results <- renderTable({

    sc <- scored_test()

    tab <- t(data.frame(c(min(sc), max(sc), mean(sc), median(sc), sd(sc),
                          skewness(sc), kurtosis(sc))))
    colnames(tab) <- c("Min", "Max", "Mean", "Median", "SD", "Skewness", "Kurtosis")
    tab

    },
    digits = 2,
    include.rownames = F,
    include.colnames = T
    )

  # ** Histogram of Total Scores ######
  histogram_totalscores <- function()
    {
    a <- test_answers()
    k <- test_key()
    sc <- scored_test()

    bin <- as.numeric(input$inSlider2)

    df <- data.frame(sc,
                     gr = cut(sc,
                              breaks = unique(c(0, bin - 1, bin, ncol(a))),
                              include.lowest = T))

    if (bin < min(sc)){
      col <- "blue"
    } else {
      if (bin == min(sc)){
        col <- c("grey", "blue")
      } else {
        col <- c("red", "grey", "blue")
      }
    }

    ggplot(df, aes(x = sc)) +
      geom_histogram(aes(fill = gr), binwidth = 1, color = "black") +
      scale_fill_manual("", breaks = df$gr, values = col) +
      labs(x = "Total Score",
           y = "Number of Students") +
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
  }

  # ** output - Histogram of Total Scores ######
  output$histogram_totalscores <- renderPlot ({
    histogram_totalscores()
  })
  # ** download button - Histogram of Total Scores ####
  output$DP_histogram_totalscores <- downloadHandler(
      filename =  function() {
        paste("plot", input$name, ".png", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = histogram_totalscores(), device = "png")
      }
    )

  # * STANDARD SCORES #####
  # ** Table by Score ######
  output$percentile <- renderTable({

    a  <- test_answers()
    k  <- test_key()
    sc <- scored_test()

    # total score
    tosc <- sort(unique(sc))
    # percentile
    perc <- cumsum(prop.table(table(sc)))
    # succes rate
    sura <- (tosc / max(sc)) * 100
    # Z score
    zsco <- sort(unique(scale(sc)))
    # T score
    tsco <- 50 + 10 * zsco

    tab <- round(data.frame(tosc, perc, sura, zsco, tsco), 2)
    colnames(tab) <- c("Total Score", "Percentile", "Success Rate", "Z-score", "T-score")

    tab
  },
  include.rownames = FALSE)

  ############################
  # TRADITIONAL ANALYSIS #####
  ############################
  # * ITEM ANALYSIS #####
  # ** Item Difficulty/Discrimination Graph ######
  output$difplot <- renderPlot({

    correct <- correct_answ()
    DDplot(correct)

      })

  output$uidifplot <- renderUI({
    plotOutput("difplot", height = 1000)
    })

  # ** Cronbach's alpha ####
  output$cronbachalpha <- renderTable({
    correct <- correct_answ()
    tab <- c(psych::alpha(correct)$total[1], psych::alpha(correct)$total[8])
    tab <- as.data.frame(tab)
    colnames(tab) <- c("Estimate", "SD")
    tab
  },
  include.rownames = F,
  include.colnames = T)

  # ** Traditional Item Analysis Table #####
  output$itemexam <- renderTable({

    a <- test_answers()
    k <- test_key()
    correct <- correct_answ()

    alphadrop <- psych::alpha (correct)$alpha.drop[, 1]
    tab <- item.exam(correct, discr = TRUE)[, 1:5]
    tab <- cbind(c(1:ncol(test_answers())),
                 tab[, c(4, 1, 5, 2, 3)],
                 alphadrop)
    colnames(tab) <- c("Item", "Difficulty", "SD", "Discrimination ULI",
                       "Discrimination RIT", "Discrimination RIR", "Alpha Drop")
    tab

  },
  include.rownames = FALSE)


  # * DISTRACTORS #####
  # ** Distractor text #####
  output$text_distractor <- renderUI({

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

  # ** Distractor Analysis Table ######
  output$tab_distractor_by_group <- renderTable({
    sc <- scored_test()

    score.level <- quantile(sc, seq(0, 1, by = 1/input$gr))
    tab <- table(cut(sc,
                     score.level,
                     include.lowest = T,
                     labels = score.level[-1]))

    tab <- t(data.frame(tab))

    rownames(tab) <- c('Max Points', 'Count')
    colnames(tab) <- paste('Group', 1:input$gr)

    tab
  },
  include.colnames = TRUE,
  include.rownames = TRUE)



  # ** Distractors histograms by group #####
  output$hist_distractor_by_group <- renderPlot({
    a <- test_answers()
    k <- test_key()
    sc <- scored_test()

    df <- data.frame(sc,
                     gr = cut(sc, quantile(sc, seq(0, 1, by = 1/input$gr)),
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
      labs(x = "Total Score",
           y = "Number of Students") +
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

  # ** Distractors Plot #####
  output$graf <- renderPlot({
    a <- test_answers()
    k <- test_key()

    multiple.answers <- c(input$type_combinations_distractor == "Combinations")
    plotDistractorAnalysis(data = a, key = k, num.group = input$gr, item = input$distractorSlider,
                           multiple.answers = multiple.answers)
  })

  # ** Table with Counts ######
  output$tab_counts_distractor <- renderTable({
    a <- test_answers()
    k <- test_key()

    DA <- DistractorAnalysis(a, k, num.groups = input$gr)[[input$distractorSlider]]
    df <- dcast(as.data.frame(DA), response ~ score.level, sum, margins = T, value.var = "Freq")
    colnames(df) <- c("Response", paste("Group", 1:input$gr), "Total")
    levels(df$Response)[nrow(df)] <- "Total"
    df
  })

  # ** Table with Proportions #####
  output$tab_props_distractor <- renderTable({
    a <- test_answers()
    k <- test_key()

    DA <- DistractorAnalysis(a, k, num.groups = input$gr, p.table = TRUE)[[input$distractorSlider]]
    df <- dcast(as.data.frame(DA), response ~ score.level, sum, value.var = "Freq")
    colnames(df) <- c("Response", paste("Group", 1:input$gr))
    df
  })


  ######################
  # REGRESSION ####
  ######################


  # * LOGISTIC ####
  logistic_reg <- reactive ({
    model <- glm(correct_answ()[, input$logregSlider] ~ scored_test(), family = binomial)
  })
  # ** Plot with estimated logistic curve ####
  output$logreg <- renderPlot({
    sc <- scored_test()
    correct <- correct_answ()

    fun <- function(x, b0, b1) {exp(b0 + b1 * x) / (1 + exp(b0 + b1 * x))}

    df <- data.frame(x = sort(unique(sc)),
                     y = tapply(correct[, input$logregSlider], sc, mean),
                     size = as.numeric(table(sc)))

    ggplot(df, aes(x = x, y = y)) +
      geom_point(aes(size = size),
                 color = "darkblue",
                 fill = "darkblue",
                 shape = 21, alpha = 0.5) +
      stat_function(fun = fun, geom = "line",
                    args = list(b0 = coef(logistic_reg())[1],
                                b1 = coef(logistic_reg())[2]),
                    size = 1,
                    color = "darkblue") +
      xlab("Total Score") +
      ylab("Probability of Correct Answer") +
      scale_y_continuous(limits = c(0, 1)) +
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
      ggtitle(paste("Item", input$logregSlider))
  })
  # ** Table of parameters ####
  output$logregtab <- renderTable({

    tab <- summary(logistic_reg())$coef[1:2, 1:2]

    colnames(tab) <- c("Estimate", "SD")
    rownames(tab) <- c("b0", "b1")
    tab
  },
  include.rownames = T,
  include.colnames = T)
  # ** Interpretation ####
  output$logisticint <- renderUI({

    b1 <- coef(logistic_reg())[2]
    b1 <- round(b1, 2)
    txt1 <- paste ("<b>", "Interpretation:","</b>")
    txt2 <- paste (
      "A one-unit increase in the total
      score is associated with the increase in the log
      odds of answering the item correctly
      vs. not correctly in the amount of"
    )
    txt3 <- paste ("<b>", b1, "</b>")
    HTML(paste(txt1, txt2, txt3))
  })



  # * LOGISTIC Z #####
  # ** Model ####
  z_logistic_reg <- reactive({
    scaledsc <- c(scale(scored_test()))
    model <- glm(correct_answ()[, input$zlogregSlider] ~ scaledsc, family = "binomial")
  })

  output$zlogreg <- renderPlot({
    scaledsc <- scale(scored_test())


    fun <- function(x, b0, b1) {exp(b0 + b1 * x) / (1 + exp(b0 + b1 * x))}

    df <- data.frame(x = sort(unique(scaledsc)),
                     y = tapply(correct_answ()[, input$zlogregSlider], scaledsc, mean),
                     size = as.numeric(table(scaledsc)))
    ggplot(df, aes(x = x, y = y)) +
      geom_point(aes(size = size),
                 color = "darkblue",
                 fill = "darkblue",
                 shape = 21, alpha = 0.5) +
      stat_function(fun = fun, geom = "line",
                    args = list(b0 = coef(z_logistic_reg())[1],
                                b1 = coef(z_logistic_reg())[2]),
                    size = 1,
                    color = "darkblue") +
        xlab("Standardized Total Score (Z-score)") +
        ylab("Probability of Correct Answer") +
        scale_y_continuous(limits = c(0, 1)) +
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
        ggtitle(paste("Item", input$zlogregSlider))
  })

  # * Table of parameters ####
  output$zlogregtab <- renderTable({

    tab <- summary(z_logistic_reg())$coef[1:2, 1:2]
    colnames(tab) <- c("Estimate", "SD")
    rownames(tab) <- c("b0", "b1")
    tab
  },
  include.rownames = T,
  include.colnames = T)

  # * Interpretation ####
  output$zlogisticint <- renderUI({

    b1 <- summary(z_logistic_reg())$coef[2, 1]
    b1 <- round(b1, 2)
    b0 <- round(summary(z_logistic_reg())$coef[1, 1], 2)

    txt1 <- paste ("<b>", "Interpretation:", "</b>")
    txt2 <-
      paste (
        "A one-unit increase in the z-score (one SD increase in original scores)
        is associated with the increase in the log
        odds of answering the item correctly
        vs. not correctly in the amount of"
      )
    txt3 <- paste ("<b>", b1, "</b>")
    HTML(paste(txt1, txt2, txt3))
  })


  # * LOGISTIC IRT Z ##### ####
  # ** Plot with estimated logistic curve ####
  output$zlogreg_irt <- renderPlot({
    scaledsc <- scale(scored_test())

    fun <- function(x, b0, b1) {exp(b0 + b1 * x) / (1 + exp(b0 + b1 * x))}

    df <- data.frame(x = sort(unique(scaledsc)),
                     y = tapply(correct_answ()[, input$zlogreg_irtSlider], scaledsc, mean),
                     size = as.numeric(table(scaledsc)))
    ggplot(df, aes(x = x, y = y)) +
      geom_point(aes(size = size),
                 color = "darkblue",
                 fill = "darkblue",
                 shape = 21, alpha = 0.5) +
      stat_function(fun = fun, geom = "line",
                    args = list(b0 = coef(z_logistic_reg())[1],
                                b1 = coef(z_logistic_reg())[2]),
                    size = 1,
                    color = "darkblue") +
      xlab("Standardized Total Score (Z-score)") +
      ylab("Probability of Correct Answer") +
      scale_y_continuous(limits = c(0, 1)) +
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
      ggtitle(paste("Item", input$zlogreg_irtSlider))
  })

  # ** Table of parameters ####
  output$zlogregtab_irt <- renderTable({

    tab_coef_old <- coef(z_logistic_reg())

    # delta method
    g <- list( ~ x2,  ~ -x1/x2)
    cov <- vcov(z_logistic_reg())
    cov <- as.matrix(cov)
    syms <- paste("x", 1:2, sep = "")
    for (i in 1:2) assign(syms[i], tab_coef_old[i])
    gdashmu <- t(sapply(g, function(form) {
      as.numeric(attr(eval(deriv(form, syms), envir = parent.frame()), "gradient"))
    }))
    new.covar <- gdashmu %*% cov %*% t(gdashmu)
    tab_sd <- sqrt(diag(new.covar))


    tab_coef <- c(tab_coef_old[2], -tab_coef_old[1]/tab_coef_old[2])
    tab <- cbind(tab_coef, tab_sd)

    colnames(tab) <- c("Estimate", "SD")
    rownames(tab) <- c("a", "b")
    tab
  },
  include.rownames = T)

  # ** Interpretation ####
  output$zlogisticint_irt <- renderUI({

    b1 <- summary(z_logistic_reg())$coef[2, 1]
    b1 <- round(b1, 2)
    b0 <- round(summary(z_logistic_reg())$coef[1, 1], 2)

    txt1 <- paste ("<b>", "Interpretation:", "</b>")
    txt2 <-
      paste (
        "A one-unit increase in the z-score (one SD increase in original scores)
        is associated with the increase in the log
        odds of answering the item correctly
        vs. not correctly in the amount of"
      )
    txt3 <- paste ("<b>", b1, "</b>")
    HTML(paste(txt1, txt2, txt3))
  })


  # * NONLINEAR IRT Z #####
  # ** Plot with estimated nonlinear curve ####
  nls_model <- reactive({
    regFce_noDIF <- deriv3(
      ~ c + (1 - c) / (1 + exp(-a * (x - b))),
      namevec = c("a", "b", "c"),
      function.arg = function(x, a, b, c) {}
      )
    regFce_klasik <- deriv3(
      ~ c + (1 - c) / (1 + exp(-(b0 + b1 * x))),
      namevec = c("b0", "b1", "c"),
      function.arg = function(x, b0, b1, c) {}
      )

    scaledsc <- scale(scored_test())

    Q3 <- cut(scaledsc, quantile(scaledsc, (0:3) / 3),
              c("I", "II", "III"),
              include.lowest = TRUE)

    x <- cbind(mean(scaledsc[Q3 == "I"]),
               apply(correct_answ()[Q3 == "I",], 2, mean))
    y <- cbind(mean(scaledsc[Q3 == "III"]),
               apply(correct_answ()[Q3 == "III",], 2, mean))
    u1 <- y[, 1] - x[, 1]
    u2 <- y[, 2] - x[, 2]
    ### intercept of line
    c <- -(-u1 * y[, 2] + u2 * y[, 1]) / u1
    ### slope of line
    t <- u2 / u1
    g <- apply(cbind(0, t * (-4) + c), 1, max)

    b <- ((1 + g) / 2 - c) / t

    alpha <- 4 * t / (1 - g)

    discr <- alpha
    diffi <- b
    guess <- g
    i <- input$nlsSlider

    start <- cbind(discr, diffi, guess)
    colnames(start) <- c("a", "b", "c")

    estim_klasik1 <-
      nls(
        correct_answ()[, i] ~ regFce_noDIF(scaledsc, a, b, c),
        algorithm = "port", start = start[input$nlsSlider,],
        lower = c(-20, -20, 0), upper = c(20, 20, 1)
      )

    estim_klasik1
  })


  output$nlsplot <- renderPlot({

    scaledsc = scale(scored_test())

    fun <- function(x, a, b, c){c + (1 - c) / (1 + exp(-a * (x - b)))}
    df <- data.frame(x = sort(unique(scaledsc)),
                     y = tapply(correct_answ()[, input$nlsSlider], scaledsc, mean),
                     size = as.numeric(table(scaledsc)))
    ggplot(df, aes(x = x, y = y)) +
      geom_point(aes(size = size),
                 color = "darkblue",
                 fill = "darkblue",
                 shape = 21, alpha = 0.5) +
      stat_function(fun = fun, geom = "line",
                    args = list(a = coef(nls_model())[1],
                                b = coef(nls_model())[2],
                                c = coef(nls_model())[3]),
                    size = 1,
                    color = "darkblue") +
      xlab("Standardized Total Score (Z-score)") +
      ylab("Probability of Correct Answer") +
      scale_y_continuous(limits = c(0, 1)) +
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
      ggtitle(paste("Item", input$nlsSlider))
  })

  # Table of parameters
  output$nonlinearztab <- renderTable({

    tabulka <- summary(nls_model())$parameters[, 1:2]
    colnames(tabulka) <- c("Estimate", "SD")
    tabulka
  },
  include.rownames = T,
  include.colnames = T
  )

  # ** Interpretation ####
  output$nonlinearint <- renderUI({

    a <- round(summary(nls_model())$coef[1, 1], 2)
    b <- round(summary(nls_model())$coef[2, 1], 2)

    txt1 <- paste ("<b>", "Interpretation:", "</b>")
    txt2 <- paste (
        "A one-unit increase in the z-score (one SD increase in original scores) is associated
        with the increase in the log odds of answering the item correctly vs. not correctly
        in the amount of "
      )
    txt3 <- paste ("<b>", a, "</b>")
    HTML(paste(txt1, txt2, txt3))

  })

  # * MULTINOMIAL ######
  # ** Model ####
  multinomial_model <- reactive({
    stotal <- c(scale(scored_test()))
    k <- t(as.data.frame(test_key()))

    fitM <- multinom(relevel(as.factor(test_answers()[, input$multiSlider]),
                             ref = paste(k[input$multiSlider])) ~ stotal)
    fitM
  })
  # ** Plot with estimated curves of multinomial regression ####
  output$multiplot <- renderPlot({
    k <- t(as.data.frame(test_key()))

    stotal <- c(scale(scored_test()))

    fitM <- multinom(relevel(as.factor(test_answers()[, input$multiSlider]),
                             ref = paste(k[input$multiSlider])) ~ stotal)

    pp <- fitted(fitM)

    stotals <- rep(stotal, length(levels(relevel(as.factor(test_answers()[, input$multiSlider]),
                                                 ref = paste(k[input$multiSlider])))))
    df <- cbind(melt(pp), stotals)


    df2 <- data.frame(table(test_answers()[, input$multiSlider], stotal),
                      y = data.frame(prop.table(table(test_answers()[, input$multiSlider], stotal), 2))[, 3])
    df2$stotal <- as.numeric(levels(df2$stotal))[df2$stotal]
    df2$Var2 <- relevel(df2$Var1, ref = paste(k[input$multiSlider]))


    ggplot() +
      geom_line(data = df,
                aes(x = stotals , y = value,
                    colour = Var2, linetype = Var2), size = 1) +
      geom_point(data = df2,
                 aes(x = stotal, y = y,
                     colour = Var2, fill = Var2,
                     size = Freq),
                 alpha = 0.5, shape = 21) +

      ylim(0, 1) +
      labs(title = paste("Item", input$multiSlider),
           x = "Standardized Total Score",
           y = "Probability of Correct Answer") +
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
            plot.title = element_text(face = "bold"),
            legend.key.width = unit(1, "cm"))

    })

  output$multieq <- renderUI ({
    cor_option <- test_key()[input$multiSlider]
    withMathJax(
      sprintf(
        '$$\\mathrm{P}(Y = i|Z, b_{i0}, b_{i1}) = \\frac{e^{\\left( b_{i0} + b_{i1} Z\\right)}}{1 + \\sum_j e^{\\left( b_{j0} + b_{j1} Z\\right)}}, \\\\
           \\mathrm{P}(Y = %s|Z, b_{i0}, b_{i1}) = \\frac{1}{1 + \\sum_j e^{\\left( b_{j0} + b_{j1} Z\\right)}}, \\\\
        \\text{where } i \\text{ is one of the wrong options and } %s \\text{ is the correct one.}$$',
        cor_option, cor_option, cor_option, cor_option
      )
    )
  })

  # ** Table of parameters ####
  output$multitab <- renderTable({

    koef <- as.vector(coef(multinomial_model()))
    std  <- as.vector(sqrt(diag(vcov(multinomial_model()))))
    tab  <- cbind(koef, std)
    colnames(tab) <- c("Estimate", "SD")
    rownames(tab) <- c(paste("b", rownames(coef(multinomial_model())), "0", sep = ""),
                       paste("b", rownames(coef(multinomial_model())), "1", sep = ""))
    tab
  },
  include.rownames = T)

  # ** Interpretation ####
  output$multiint <- renderUI({

    koef <- summary(multinomial_model())$coefficients
    txt  <- c()

    for (i in 1:nrow(koef)){
    txt[i] <- paste (
      "A one-unit increase in the z-score (one SD increase in original
        scores)  is associated with the decrease in the log odds of
        answering the item "
    ,"<b>", row.names(koef)[i], "</b>", "vs.", "<b>",
    test_key()[input$multiSlider],
    "</b>","in the amount of ",
    "<b>", round(koef[i, 2], 2), "</b>", '<br/>')
    }
  HTML(paste(txt))
  })


  ######################
  # * IRT MODELS ######
  ######################
  # ** 1 PL (RASCH) ####
  rasch_model <- reactive({
    fitRasch <- rasch(correct_answ())
  })

  # *** CC ####
  output$rasch <- renderPlot({
    plot(rasch_model())
  })

  # *** IIC ####
  output$raschiic <- renderPlot({
    plot(rasch_model(), type = "IIC")
  })

  # *** TIF ####
  output$raschtif <- renderPlot({
    plot(rasch_model(),items = 0, type = "IIC")
  })

  # *** Table of parameters ####
  output$raschcoef <- renderTable({

    tab <- coef(rasch_model())
    tab <- cbind(tab,
                 sqrt(diag(vcov(rasch_model())))[1:nrow(tab)],
                 rep(sqrt(diag(vcov(rasch_model())))[nrow(tab) + 1], nrow(tab)))
    tab <- tab[, c(2, 4, 1, 3)]
    colnames(tab) <- c("a", "SD(a)", "b", "SD(b)")
    rownames(tab) <- paste("Item", 1:nrow(tab))
    tab
  },
  include.rownames = T)

  # *** Factor scores plot ####
  output$raschFactor <- renderPlot({
    fit1 <- rasch_model()
    df1  <- ltm::factor.scores(fit1, return.MIvalues = T)$score.dat
    FS   <- as.vector(df1[, "z1"])
    df2  <- df1
    df2$Obs <- df2$Exp <- df2$z1 <- df2$se.z1 <- NULL
    STS <- as.vector(scale(apply(df2, 1, sum)))
    df  <- data.frame(FS, STS)


    ggplot(df, aes_string("STS", "FS")) +
      geom_point(size = 3) +
      labs(x = "Standardized total score", y = "Factor score") +
      theme_bw() +
      theme(text = element_text(size = 14),
            plot.title = element_text(face = "bold", vjust = 1.5),
            axis.line  = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank()) +
      theme(legend.box.just = "left",
            legend.justification = c(1, 0),
            legend.position = c(1, 0),
            legend.margin = unit(0, "lines"),
            legend.box = "vertical",
            legend.key.size = unit(1, "lines"),
            legend.text.align = 0,
            legend.title.align = 0)
  })

  # ** 2PL ####
  two_param_irt <- reactive({
    fit2PL <- ltm(correct_answ() ~ z1, IRT.param = TRUE)
  })

  # *** ICC ####
  output$twoparam <- renderPlot({
    plot(two_param_irt())
  })

  # *** IIC ####
  output$twoparamiic <- renderPlot({
    plot(two_param_irt(), type = "IIC")
  })

  # *** TIF ####
  output$twoparamtif <- renderPlot({
    plot(two_param_irt(), items = 0, type = "IIC")
  })

  # ** Table of parameters ####
  output$twoparamcoef <- renderTable({
    fit2pl <- two_param_irt()
    tab <- coef(fit2pl)
    tab <- cbind(tab,
                 sqrt(diag(vcov(fit2pl)))[1:nrow(tab)],
                 sqrt(diag(vcov(fit2pl)))[(nrow(tab) + 1):(2 * nrow(tab))])
    tab <- tab[, c(2, 4, 1, 3)]
    colnames(tab) <- c("a", "SD(a)", "b", "SD(b)")
    rownames(tab) <- paste("Item", 1:nrow(tab))
    tab
  },
  include.rownames = T)

  # *** Factor scores plot ####
  output$twoFactor <- renderPlot({
    fit2 <- two_param_irt()
    df1  <- ltm::factor.scores(fit2, return.MIvalues = T)$score.dat
    FS   <- as.vector(df1[, "z1"])
    df2  <- df1
    df2$Obs <- df2$Exp <- df2$z1 <- df2$se.z1 <- NULL
    STS  <- as.vector(scale(apply(df2, 1, sum)))
    df   <- data.frame(FS, STS)

    ggplot(df, aes_string("STS", "FS")) +
      geom_point(size = 3) +
      labs(x = "Standardized total score", y = "Factor score") +
      theme_bw() +
      theme(text = element_text(size = 14),
            plot.title = element_text(face = "bold", vjust = 1.5),
            axis.line  = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank()) +
      theme(legend.box.just = "left",
            legend.justification = c(1, 0),
            legend.position = c(1, 0),
            legend.margin = unit(0, "lines"),
            legend.box = "vertical",
            legend.key.size = unit(1, "lines"),
            legend.text.align = 0,
            legend.title.align = 0)
  })

  # ** 3PL ####
  three_param_irt <- reactive({
    fit3PL <- tpm(correct_answ(), IRT.param = TRUE)
  })
  # ** ICC ####
  output$threeparam <- renderPlot({
    plot(three_param_irt())
  })

  # *** IIC ####
  output$threeparamiic <- renderPlot({
    plot(three_param_irt(), type = "IIC")
  })

  # *** TIF ####
  output$threeparamtif <- renderPlot({
    plot(three_param_irt(), items = 0, type = "IIC")
  })

  # *** Table of parameters ####
  output$threeparamcoef <- renderTable({
    fit3pl <- tpm(GMAT2[, 1:20], IRT.param = TRUE)
    tab <- coef(fit3pl)
    tab <- cbind(tab,
                 sqrt(diag(vcov(fit3pl)))[1:nrow(tab)],
                 sqrt(diag(vcov(fit3pl)))[(nrow(tab) + 1):(2 * nrow(tab))],
                 sqrt(diag(vcov(fit3pl)))[(2 * nrow(tab) + 1):(3 * nrow(tab))])
    tab <- tab[, c(3, 6, 2, 5, 1, 4)]
    colnames(tab) <- c("a", "SD(a)", "b", "SD(b)", "c", "SD(c)")
    rownames(tab) <- paste("Item", 1:nrow(tab))
    tab
  },
  include.rownames = T)

  # *** Factor scores ####
  output$threeFactor <- renderPlot({
    fit3 <- three_param_irt()
    df1  <- ltm::factor.scores(fit3, return.MIvalues = T)$score.dat
    FS   <- as.vector(df1[, "z1"])
    df2  <- df1
    df2$Obs <- df2$Exp <- df2$z1 <- df2$se.z1 <- NULL
    STS  <- as.vector(scale(apply(df2, 1, sum)))
    df   <- data.frame(FS, STS)

    ggplot(df, aes_string("STS", "FS")) +
      geom_point(size = 3) +
      labs(x = "Standardized total score", y = "Factor score") +
      theme_bw() +
      theme(text = element_text(size = 14),
            plot.title = element_text(face = "bold", vjust = 1.5),
            axis.line  = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank()) +
      theme(legend.box.just = "left",
            legend.justification = c(1, 0),
            legend.position = c(1, 0),
            legend.margin = unit(0, "lines"),
            legend.box = "vertical",
            legend.key.size = unit(1, "lines"),
            legend.text.align = 0,
            legend.title.align = 0)
  })

  ######################
  # DIF/FAIRNESS ####
  ######################
  # * TOTAL SCORES ####
  # ** Summary of Total Scores for Groups ####
  output$resultsgroup <- renderTable({
    sc_one  <- scored_test()[DIF_groups() == 1]
    sc_zero <- scored_test()[DIF_groups() == 0]
    tab <- t(data.frame(round(c(min(sc_zero), max(sc_zero), mean(sc_zero), median(sc_zero),
                                sd(sc_zero), skewness(sc_zero), kurtosis(sc_zero)), 2),
                        round(c(min(sc_one), max(sc_one), mean(sc_one), median(sc_one),
                                sd(sc_one), skewness(sc_one), kurtosis(sc_one)), 2)))
    colnames(tab) <- c("Min", "Max", "Mean", "Median", "SD", "Skewness", "Kurtosis")
    rownames(tab) <- c("Reference group (0)", "Focal group (1)")
    tab
  },
  digits = 2,
  include.rownames = T,
  include.colnames = T)

  # ** Cut score ####
  output$slider2group <- renderUI({
    sliderInput(
      "inSlider2group", "Cut-Score", min = 0, max = ncol(test_answers()),
      value = round(median(scored_test()[DIF_groups() == 1])), step = 1
    )
  })

  # ** Histogram of total score for group = 1 (focal) ####
  output$histbyscoregroup1 <- renderPlot ({

    a <- test_answers()
    k <- test_key()
    sc  <- scored_test()[DIF_groups() == 1]


    bin <- as.numeric(input$inSlider2group)

    df <- data.frame(sc,
                     gr = cut(sc,
                              breaks = unique(c(0, bin - 1, bin, ncol(a))),
                              include.lowest = T))

    if (bin < min(sc)){
      col <- "blue"
    } else {
      if (bin == min(sc)){
        col <- c("grey", "blue")
      } else {
        col <- c("red", "grey", "blue")
      }
    }

    ggplot(df, aes(x = sc)) +
      geom_histogram(aes(fill = gr), binwidth = 1, color = "black") +
      scale_fill_manual("", breaks = df$gr, values = col) +
      labs(x = "Total Score",
           y = "Number of Students") +
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
            plot.title = element_text(face = "bold")) +
      ggtitle("Histogram of Total Scores for Focal Group")
  })
  # ** Histogram of total score for group = 0 (reference) ####
  output$histbyscoregroup0 <- renderPlot ({

    a <- test_answers()
    k <- test_key()
    sc  <- scored_test()[DIF_groups() == 0]

    bin <- as.numeric(input$inSlider2group)


    df <- data.frame(sc,
                     gr = cut(sc,
                              breaks = unique(c(0, bin - 1, bin, ncol(a))),
                              include.lowest = T))

    if (bin < min(sc)){
      col <- "blue"
    } else {
      if (bin == min(sc)){
        col <- c("grey", "blue")
      } else {
        col <- c("red", "grey", "blue")
      }
    }
    ggplot(df, aes(x = sc)) +
      geom_histogram(aes(fill = gr), binwidth = 1, color = "black") +
      scale_fill_manual("", breaks = df$gr, values = col) +
      labs(x = "Total Score",
           y = "Number of Students") +
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
            plot.title = element_text(face = "bold")) +
      ggtitle("Histogram of Total Scores for Reference Group")
  })


  # * DELTA PLOT ####
  deltaGpurn <- reactive ({
    switch(input$type_threshold,
           "Fixed" = deltaPlot(DPdata(), group = "group",
                               focal.name = 1,
                               thr = 1.5),
           "Normal"= deltaPlot(DPdata(), group = "group",
                               focal.name = 1,
                               thr = "norm")
    )
  })


  # * Delta plot ####
  output$deltaplot <- renderPlot({
    p <- ggplot(data.frame(deltaGpurn()$Deltas),
                aes(x = X1, y = X2, label = rownames(data.frame(deltaGpurn()$Deltas)))) +
      geom_point() +
      geom_text(hjust = 0, nudge_x = 0.05) +
      geom_abline(intercept = deltaGpurn()$axis.par[1], slope = deltaGpurn()$axis.par[2],
                  size = 1) +
      geom_abline(intercept = deltaGpurn()$axis.par[1] + deltaGpurn()$thr * sqrt(deltaGpurn()$axis.par[2]^2 + 1),
                  slope = deltaGpurn()$axis.par[2],
                  color = "red",
                  linetype = "dashed",
                  size = 1) +
      geom_abline(intercept = deltaGpurn()$axis.par[1] - deltaGpurn()$thr * sqrt(deltaGpurn()$axis.par[2]^2 + 1),
                  slope = deltaGpurn()$axis.par[2],
                  color = "red",
                  linetype = "dashed",
                  size = 1) +
      labs(x = "Reference group",
           y = "Focal group") +
      xlim(min(deltaGpurn()$Deltas) - 0.5, max(deltaGpurn()$Deltas) + 0.5) +
      ylim(min(deltaGpurn()$Deltas) - 0.5, max(deltaGpurn()$Deltas) + 0.5) +
      theme_bw() +
      theme(legend.title = element_blank(),
            axis.line  = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            text = element_text(size = 14),
            plot.title = element_text(face = "bold"))
    if (is.numeric(deltaGpurn()$DIFitems)){
      p <- p + geom_point(aes(x = deltaGpurn()$Deltas[deltaGpurn()$DIFitems, 1],
                              y = deltaGpurn()$Deltas[deltaGpurn()$DIFitems, 2]),
                          size = 6, color = "black", shape = 1)
    }
    p
  })

  # Output
  output$dp_text_normal <- renderPrint({
    deltaGpurn()
  })

  # * MANTEL-HAENSZEL ####
  # ** Model for print ####
  model_DIF_MH <- reactive({
    group <- DIF_groups()
    data <- correct_answ()

    mod <- difMH(Data = data, group = group, focal.name = 1,
                 p.adjust.method = input$correction_method_MZ_print)
    mod
  })

  # ** Model for tables ####
  model_DIF_MH_tables <- reactive({
    group <- DIF_groups()
    data <- correct_answ()

    mod <- difMH(Data = data, group = group, focal.name = 1)
    # no need for correction, estimates of OR are the same
    mod
  })

  # ** Output print ####
  output$print_DIF_MH <- renderPrint({
    print(model_DIF_MH())
  })


  # ** Contingency tables ####
  table_DIF_MH <- reactive({

    group <- DIF_groups()
    data <- correct_answ()

    total <- apply(data, 1, sum)

    df <- data.frame(data[, input$difMHSlider_item], group)
    colnames(df) <- c("Answer", "Group")
    df$Answer <- relevel(factor(df$Answer, labels = c("Incorrect", "Correct")), "Correct")
    df$Group <- factor(df$Group, labels = c("Reference Group", "Focal Group"))


    df <- df[total == input$difMHSlider_score, ]

    tab <- dcast(data.frame(xtabs(~ Group + Answer, data = df)),
                 Group ~ Answer, value.var = "Freq", margins = T,
                 fun = sum)

    colnames(tab)[4] <- tab$Group[3] <- levels(tab$Group)[3]  <- "Total"
    colnames(tab)[1] <- ""
    tab

  })

  # ** Contingency tables output ####
  output$table_DIF_MH <- renderTable({
    table_DIF_MH()
  })

  # ** OR calculation ####
  output$ORcalculation <- renderUI ({
    a <- table_DIF_MH()[1, 2]
    b <- table_DIF_MH()[1, 3]
    c <- table_DIF_MH()[2, 2]
    d <- table_DIF_MH()[2, 3]
    OR <- round((a*d)/(b*c), 2)

    alphaMH <- round(model_DIF_MH_tables()$alphaMH[input$difMHSlider_item], 2)

    txt <- ifelse((b * c == 0)|(a * d == 0), "Odds ratio cannot be calculated!",
                  paste("For students who reached total score of", input$difMHSlider_score,
                        "the odds of answering item", input$difMHSlider_item, "correctly is",
                        ifelse(OR == 1, "is the same for both groups. ",
                               ifelse(OR > 1,
                                      paste(OR, "times higher in the reference group than in the focal group."),
                                      paste(OR, "times lower in the reference group than in the focal group.")))))

    txtMH <- paste("Mantel-Haenszel estimate of odds ratio accounting for all levels of total score is equal to",
                   alphaMH, ". The odds of answering item", input$difMHSlider_item, "correctly is",
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




  # * LOGISTIC  ####
  # ** Model for plot ####
  model_DIF_logistic_plot <- reactive({
    group <- DIF_groups()
    data <- correct_answ()

    mod <- difLogistic(Data = data, group = group, focal.name = 1,
                       type = input$type_plot_DIF_logistic, p.adjust.method = input$correction_method_logItems)
    mod
  })

  # ** Model for print ####
  model_DIF_logistic_print <- reactive({
    group <- DIF_groups()
    data <- correct_answ()

    mod <- difLogistic(Data = data, group = group, focal.name = 1,
                       type = input$type_print_DIF_logistic, p.adjust.method = input$correction_method_logSummary)
    mod
  })

  # ** Output print ####
  output$print_DIF_logistic <- renderPrint({
    print(model_DIF_logistic_print())
  })

  # ** Plot ####
  output$plot_DIF_logistic <- renderPlot({
    group <- DIF_groups()
    data <- correct_answ()

    type <- input$type_plot_DIF_logistic
    plotDIFLogistic(data, group,
                    type = input$type_plot_DIF_logistic,
                    item =  input$diflogSlider,
                    IRT = F,
                    p.adjust.method = input$correction_method_logItems
                    )

  })

  # ** Table with coefficients ####
  output$tab_coef_DIF_logistic <- renderTable({

    tab_coef <- model_DIF_logistic_plot()$logitPar[input$diflogSlider, ]
    tab_sd <- model_DIF_logistic_plot()$logitSe[input$diflogSlider, ]

    tab <- data.frame(tab_coef, tab_sd)

    rownames(tab) <- c('b0', 'b1', 'b2', 'b3')
    colnames(tab) <- c("Estimate", "SD")

    tab
  },
  include.rownames = T,
  include.colnames = T)


  # * LOGISTIC IRT Z ####
  # ** Model for plot ####
  model_DIF_logistic_IRT_Z_plot <- reactive({
    group <- DIF_groups()
    data <- correct_answ()

    mod <- difLogistic(Data = data, group = group, focal.name = 1,
                       type = input$type_plot_DIF_logistic_IRT_Z,
                       match = scale(scored_test()),
                       p.adjust.method = input$correction_method_logZItems,
                       all.cov = T)
    mod
  })

  # ** Model for print ####
  model_DIF_logistic_IRT_Z_print <- reactive({
    group <- DIF_groups()
    data <- correct_answ()

    mod <- difLogistic(Data = data, group = group, focal.name = 1,
                       type = input$type_print_DIF_logistic_IRT_Z,
                       match = scale(scored_test()),
                       p.adjust.method = input$correction_method_logzZSummary,
                       all.cov = T)
    mod
  })

  # ** Output print ####
  output$print_DIF_logistic_IRT_Z <- renderPrint({
    print(model_DIF_logistic_print())
  })

  # ** Plot ####
  output$plot_DIF_logistic_IRT_Z <- renderPlot({
    group <- DIF_groups()
    data <- correct_answ()

    type <- input$type_plot_DIF_logistic
    plotDIFLogistic(data, group,
                    type = input$type_plot_DIF_logistic_IRT_Z,
                    item =  input$diflog_irtSlider,
                    IRT = T,
                    p.adjust.method = input$correction_method_logZItems)
  })

  output$tab_coef_DIF_logistic_IRT_Z <- renderTable({

    fit <- model_DIF_logistic_IRT_Z_plot()

    tab_coef_old <- fit$logitPar[input$diflog_irtSlider, ]
    tab_coef <- c()
    # a = b1, b = -b0/b1, adif = b3, bdif = -(b1b2-b0b3)/(b1(b1+b3))
    tab_coef[1] <- tab_coef_old[2]
    tab_coef[2] <- -(tab_coef_old[1] / tab_coef_old[2])
    tab_coef[3] <- tab_coef_old[4]
    tab_coef[4] <- -(tab_coef_old[2] * tab_coef_old[3] + tab_coef_old[1] * tab_coef_old[4] ) /
      (tab_coef_old[2] * (tab_coef_old[2] + tab_coef_old[4]))

    # delta method
    g <- list( ~ x2,  ~ -x1/x2, ~ x4, ~ -((x2 * x3 - x1 * x4) / (x2 * (x2 + x4))))
    if (is.character(fit$DIFitems) | !(input$diflog_irtSlider %in% fit$DIFitems)){
      cov <- matrix(0, ncol = 4, nrow = 4)
      cov[1:2, 1:2] <-  fit$cov.M1[[input$diflog_irtSlider]]
    } else {
      cov <-  fit$cov.M0[[input$diflog_irtSlider]]
    }
    cov <- as.matrix(cov)
    syms <- paste("x", 1:4, sep = "")
    for (i in 1:4) assign(syms[i], tab_coef_old[i])
    gdashmu <- t(sapply(g, function(form) {
      as.numeric(attr(eval(deriv(form, syms), envir = parent.frame()), "gradient"))
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


  # * NLR DIF ####
  # ** Model for plot ####
  model_DIF_NLR_plot <- reactive({
    group <- DIF_groups()
    data <- correct_answ()

    mod <- difNLR(data = data, group = group, type = input$type_plot_DIF_NLR,
                  p.adjust.method = input$correction_method_nlrItems)
    mod
  })

  # ** Model for print ####
  model_DIF_NLR_print <- reactive({
    group <- DIF_groups()
    data <- correct_answ()

    mod <- difNLR(data = data, group = group, type = input$type_print_DIF_NLR,
                  p.adjust.method = input$correction_method_nlrSummary)
    mod
  })

  # ** Output print ####
   output$print_DIF_NLR <- renderPrint({
     print(model_DIF_NLR_print())
   })

   # ** Plot ####
   output$plot_DIF_NLR <- renderPlot({
     plot(model_DIF_NLR_plot(), item = input$difnlrSlider)[[1]] +
       theme(text = element_text(size = 14),
             plot.title = element_text(size = 14, face = "bold", vjust = 1.5))
   })

   output$tab_coef_DIF_NLR <- renderTable({

     tab_coef <- t(as.table(model_DIF_NLR_plot()$coef[input$difnlrSlider, ]))
     tab_sd <- lapply(lapply(model_DIF_NLR_plot()$vcov, diag), `length<-`,
                      max(lengths(lapply(model_DIF_NLR_plot()$vcov, diag))))
     tab_sd <- t(matrix(unlist(tab_sd), nrow = 5))
     tab_sd[is.na(tab_sd)] <- 0

     tab <- data.frame(tab_coef, tab_sd[input$difnlrSlider, ])
     tab <- data.frame(tab[, 3:4])
     rownames(tab) <- c('a', 'b', 'c', 'aDIF', 'bDIF')
     colnames(tab) <- c("Estimate", "SD")

     tab
   },
   include.rownames = T)

   # * IRT LORD ####
   # ** Model for plot ####
   model_DIF_IRT_Lord_plot <- reactive({
     group <- DIF_groups()
     data <- correct_answ()

     guess <- ifelse(input$type_print_DIF_IRT_lord == "3PL",
                     itemPar3PL(data)[, 3], 0)

     mod <- switch(input$type_print_DIF_IRT_lord,
                   "1PL" = difLord(Data = data, group = group, focal.name = 1,
                                   model = "1PL",
                                   p.adjust.method = "BH"),
                   "2PL" = difLord(Data = data, group = group, focal.name = 1,
                                   model = "2PL",
                                   p.adjust.method = "BH"),
                   "3PL" = difLord(Data = data, group = group, focal.name = 1,
                                   model = "3PL", c = guess,
                                   p.adjust.method = "BH"))
     mod
   })

   # ** Model for print ####
   model_DIF_IRT_Lord_print <- reactive({
     group <- DIF_groups()
     data <- correct_answ()

     guess <- ifelse(input$type_print_DIF_IRT_lord == "3PL",
                     itemPar3PL(data)[, 3], 0)

     mod <- switch(input$type_print_DIF_IRT_lord,
                   "1PL" = difLord(Data = data, group = group, focal.name = 1,
                                   model = "1PL",
                                   p.adjust.method = "BH"),
                   "2PL" = difLord(Data = data, group = group, focal.name = 1,
                                   model = "2PL",
                                   p.adjust.method = "BH"),
                   "3PL" = difLord(Data = data, group = group, focal.name = 1,
                                   model = "3PL", c = guess,
                                   p.adjust.method = "BH"))
     mod
   })

   # ** Output print ####
   output$print_DIF_IRT_Lord <- renderPrint({
     print(model_DIF_IRT_Lord_print())
   })

   # ** Plot ####
   output$plot_DIF_IRT_Lord <- renderPlot({
     plotDIFirt(parameters = tab_coef_DIF_IRT_Lord())
   })

   # ** Table with coefficients ####
   tab_coef_DIF_IRT_Lord <- reactive({

     m <- nrow(model_DIF_IRT_Lord_plot()$itemParInit)/2
     wh_coef <- switch(input$type_plot_DIF_IRT_lord,
                  "1PL" = 1,
                  "2PL" = 1:2,
                  "3PL" = c(1, 2, 6))
     wh_sd <- switch(input$type_plot_DIF_IRT_lord,
                     "1PL" = 2,
                     "2PL" = 3:4,
                     "3PL" = 3:4)
     tab_coef <- c(model_DIF_IRT_Lord_plot()$itemParInit[c(input$difirt_lord_itemSlider,
                                                           m + input$difirt_lord_itemSlider),
                                                         wh_coef])
     tab_sd <- c(model_DIF_IRT_Lord_plot()$itemParInit[c(input$difirt_lord_itemSlider,
                                                         m + input$difirt_lord_itemSlider),
                                                       wh_sd])

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

   # ** Table with coefficients output ####
   output$tab_coef_DIF_IRT_Lord <- renderTable({
     tab_coef_DIF_IRT_Lord()
   },
   include.rownames = T,
   include.colnames = T)


   # * IRT Raju ####
   # ** Model for plot ####
   model_DIF_IRT_Raju_plot <- reactive({
     group <- DIF_groups()
     data <- correct_answ()

     guess <- ifelse(input$type_print_DIF_IRT_raju == "3PL",
                     itemPar3PL(data)[, 3], 0)

     mod <- switch(input$type_print_DIF_IRT_raju,
                   "1PL" = difRaju(Data = data, group = group, focal.name = 1,
                                   model = "1PL",
                                   p.adjust.method = "BH"),
                   "2PL" = difRaju(Data = data, group = group, focal.name = 1,
                                   model = "2PL",
                                   p.adjust.method = "BH"),
                   "3PL" = difRaju(Data = data, group = group, focal.name = 1,
                                   model = "3PL", c = guess,
                                   p.adjust.method = "BH"))
     mod
   })

   # ** Model for print ####
   model_DIF_IRT_Raju_print <- reactive({
     group <- DIF_groups()
     data <- correct_answ()

     guess <- ifelse(input$type_print_DIF_IRT_raju == "3PL",
                     itemPar3PL(data)[, 3], 0)

     mod <- switch(input$type_print_DIF_IRT_raju,
                   "1PL" = difRaju(Data = data, group = group, focal.name = 1,
                                   model = "1PL",
                                   p.adjust.method = "BH"),
                   "2PL" = difRaju(Data = data, group = group, focal.name = 1,
                                   model = "2PL",
                                   p.adjust.method = "BH"),
                   "3PL" = difRaju(Data = data, group = group, focal.name = 1,
                                   model = "3PL", c = guess,
                                   p.adjust.method = "BH"))
     mod
   })

   # ** Output print ####
   output$print_DIF_IRT_Raju <- renderPrint({
     print(model_DIF_IRT_Raju_print())
   })

   # ** Plot ####
   output$plot_DIF_IRT_Raju <- renderPlot({
     plotDIFirt(parameters = tab_coef_DIF_IRT_Raju(), test = "Raju")
   })

   # ** Table with coefficients ####
   tab_coef_DIF_IRT_Raju <- reactive({

     m <- nrow(model_DIF_IRT_Raju_plot()$itemParInit)/2
     wh_coef <- switch(input$type_plot_DIF_IRT_raju,
                       "1PL" = 1,
                       "2PL" = 1:2,
                       "3PL" = c(1, 2, 6))
     wh_sd <- switch(input$type_plot_DIF_IRT_raju,
                     "1PL" = 2,
                     "2PL" = 3:4,
                     "3PL" = 3:4)
     tab_coef <- c(model_DIF_IRT_Raju_plot()$itemParInit[c(input$difirt_raju_itemSlider, m + input$difirt_raju_itemSlider), wh_coef])
     tab_sd <- c(model_DIF_IRT_Raju_plot()$itemParInit[c(input$difirt_raju_itemSlider, m + input$difirt_raju_itemSlider), wh_sd])

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

   # ** Table with coefficients output ####
   output$tab_coef_DIF_IRT_Raju <- renderTable({
     tab_coef_DIF_IRT_Raju()
   },
   include.rownames = T,
   include.colnames = T)

}
