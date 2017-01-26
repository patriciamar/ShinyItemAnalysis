######################
# GLOBAL LIBRARY #####
######################

library(corrplot)
library(CTT)
library(deltaPlotR)
library(difNLR)
library(difR)
# library(foreign)
library(ggplot2)
library(grid)
# library(gridExtra)
library(ltm)
library(mirt)
library(moments)
library(nnet)
library(psych)
library(psychometric)
library(reshape2)
library(stringr)
library(ShinyItemAnalysis)
library(shinyjs)
library(rmarkdown)
library(WrightMap)

###########
# DATA ####
###########

data('GMAT', package = 'difNLR')
data('GMATtest', package = 'difNLR')
data('GMATkey', package = 'difNLR')
test <- get("GMATtest")
key <- get("GMATkey")

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

# WrightMap
source("wrightMap.R")
source("itemClassic.R")
source("personHist.R")

#####################
# SERVER SCRIPT #####
#####################

function(input, output, session) {


  dataset <- reactiveValues()

  dataset$answers <- NULL
  dataset$key <- NULL
  dataset$group <- NULL

  ######################
  ### HITS COUNTER #####
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

  # LOAD ABCD DATA #####
  test_answers <- reactive ({
    if (is.null(input$data)) {
      a=input$dataSelect
      pos=regexpr("_", a)[1]
      datasetName=str_sub(a, 1,pos-1)
      packageName=str_sub(a, pos+1)

      do.call(data, args=list(paste0(datasetName,"test"), package=packageName))
      test=get(paste0(datasetName,"test"))

      do.call(data, args=list(paste0(datasetName,"key"), package=packageName))
      key=get(paste0(datasetName,"key"))

      test = test[,1:length(key)]
      dataset$answers = test
    } else {
      test = dataset$answers
    }
    test
  })

  # LOAD KEY #####
  test_key <- reactive({
    if ((is.null(input$key)) | (is.null(dataset$key))) {
      a=input$dataSelect
      pos=regexpr("_", a)[1]
      datasetName=str_sub(a, 1,pos-1)
      packageName=str_sub(a, pos+1)

      do.call(data, args=list(paste0(datasetName,"key"), package=packageName))
      key=get(paste0(datasetName,"key"))
      dataset$key = key
    } else {
      if (length(dataset$key) == 1) {
        if (dataset$key == "missing"){
          validate(
            need(dataset$key != "missing", "Key is missing!"),
            errorClass = "error_key_missing"
          )
        }
      } else {
        validate(
          need(ncol(dataset$answers) == length(dataset$key), "Length of key is not the same as
               number of items in the main data set!"),
          errorClass = "error_dimension"
          )
      }
      key=dataset$key
    }
    key
  })

  # LOAD GROUPS #####
  DIF_groups <- reactive({
    if (is.null(input$data) | (is.null(dataset$group))) {
      a=input$dataSelect
      pos=regexpr("_", a)[1]
      datasetName=str_sub(a, 1,pos-1)
      packageName=str_sub(a, pos+1)

      do.call(data, args=list(paste0(datasetName,"test"), package=packageName))
      test=get(paste0(datasetName,"test"))

      if (datasetName == "dataMedical"){
        group <- NULL
        dataset$group <- NULL
        validate(
          need(!is.null(group),
               "Sorry, for this dataset group is not available. DIF and DDF analyses are not possible!"),
          errorClass = "warning_group_missing"
        )
      } else {
        group <- test[, ncol(test)]
      }
      dataset$group = group
    } else {
      if (length(dataset$group) == 1){
        if (dataset$group == "missing"){
          validate(
            need(dataset$group != "missing",
                 "Group is missing! DIF and DDF analyses are not available!"),
            errorClass = "warning_group_missing"
          )
        }
      } else {
        validate(
          need(nrow(dataset$answers) == length(dataset$group), "Length of group is not the same as
             number of observation in the main data set!"),
          errorClass = "error_dimension"
        )
      }
      group = dataset$group
    }
    # group <- as.numeric(paste(as.factor(group)))
    group
  })

  # SUBMIT BUTTON #####

  observeEvent(
    eventExpr = input$submitButton,
    handlerExpr = {

      key=NULL
      answ=NULL
      k=NULL
      group=NULL

      if (is.null(input$data)){
        key <- test_key()
        answ <- test[ , 1:length(key)]
        group <- DIF_groups()
      } else {
        answ <- read.csv(input$data$datapath, header = input$header,
                         sep = input$sep, quote = input$quote)
        if (is.null(input$key)){
          key <- "missing"
        } else {
          key <- read.csv(input$key$datapath, header = input$header)
          key <- as.character(key[[1]])
        }
        if (is.null(input$groups)){
          print(group)
          group <- "missing"
          print(group)
        } else {
          print(group)
          group <- read.csv(input$groups$datapath, header = input$header)
          group <- as.vector(group[[1]])
          print(group)
        }
      }

      dataset$answers<-answ
      dataset$key<-key
      dataset$group<-group

    }
  )

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


  ##### ITEM SLIDERS #####

  observe({
    sliderList<-c(
      "distractorSlider",
      "logregSlider",
      "zlogregSlider",
      "zlogreg_irtSlider",
      "nlsSlider",
      "multiSlider",
      "difMHSlider_item",
      "diflogSlider",
      "diflog_irtSlider",
      "difnlrSlider",
      "difirt_lord_itemSlider",
      "difirt_raju_itemSlider",
      "ddfSlider",
      "reportSlider"
      )

    itemCount = ncol(test_answers())

    updateSliderInput(session = session, inputId = "distractorSlider", max=itemCount)
    updateSliderInput(session = session, inputId = "logregSlider", max=itemCount)
    updateSliderInput(session = session, inputId = "zlogregSlider", max=itemCount)
    updateSliderInput(session = session, inputId = "zlogreg_irtSlider", max=itemCount)
    updateSliderInput(session = session, inputId = "nlsSlider", max=itemCount)
    updateSliderInput(session = session, inputId = "multiSlider", max=itemCount)
    updateSliderInput(session = session, inputId = "difMHSlider_item", max=itemCount)
    updateSliderInput(session = session, inputId = "diflogSlider", max=itemCount)
    updateSliderInput(session = session, inputId = "diflog_irtSlider", max=itemCount)
    updateSliderInput(session = session, inputId = "difnlrSlider", max=itemCount)
    updateSliderInput(session = session, inputId = "difirt_lord_itemSlider", max=itemCount)
    updateSliderInput(session = session, inputId = "difirt_raju_itemSlider", max=itemCount)
    updateSliderInput(session = session, inputId = "ddfSlider", max=itemCount)

    updateSliderInput(session = session, inputId = "inSlider2", max=itemCount, value = round(median(scored_test())))
    updateSliderInput(session = session, inputId = "inSlider2group", max=itemCount, value = round(median(scored_test()[DIF_groups() == 1])))
    updateSliderInput(session = session, inputId = "difMHSlider_score", max=itemCount, value = round(median(scored_test())))

  })

  ########################
  # SUMMARY  ####
  ########################
  # * TOTAL SCORES #####
  # ** Summary table #####
  resultsInput <- reactive({
    sc <- scored_test()

    tab <- t(data.frame(c(min(sc), max(sc), mean(sc), median(sc), sd(sc),
                          skewness(sc), kurtosis(sc))))
    colnames(tab) <- c("Min", "Max", "Mean", "Median", "SD", "Skewness", "Kurtosis")
    tab
  })

  output$results <- renderTable({
    resultsInput()
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
  histogram_totalscoresInput<- reactive({
    histogram_totalscores()
  })

  output$histogram_totalscores <- renderPlot ({
    histogram_totalscoresInput()
  })
  # ** download button - Histogram of Total Scores ####
  output$DP_histogram_totalscores <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = histogram_totalscoresInput(), device = "png", height=3, width=9, dpi=160)
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
    sura <- (tosc / length(k)) * 100
    # Z score
    zsco <- sort(unique(scale(sc)))
    # T score
    tsco <- 50 + 10 * zsco

    tab <- round(data.frame(tosc, perc, sura, zsco, tsco), 2)
    colnames(tab) <- c("Total Score", "Percentile", "Success Rate", "Z-score", "T-score")

    tab
  },
  include.rownames = FALSE)

  # * CORRELATION STRUCTURE #####

  corr_structure <- reactive({
    data <- correct_answ()

    corP <- polychoric(data)
    corP
  })

  # ** Correlation plot ######
  corr_plotInput <- reactive({
    corP <- corr_structure()
    corrplot(corP$rho)
  })

  # ** Output Correlation plot ######
  output$corr_plot <- renderPlot({
    corr_plotInput()
  })

  # ** Scree plot ######
  scree_plotInput <- reactive({
    corP <- corr_structure()
    ev <- eigen(corP$rho)$values
    df <- data.frame(pos = 1:length(ev), ev)

    ggplot(data = df, aes(x = pos, y = ev)) +
      geom_point() +
      geom_line() +
      xlab("Component Number") + ylab("Eigen Value") +
      scale_x_continuous(breaks = 1:length(ev)) +
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

  # ** Output Scree plot ######
  output$scree_plot <- renderPlot({
    scree_plotInput()
  })

  ############################
  # TRADITIONAL ANALYSIS #####
  ############################
  # * ITEM ANALYSIS #####
  # ** Item Difficulty/Discrimination Graph ######
  difplotInput <- reactive({
    correct <- correct_answ()
    DDplot(correct)
  })

  output$difplot <- renderPlot({
    difplotInput()
  })

  output$DP_difplot <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = difplotInput(), device = "png", height=4, width=14, dpi=120)
    }
  )

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
  itemexamInput<-reactive({
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
  })

  output$itemexam <- renderTable({
    itemexamInput()
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
  hist_distractor_by_groupInput <- reactive({
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

  output$hist_distractor_by_group <- renderPlot({
    hist_distractor_by_groupInput()
  })

  output$DP_hist_distractor_by_group <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = hist_distractor_by_groupInput(), device = "png", height=3, width=9, dpi=160)
    }
  )

  # ** Distractors Plot #####
  grafInput <- reactive({
    a <- test_answers()
    k <- test_key()

    multiple.answers <- c(input$type_combinations_distractor == "Combinations")
    plotDistractorAnalysis(data = a, key = k, num.group = input$gr, item = input$distractorSlider,
                           multiple.answers = multiple.answers)
  })

  grafReportInput<-reactive({
    a <- test_answers()
    k <- test_key()

    multiple.answers <- c(input$type_combinations_distractor == "Combinations")

    graflist<-list()

    for (i in 1:length(k)) {
      g<-plotDistractorAnalysis(data = a, key = k, num.group = input$gr, item = i,
                             multiple.answers = multiple.answers)
      g=g+ggtitle(paste("\nDistractor Plot for Item", i))
      g=ggplotGrob(g)
      graflist[[i]]=g
    }
    graflist
  })

  output$graf <- renderPlot({
    grafInput()
  })

  output$DP_graf <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = grafInput(), device = "png", height=3, width=9, dpi=160)
    }
  )

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
  logregInput <- reactive({
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

  output$logreg <- renderPlot({
    logregInput()
  })

  output$DP_logreg <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = logregInput(), device = "png", height=3, width=9, dpi=160)
    }
  )
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

  zlogregInput <- reactive({
    scaledsc = scale(scored_test())

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

  output$zlogreg <- renderPlot({
    zlogregInput()
  })

  output$DP_zlogreg <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = zlogregInput(), device = "png", height=3, width=9, dpi=160)
    }
  )

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
  # ** Model ####
  z_logistic_irt_reg <- reactive({
    scaledsc <- c(scale(scored_test()))
    model <- glm(correct_answ()[, input$zlogreg_irtSlider] ~ scaledsc, family = "binomial")
  })

  # ** Plot with estimated logistic curve ####
  zlogreg_irtInput <- reactive({
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
                    args = list(b0 = coef(z_logistic_irt_reg())[1],
                                b1 = coef(z_logistic_irt_reg())[2]),
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

  output$zlogreg_irt <- renderPlot({
    zlogreg_irtInput()
  })

  output$DP_zlogreg_irt <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = zlogreg_irtInput(), device = "png", height=3, width=9, dpi=160)
    }
  )


  # ** Table of parameters ####
  output$zlogregtab_irt <- renderTable({
    fit <- z_logistic_irt_reg()
    tab_coef_old <- coef(fit)

    # delta method
    g <- list( ~ x2,  ~ -x1/x2)
    cov <- vcov(fit)
    cov <- as.matrix(cov)
    syms <- paste("x", 1:2, sep = "")
    for (i in 1:2) assign(syms[i], tab_coef_old[i])
    gdashmu <- t(sapply(g, function(form) {
      as.numeric(attr(eval(deriv(form, syms)), "gradient"))
      # in some shiny v. , envir = parent.frame() in eval() needs to be added
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
    fit <- z_logistic_irt_reg()

    b1 <- summary(fit)$coef[2, 1]
    b1 <- round(b1, 2)
    b0 <- round(summary(fit)$coef[1, 1], 2)

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
        algorithm = "port", start = start[i, ],
        lower = c(-20, -20, 0), upper = c(20, 20, 1)
      )

    estim_klasik1
  })

  nlsplotInput <- reactive({
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

  output$nlsplot <- renderPlot({
    nlsplotInput()
  })

  output$DP_nlsplot <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = nlsplotInput(), device = "png", height=3, width=9, dpi=160)
    }
  )

  # Table of parameters
  output$nonlinearztab <- renderTable({
    fit <- nls_model()

    tab <- summary(fit)$parameters[, 1:2]
    colnames(tab) <- c("Estimate", "SD")
    tab
  },
  include.rownames = T,
  include.colnames = T
  )

  # ** Interpretation ####
  output$nonlinearint <- renderUI({
    fit <- nls_model()

    a <- round(summary(fit)$coef[1, 1], 2)
    b <- round(summary(fit)$coef[2, 1], 2)

    txt1 <- paste ("<b>", "Interpretation:", "</b>")
    txt2 <- paste (
      "A one-unit increase in the z-score (one SD increase in original scores) is associated
      with the increase in the log odds of answering the item correctly vs. not correctly
      in the amount of "
    )
    txt3 <- paste ("<b>", a, "</b>")
    HTML(paste(txt1, txt2, txt3))
  })

  # * MODEL COMPARISON ######
  regr_comp_tableInput <- reactive({
    Data <- correct_answ()
    scaledsc <- c(scale(scored_test()))

    m <- ncol(Data)

    regFce_noDIF <- deriv3(
      ~ c + (1 - c) / (1 + exp(-a * (x - b))),
      namevec = c("a", "b", "c"),
      function.arg = function(x, a, b, c) {}
    )

    Q3 <- cut(scaledsc, quantile(scaledsc, (0:3) / 3),
              c("I", "II", "III"),
              include.lowest = TRUE)

    x <- cbind(mean(scaledsc[Q3 == "I"]),
               apply(Data[Q3 == "I",], 2, mean))
    y <- cbind(mean(scaledsc[Q3 == "III"]),
               apply(Data[Q3 == "III",], 2, mean))
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


    start <- cbind(discr, diffi, guess)
    colnames(start) <- c("a", "b", "c")

    # fit2PL <- lapply(1:m, function(i) glm(Data[, i] ~ scaledsc, family = "binomial"))

    fit2PL <- lapply(1:m, function(i) nls(Data[, i] ~  regFce_noDIF(scaledsc, a, b, c = 0),
                                           algorithm = "port", start = start[i, 1:2],
                                           lower = c(-Inf, -Inf),
                                           upper = c(Inf, Inf)))

    fit3PL <- lapply(1:m, function(i) nls(Data[, i] ~  regFce_noDIF(scaledsc, a, b, c),
                                           algorithm = "port", start = start[i,],
                                           lower = c(-Inf, -Inf, 0),
                                           upper = c(Inf, Inf, 1)))

    bestAIC <- ifelse(sapply(fit2PL, AIC) < sapply(fit3PL, AIC), "2PL", "3PL")
    bestBIC <- ifelse(sapply(fit2PL, BIC) < sapply(fit3PL, BIC), "2PL", "3PL")

    LRstat <- -2 * (sapply(fit2PL, logLik) - sapply(fit3PL, logLik))
    LRdf <- 1
    LRpval <- 1 - pchisq(LRstat, LRdf)
    LRpval <- p.adjust(LRpval, method = input$correction_method_regrmodels)
    bestLR <- ifelse(LRpval < 0.05, "3PL", "2PL")


    tab <- rbind(sprintf("%.2f", round(sapply(fit2PL, AIC), 2)),
                 sprintf("%.2f", round(sapply(fit3PL, AIC), 2)),
                 sprintf("%.2f", round(sapply(fit2PL, BIC), 2)),
                 sprintf("%.2f", round(sapply(fit3PL, BIC), 2)),
                 sprintf("%.2f", round(LRstat, 3)),
                 ifelse(round(LRpval, 3) < 0.001, "<0.001", sprintf("%.3f", round(LRpval, 3))),
                 bestAIC,
                 bestBIC,
                 bestLR)

    rownames(tab) <- c("AIC 2PL", "AIC 3PL", "BIC 2PL", "BIC 3PL", "LR X2", "LR p-value",
                       "BEST AIC", "BEST BIC", "BEST LR")

    tab
  })

  # Table to print
  output$regr_comp_table <- renderTable({
    tab <- regr_comp_tableInput()
    tab
  },
  include.rownames = T,
  include.colnames = T
  )

  # * MULTINOMIAL ######
  # ** Model ####
  multinomial_model <- reactive({
    stotal <- c(scale(scored_test()))
    k <- t(as.data.frame(test_key()))
    i <- input$multiSlider

    fitM <- multinom(relevel(as.factor(test_answers()[, i]),
                             ref = paste(k[i])) ~ stotal,
                     trace = F)
    fitM
  })

  # ** Plot with estimated curves of multinomial regression ####
  multiplotInput <- reactive({

    k <- t(as.data.frame(test_key()))
    data <- test_answers()
    stotal <- c(scale(scored_test()))
    i <- input$multiSlider

    fitM <- multinom(relevel(as.factor(data[, i]),
                             ref = paste(k[i])) ~ stotal,
                     trace = F)

    pp <- fitted(fitM)
    stotals <- rep(stotal, length(levels(relevel(as.factor(data[, i]),
                                                 ref = paste(k[i])))))
    df <- cbind(melt(pp), stotals)
    df2 <- data.frame(table(data[, i], stotal),
                      y = data.frame(prop.table(table(data[, i], stotal), 2))[, 3])
    df2$stotal <- as.numeric(levels(df2$stotal))[df2$stotal]
    df2$Var2 <- relevel(df2$Var1, ref = paste(k[i]))


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
      labs(title = paste("Item", i),
           x = "Standardized Total Score",
           y = "Probability of Answer") +
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

  multiplotReportInput<-reactive({
    graflist=list()

    for (i in 1:length(test_key())) {
      k <- t(as.data.frame(test_key()))

      stotal <- c(scale(scored_test()))


      fitM <- multinom(relevel(as.factor(test_answers()[, i]),
                               ref = paste(k[i])) ~ stotal,
                       trace = F)

      pp <- fitted(fitM)

      stotals <- rep(stotal, length(levels(relevel(as.factor(test_answers()[, i]),
                                                   ref = paste(k[i])))))
      df <- cbind(melt(pp), stotals)


      df2 <- data.frame(table(test_answers()[, i], stotal),
                        y = data.frame(prop.table(table(test_answers()[, i], stotal), 2))[, 3])
      df2$stotal <- as.numeric(levels(df2$stotal))[df2$stotal]
      df2$Var2 <- relevel(df2$Var1, ref = paste(k[i]))


      g<-ggplot() +
        geom_line(data = df,
                  aes(x = stotals , y = value,
                      colour = Var2, linetype = Var2), size = 1) +
        geom_point(data = df2,
                   aes(x = stotal, y = y,
                       colour = Var2, fill = Var2,
                       size = Freq),
                   alpha = 0.5, shape = 21) +

        ylim(0, 1) +
        labs(title = paste("Item", i),
             x = "Standardized Total Score",
             y = "Probability of Answer") +
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
      g=g+ggtitle(paste("\nMultinomial Plot for Item", i))
      g=ggplotGrob(g)
      graflist[[i]]=g
    }
    graflist
  })

  output$multiplot <- renderPlot({
    multiplotInput()
  })

  output$DP_multiplot <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = multiplotInput(), device = "png", height=3, width=9, dpi=160)
    }
  )

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
    fit <- multinomial_model()

    koef <- as.vector(coef(fit))
    std  <- as.vector(sqrt(diag(vcov(fit))))
    tab  <- cbind(koef, std)
    colnames(tab) <- c("Estimate", "SD")
    rownames(tab) <- c(paste("b", rownames(coef(fit)), "0", sep = ""),
                       paste("b", rownames(coef(fit)), "1", sep = ""))
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
  raschInput <- reactive({
    plot(rasch_model())
    g<-recordPlot()
    plot.new()
    g
  })

  output$rasch <- renderPlot({
    raschInput()
  })

  output$DP_rasch <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      png(file, height=800, width=1200, res=100)
      plot(rasch_model())
      dev.off()
    }
  )

  # *** IIC ####
  raschiicInput<-reactive({
    plot(rasch_model(), type = "IIC")
    g<-recordPlot()
    plot.new()
    g
  })

  output$raschiic <- renderPlot({
    raschiicInput()
  })

  output$DP_raschiic <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      png(file, height=800, width=1200, res=100)
      plot(rasch_model(), type = "IIC")
      dev.off()
      }
  )

  # *** TIF ####
  raschtifInput<-reactive({
    plot(rasch_model(), items = 0, type = "IIC")
    g<-recordPlot()
    plot.new()
    g
  })

  output$raschtif <- renderPlot({
    raschtifInput()
  })

  output$DP_raschtif <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      png(file, height=800, width=1200, res=100)
      plot(rasch_model(),items = 0, type = "IIC")
      dev.off()
    }
  )


  # *** Table of parameters ####
  raschcoefInput<- reactive({
    tab <- coef(rasch_model())
    tab <- cbind(tab,
                 sqrt(diag(vcov(rasch_model())))[1:nrow(tab)],
                 rep(sqrt(diag(vcov(rasch_model())))[nrow(tab) + 1], nrow(tab)))
    tab <- tab[, c(2, 4, 1, 3)]
    colnames(tab) <- c("a", "SD(a)", "b", "SD(b)")
    rownames(tab) <- paste("Item", 1:nrow(tab))
    tab
  })

  output$raschcoef <- renderTable({
    raschcoefInput()
  },
  include.rownames = T)

  # *** Factor scores plot ####
  raschFactorInput <- reactive({
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
            # legend.margin = unit(0, "lines"),
            legend.box = "vertical",
            legend.key.size = unit(1, "lines"),
            legend.text.align = 0,
            legend.title.align = 0)
  })

  output$raschFactor <- renderPlot({
    raschFactorInput()
  })

  output$DP_raschFactor <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = raschFactorInput(), device = "png", height=3, width=9, dpi=160)
    }
  )


  # ** 2PL ####
  two_param_irt <- reactive({
    fit2PL <- ltm(correct_answ() ~ z1, IRT.param = TRUE)
  })

  # *** ICC ####
  twoparamInput<-reactive({
    plot(two_param_irt())
    g<-recordPlot()
    plot.new()
    g
  })

  output$twoparam <- renderPlot({
    twoparamInput()
  })

  output$DP_twoparam <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      png(file, height=800, width=1200, res=100)
      plot(two_param_irt())
      dev.off()
      }
  )

  # *** IIC ####
  twoparamiicInput<-reactive({
    plot(two_param_irt(), type = "IIC")
    g<-recordPlot()
    plot.new()
    g
  })

  output$twoparamiic <- renderPlot({
    twoparamiicInput()
  })

  output$DP_twoparamiic <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      png(file, height=800, width=1200, res=100)
      plot(two_param_irt(), type = "IIC")
      dev.off()
      }
  )

  # *** TIF ####
  twoparamtifInput<-reactive({
    plot(two_param_irt(), items = 0, type = "IIC")
    g<-recordPlot()
    plot.new()
    g
  })

  output$twoparamtif <- renderPlot({
    twoparamtifInput()
  })

  output$DP_twoparamtif <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      png(file, height=800, width=1200, res=100)
      plot(two_param_irt(), items = 0, type = "IIC")
      dev.off()
      }
  )

  # ** Table of parameters ####
  twoparamcoefInput <- reactive({
    fit2pl <- two_param_irt()
    tab <- coef(fit2pl)
    tab <- cbind(tab,
                 sqrt(diag(vcov(fit2pl)))[1:nrow(tab)],
                 sqrt(diag(vcov(fit2pl)))[(nrow(tab) + 1):(2 * nrow(tab))])
    tab <- tab[, c(2, 4, 1, 3)]
    colnames(tab) <- c("a", "SD(a)", "b", "SD(b)")
    rownames(tab) <- paste("Item", 1:nrow(tab))
    tab
  })

  output$twoparamcoef <- renderTable({
    twoparamcoefInput()
  },
  include.rownames = T)

  # *** Factor scores plot ####
  twoFactorInput <- reactive({
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
            # legend.margin = unit(0, "lines"),
            legend.box = "vertical",
            legend.key.size = unit(1, "lines"),
            legend.text.align = 0,
            legend.title.align = 0)
  })

  output$twoFactor <- renderPlot({
    twoFactorInput()
  })

  output$DP_twoFactor <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = twoFactorInput(), device = "png", height=3, width=9, dpi=160)
    }
  )

  # ** 3PL ####
  three_param_irt <- reactive({
    fit3PL <- tpm(correct_answ(), IRT.param = TRUE)
  })
  # ** ICC ####
  threeparamInput<-reactive({
    plot(three_param_irt())
    g<-recordPlot()
    plot.new()
    g
  })

  output$threeparam <- renderPlot({
    threeparamInput()
  })

  output$DP_threeparam <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      png(file, height=800, width=1200, res=100)
      plot(three_param_irt())
      dev.off()
      }
  )

  # *** IIC ####
  threeparamiicInput<-reactive({
    plot(three_param_irt(), type = "IIC")
    g<-recordPlot()
    plot.new()
    g
  })

  output$threeparamiic <- renderPlot({
    threeparamiicInput()
  })

  output$DP_threeparamiic <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      png(file, height=800, width=1200, res=100)
      plot(three_param_irt(), type = "IIC")
      dev.off()
    }
  )

  # *** TIF ####
  threeparamtifInput<-reactive({
    plot(three_param_irt(), items = 0, type = "IIC")
    g<-recordPlot()
    plot.new()
    g
  })

  output$threeparamtif <- renderPlot({
    threeparamtifInput()
  })

  output$DP_threeparamtif <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      png(file, height=800, width=1200, res=100)
      plot(three_param_irt(), items = 0, type = "IIC")
      dev.off()
    }
  )

  # *** Table of parameters ####
  threeparamcoefInput<-reactive({
    fit3pl <- tpm(correct_answ(), IRT.param = TRUE)
    tab <- coef(fit3pl)
    tab <- cbind(tab,
                 sqrt(diag(vcov(fit3pl)))[1:nrow(tab)],
                 sqrt(diag(vcov(fit3pl)))[(nrow(tab) + 1):(2 * nrow(tab))],
                 sqrt(diag(vcov(fit3pl)))[(2 * nrow(tab) + 1):(3 * nrow(tab))])
    tab <- tab[, c(3, 6, 2, 5, 1, 4)]
    colnames(tab) <- c("a", "SD(a)", "b", "SD(b)", "c", "SD(c)")
    rownames(tab) <- paste("Item", 1:nrow(tab))
    tab
  })

  output$threeparamcoef <- renderTable({
    threeparamcoefInput()
  },
  include.rownames = T)

  # *** Factor scores ####
  threeFactorInput <- reactive({
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
            # legend.margin = unit(0, "lines"),
            legend.box = "vertical",
            legend.key.size = unit(1, "lines"),
            legend.text.align = 0,
            legend.title.align = 0)
  })

  output$threeFactor <- renderPlot({
    threeFactorInput()
  })

  output$DP_threeFactor <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = threeFactorInput(), device = "png", height=3, width=9, dpi=160)
    }
  )

  ###############################
  # * IRT MODELS WITH MIRT ######
  ###############################

  # ** RASCH ####
  rasch_model_mirt <- reactive({
    fitRasch <- mirt(correct_answ(), model = 1, itemtype = "Rasch",
                     SE = T, verbose = F)
  })

  # *** CC ####
  raschInput_mirt <- reactive({
    plot(rasch_model_mirt(), type = "trace", facet_items = F)
    # g <- recordPlot()
    # plot.new()
    # g
  })

  output$rasch_mirt <- renderPlot({
    raschInput_mirt()
  })

  output$DP_rasch_mirt <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      png(file, height = 800, width = 1200, res = 100)
      plot(rasch_model_mirt(), type = "trace", facet_items = F)
      dev.off()
    }
  )

  # *** IIC ####
  raschiicInput_mirt <- reactive({
    plot(rasch_model_mirt(), type = "infotrace", facet_items = F)
    # g <- recordPlot()
    # plot.new()
    # g
  })

  output$raschiic_mirt <- renderPlot({
    raschiicInput_mirt()
  })

  output$DP_raschiic_mirt <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      png(file, height = 800, width = 1200, res = 100)
      plot(rasch_model_mirt(), type = "infotrace", facet_items = F)
      dev.off()
    }
  )

  # *** TIF ####
  raschtifInput_mirt <- reactive({
    plot(rasch_model_mirt(), type = "infoSE")
    # g <- recordPlot()
    # plot.new()
    # g
  })

  output$raschtif_mirt <- renderPlot({
    raschtifInput_mirt()
  })

  output$DP_raschtif_mirt <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      png(file, height = 800, width = 1200, res = 100)
      plot(rasch_model_mirt(), type = "infoSE")
      dev.off()
    }
  )


  # *** Table of parameters ####
  raschcoefInput_mirt <- reactive({
    fit <- rasch_model_mirt()
    coeftab <- coef(fit)
    tab <- cbind(sapply(1:(length(coeftab) - 1), function(i) coeftab[[i]][1, "d"]),
                 sqrt(diag(vcov(fit)))[1:(length(coeftab) - 1)])
    rownames(tab) <- paste("Item", 1:nrow(tab))
    tab <- round(tab, 3)

    itemfittab <- round(itemfit(fit)[, 2:4], 3)
    tab <- data.frame(tab, itemfittab)
    colnames(tab) <- c("b", "SD(b)", "SX2-value", "df", "p-value")

    tab
  })

  output$raschcoef_mirt <- renderTable({
    raschcoefInput_mirt()
  },
  include.rownames = T)

  # *** Factor scores correlation ####
  raschFactorCorInput_mirt <- reactive({
    fs <- as.vector(fscores(rasch_model_mirt()))
    sts <- as.vector(scale(apply(correct_answ(), 1, sum)))

    cor <- cor(fs, sts)
    cor
  })
  output$raschFactorCor_mirt <- renderText({
    paste("The pearson correlation coefficient between standardized total score (Z-score)
          and factor score estimated by IRT model is", round(raschFactorCorInput_mirt(), 3))
  })

  # *** Factor scores plot ####
  raschFactorInput_mirt <- reactive({

    fs <- as.vector(fscores(rasch_model_mirt()))
    sts <- as.vector(scale(apply(correct_answ(), 1, sum)))

    df <- data.frame(fs, sts)

    ggplot(df, aes_string("sts", "fs")) +
      geom_point(size = 3) +
      labs(x = "Standardized total score", y = "Factor score") +
      theme_bw() +
      theme(text = element_text(size = 14),
            plot.title = element_text(face = "bold", vjust = 1.5),
            axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank()) +
      theme(legend.box.just = "left",
            legend.justification = c(1, 0),
            legend.position = c(1, 0),
            legend.box = "vertical",
            legend.key.size = unit(1, "lines"),
            legend.text.align = 0,
            legend.title.align = 0)
  })

  output$raschFactor_mirt <- renderPlot({
    raschFactorInput_mirt()
  })

  output$DP_raschFactor_mirt <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = raschFactorInput_mirt(), device = "png",
             height = 3, width = 9, dpi = 160)
    }
  )


  # *** Wright Map ####
  raschWrightMapInput_mirt <- reactive({

    fs <- as.vector(fscores(rasch_model_mirt()))

    fit <- rasch_model_mirt()
    coeftab <- coef(fit)
    b <- sapply(1:(length(coeftab) - 1), function(i) coeftab[[i]][1, "d"])
    names(b) <- paste("Item", 1:(length(coeftab) - 1))

    wrightMap(fs, b, item.side = itemClassic)
  })

  output$raschWrightMap_mirt<- renderPlot({
    raschWrightMapInput_mirt()
  })
  output$DP_raschWM_mirt <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      png(file, height = 800, width = 1200, res = 100)
      raschWrightMapInput_mirt()
      dev.off()
    }
  )

  # ** 1PL IRT ####
  one_param_irt_mirt <- reactive({
    data <- correct_answ()
    fit1PL <- mirt(data, model = 1, itemtype = "2PL",
                   constrain = list((1:ncol(data)) + seq(0, (ncol(data) - 1)*3, 3)),
                   SE = T, verbose = F)
  })

  # *** CC ####
  oneparamirtInput_mirt <- reactive({
    plot(one_param_irt_mirt(), type = "trace", facet_items = F)
    # g <- recordPlot()
    # plot.new()
    # g
  })

  output$oneparamirt_mirt <- renderPlot({
    oneparamirtInput_mirt()
  })

  output$DP_oneparamirt_mirt <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      png(file, height = 800, width = 1200, res = 100)
      plot(one_param_irt_mirt(), type = "trace", facet_items = F)
      dev.off()
    }
  )

  # *** IIC ####
  oneparamirtiicInput_mirt <- reactive({
    plot(one_param_irt_mirt(), type = "infotrace", facet_items = F)
    # g <- recordPlot()
    # plot.new()
    # g
  })

  output$oneparamirtiic_mirt <- renderPlot({
    oneparamirtiicInput_mirt()
  })

  output$DP_oneparamirtiic_mirt <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      png(file, height = 800, width = 1200, res = 100)
      plot(one_param_irt_mirt(), type = "infotrace", facet_items = F)
      dev.off()
    }
  )

  # *** TIF ####
  oneparamirttifInput_mirt <- reactive({
    plot(one_param_irt_mirt(), type = "infoSE")
  })

  output$oneparamirttif_mirt <- renderPlot({
    oneparamirttifInput_mirt()
  })

  output$DP_oneparamirttif_mirt <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      png(file, height = 800, width = 1200, res = 100)
      plot(one_param_irt_mirt(), type = "infoSE")
      dev.off()
    }
  )


  # *** Table of parameters ####
  oneparamirtcoefInput_mirt <- reactive({
    fit <- one_param_irt_mirt()
    coeftab <- coef(fit)
    tab <- cbind(
      sapply(1:(length(coeftab) - 1), function(i) coeftab[[i]][1, "a1"]),
      rep(sqrt(diag(vcov(fit)))[1], (length(coeftab) - 1)),
      sapply(1:(length(coeftab) - 1), function(i) coeftab[[i]][1, "d"]),
                 sqrt(diag(vcov(fit)))[2:(length(coeftab))])
    rownames(tab) <- paste("Item", 1:nrow(tab))

    itemfittab <- itemfit(fit)[, 2:4]
    tab <- data.frame(tab, itemfittab)
    colnames(tab) <- c("a", "SD(a)", "b", "SD(b)", "SX2-value", "df", "p-value")

    tab <- round(tab, 3)
    tab
  })

  output$oneparamirtcoef_mirt <- renderTable({
    oneparamirtcoefInput_mirt()
  },
  include.rownames = T)


  # *** Factor scores correlation ####
  oneparamirtFactorCorInput_mirt <- reactive({

    fs <- as.vector(fscores(one_param_irt_mirt()))
    sts <- as.vector(scale(apply(correct_answ(), 1, sum)))

    cor <- cor(fs, sts)
    cor
  })
  output$oneparamirtFactorCor_mirt <- renderText({
    paste("The pearson correlation coefficient between standardized total score (Z-score)
          and factor score estimated by IRT model is", round(oneparamirtFactorCorInput_mirt(), 3))
  })
  # *** Factor scores plot ####
  oneparamirtFactorInput_mirt <- reactive({

    fs <- as.vector(fscores(one_param_irt_mirt()))
    sts <- as.vector(scale(apply(correct_answ(), 1, sum)))

    df <- data.frame(fs, sts)

    ggplot(df, aes_string("sts", "fs")) +
      geom_point(size = 3) +
      labs(x = "Standardized total score", y = "Factor score") +
      theme_bw() +
      theme(text = element_text(size = 14),
            plot.title = element_text(face = "bold", vjust = 1.5),
            axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank()) +
      theme(legend.box.just = "left",
            legend.justification = c(1, 0),
            legend.position = c(1, 0),
            legend.box = "vertical",
            legend.key.size = unit(1, "lines"),
            legend.text.align = 0,
            legend.title.align = 0)
  })

  output$oneparamirtFactor_mirt <- renderPlot({
    oneparamirtFactorInput_mirt()
  })

  output$DP_oneparamirtFactor_mirt <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = oneparamirtFactorInput_mirt(), device = "png",
             height = 3, width = 9, dpi = 160)
    }
  )

  # *** Wright Map ####
  oneparamirtWrightMapInput_mirt <- reactive({
    fit <- one_param_irt_mirt()
    fs <- as.vector(fscores(fit))

    coeftab <- coef(fit)
    b <- sapply(1:(length(coeftab) - 1), function(i) coeftab[[i]][1, "d"])
    names(b) <- paste("Item", 1:(length(coeftab) - 1))

    wrightMap(fs, b, item.side = itemClassic)
  })

  output$oneparamirtWrightMap_mirt<- renderPlot({
    oneparamirtWrightMapInput_mirt()
  })

  output$DP_oneparamirtWM_mirt <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      png(file, height = 800, width = 1200, res = 100)
      oneparamirtWrightMapInput_mirt()
      dev.off()
    }
  )

  # ** 2PL IRT ####
  two_param_irt_mirt <- reactive({
    data <- correct_answ()
    fit2PL <- mirt(data, model = 1, itemtype = "2PL",
                   constrain = NULL,
                   SE = T, verbose = F)
  })

  # *** CC ####
  twoparamirtInput_mirt <- reactive({
    plot(two_param_irt_mirt(), type = "trace", facet_items = F)
    # g <- recordPlot()
    # plot.new()
    # g
  })

  output$twoparamirt_mirt <- renderPlot({
    twoparamirtInput_mirt()
  })

  output$DP_twoparamirt_mirt <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      png(file, height = 800, width = 1200, res = 100)
      plot(two_param_irt_mirt(), type = "trace", facet_items = F)
      dev.off()
    }
  )

  # *** IIC ####
  twoparamirtiicInput_mirt <- reactive({
    plot(two_param_irt_mirt(), type = "infotrace", facet_items = F)
    # g <- recordPlot()
    # plot.new()
    # g
  })

  output$twoparamirtiic_mirt <- renderPlot({
    twoparamirtiicInput_mirt()
  })

  output$DP_twoparamirtiic_mirt <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      png(file, height = 800, width = 1200, res = 100)
      plot(two_param_irt_mirt(), type = "infotrace", facet_items = F)
      dev.off()
    }
  )

  # *** TIF ####
  twoparamirttifInput_mirt <- reactive({
    plot(two_param_irt_mirt(), type = "infoSE")
    # g <- recordPlot()
    # plot.new()
    # g
  })

  output$twoparamirttif_mirt <- renderPlot({
    twoparamirttifInput_mirt()
  })

  output$DP_twoparamirttif_mirt <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      png(file, height = 800, width = 1200, res = 100)
      plot(two_param_irt_mirt(), type = "infoSE")
      dev.off()
    }
  )


  # *** Table of parameters ####
  twoparamirtcoefInput_mirt <- reactive({
    fit <- two_param_irt_mirt()
    coeftab <- coef(fit)
    tab <- cbind(
      sapply(1:(length(coeftab) - 1), function(i) coeftab[[i]][1, "a1"]),
      sqrt(diag(vcov(fit)))[seq(1, (2*length(coeftab) - 2), 2)],
      sapply(1:(length(coeftab) - 1), function(i) coeftab[[i]][1, "d"]),
      sqrt(diag(vcov(fit)))[seq(2, (2*length(coeftab) - 2), 2)])

    rownames(tab) <- paste("Item", 1:nrow(tab))

    itemfittab <- itemfit(fit)[, 2:4]
    tab <- data.frame(tab, itemfittab)
    colnames(tab) <- c("a", "SD(a)", "b", "SD(b)", "SX2-value", "df", "p-value")

    tab <- round(tab, 3)
    tab
  })

  output$twoparamirtcoef_mirt <- renderTable({
    twoparamirtcoefInput_mirt()
  },
  include.rownames = T)
  # *** Factor scores correlation ####
  twoparamirtFactorCorInput_mirt <- reactive({

    fs <- as.vector(fscores(two_param_irt_mirt()))
    sts <- as.vector(scale(apply(correct_answ(), 1, sum)))

    cor <- cor(fs, sts)
    cor
  })

  output$twoparamirtFactorCor_mirt <- renderText({
    paste("The pearson correlation coefficient between standardized total score (Z-score)
          and factor score estimated by IRT model is", round(twoparamirtFactorCorInput_mirt(), 3))
  })
  # *** Factor scores plot ####
  twoparamirtFactorInput_mirt <- reactive({

    fs <- as.vector(fscores(two_param_irt_mirt()))
    sts <- as.vector(scale(apply(correct_answ(), 1, sum)))

    df <- data.frame(fs, sts)

    ggplot(df, aes_string("sts", "fs")) +
      geom_point(size = 3) +
      labs(x = "Standardized total score", y = "Factor score") +
      theme_bw() +
      theme(text = element_text(size = 14),
            plot.title = element_text(face = "bold", vjust = 1.5),
            axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank()) +
      theme(legend.box.just = "left",
            legend.justification = c(1, 0),
            legend.position = c(1, 0),
            legend.box = "vertical",
            legend.key.size = unit(1, "lines"),
            legend.text.align = 0,
            legend.title.align = 0)
  })

  output$twoparamirtFactor_mirt <- renderPlot({
    twoparamirtFactorInput_mirt()
  })

  output$DP_twoparamirtFactor_mirt <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = twoparamirtFactorInput_mirt(), device = "png",
             height = 3, width = 9, dpi = 160)
    }
  )

  # ** 3PL IRT ####
  three_param_irt_mirt <- reactive({
    data <- correct_answ()
    fit3PL <- mirt(data, model = 1, itemtype = "3PL",
                   constrain = NULL,
                   SE = T, technical = list(NCYCLES = 2000),
                   verbose = F)
  })

  # *** CC ####
  threeparamirtInput_mirt <- reactive({
    plot(three_param_irt_mirt(), type = "trace", facet_items = F)
    # g <- recordPlot()
    # plot.new()
    # g
  })

  output$threeparamirt_mirt <- renderPlot({
    threeparamirtInput_mirt()
  })

  output$DP_threeparamirt_mirt <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      png(file, height = 800, width = 1200, res = 100)
      plot(three_param_irt_mirt(), type = "trace", facet_items = F)
      dev.off()
    }
  )

  # *** IIC ####
  threeparamirtiicInput_mirt <- reactive({
    plot(three_param_irt_mirt(), type = "infotrace", facet_items = F)
    # g <- recordPlot()
    # plot.new()
    # g
  })

  output$threeparamirtiic_mirt <- renderPlot({
    threeparamirtiicInput_mirt()
  })

  output$DP_threeparamirtiic_mirt <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      png(file, height = 800, width = 1200, res = 100)
      plot(three_param_irt_mirt(), type = "infotrace", facet_items = F)
      dev.off()
    }
  )

  # *** TIF ####
  threeparamirttifInput_mirt <- reactive({
    plot(three_param_irt_mirt(), type = "infoSE")
    # g <- recordPlot()
    # plot.new()
    # g
  })

  output$threeparamirttif_mirt <- renderPlot({
    threeparamirttifInput_mirt()
  })

  output$DP_threeparamirttif_mirt <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      png(file, height = 800, width = 1200, res = 100)
      plot(three_param_irt_mirt(), type = "infoSE")
      dev.off()
    }
  )


  # *** Table of parameters ####
  threeparamirtcoefInput_mirt <- reactive({
    fit <- three_param_irt_mirt()
    coeftab <- coef(fit)
    tab <- cbind(
      sapply(1:(length(coeftab) - 1), function(i) coeftab[[i]][1, "a1"]),
      sqrt(diag(vcov(fit)))[seq(1, (3*length(coeftab) - 3), 3)],
      sapply(1:(length(coeftab) - 1), function(i) coeftab[[i]][1, "d"]),
      sqrt(diag(vcov(fit)))[seq(2, (3*length(coeftab) - 3), 3)],
      sapply(1:(length(coeftab) - 1), function(i) coeftab[[i]][1, "g"]),
      sqrt(diag(vcov(fit)))[seq(3, (3*length(coeftab) - 3), 3)]
    )
    rownames(tab) <- paste("Item", 1:nrow(tab))

    itemfittab <- itemfit(fit)[, 2:4]
    tab <- data.frame(tab, itemfittab)
    colnames(tab) <- c("a", "SD(a)", "b", "SD(b)", "c", "SD(c)", "SX2-value", "df", "p-value")

    tab <- round(tab, 3)
    tab
  })

  output$threeparamirtcoef_mirt <- renderTable({
    threeparamirtcoefInput_mirt()
  },
  include.rownames = T)



  # *** Factor scores plot ####
  threeparamirtFactorCorInput_mirt <- reactive({

    fs <- as.vector(fscores(three_param_irt_mirt()))
    sts <- as.vector(scale(apply(correct_answ(), 1, sum)))

    cor <- cor(fs, sts)
    cor
  })
  output$threeparamirtFactorCor_mirt <- renderText({
    paste("The pearson correlation coefficient between standardized total score (Z-score)
          and factor score estimated by IRT model is", round(threeparamirtFactorCorInput_mirt(), 3))
  })
  # *** Factor scores plot ####
  threeparamirtFactorInput_mirt <- reactive({

    fs <- as.vector(fscores(three_param_irt_mirt()))
    sts <- as.vector(scale(apply(correct_answ(), 1, sum)))

    df <- data.frame(fs, sts)

    ggplot(df, aes_string("sts", "fs")) +
      geom_point(size = 3) +
      labs(x = "Standardized total score", y = "Factor score") +
      theme_bw() +
      theme(text = element_text(size = 14),
            plot.title = element_text(face = "bold", vjust = 1.5),
            axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank()) +
      theme(legend.box.just = "left",
            legend.justification = c(1, 0),
            legend.position = c(1, 0),
            legend.box = "vertical",
            legend.key.size = unit(1, "lines"),
            legend.text.align = 0,
            legend.title.align = 0)
  })

  output$threeparamirtFactor_mirt <- renderPlot({
    threeparamirtFactorInput_mirt()
  })

  output$DP_threeparamirtFactor_mirt <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = threeparamirtFactorInput_mirt(), device = "png",
             height = 3, width = 9, dpi = 160)
    }
  )

  # ** IRT COMPARISON ####
  irtcomparisonInput <- reactive({
    fit1PL <- one_param_irt_mirt()
    fit2PL <- two_param_irt_mirt()
    fit3PL <- three_param_irt_mirt()

    models <- list(fit1PL = fit1PL,
                   fit2PL = fit2PL,
                   fit3PL = fit3PL)

    df <- data.frame(anova(models[[1]], models[[2]], verbose = F))
    df <- rbind(df,
                data.frame(anova(models[[2]], models[[3]], verbose = F)))
    df <- round(df[c(1, 2, 4), ], 3)
    nam <- c("1PL", "2PL", "3PL")

    if (all(df[, 8] > 0.05)){
      hv <- "1PL"
    } else {
      p <- which(df[, 8] < 0.05)
      hv <- nam[p[length(p)]]
    }

    df <- rbind(df,
                c(nam[sapply(1:4, function(i) which(df[, i] == min(df[, i])))],
                  rep("", 3),
                  hv))


    rownames(df) <- c(nam, "BEST")
    df
  })

  output$irtcomparison <- renderTable({
    irtcomparisonInput()
  },
  include.rownames = T)



  # ** BOCKS NOMINAL MODEL ####
  adj_data_bock <- reactive({
    a <- test_answers()
    k <- as.factor(test_key())

    m <- ncol(a)
    lev <- unlist(lapply(1:m, function(i) levels(a[, i])))
    lev <- c(lev, levels(k))
    lev <- unique(lev)
    lev_num <- as.numeric(as.factor(lev))


    lev_k_num <- sapply(1:length(levels(k)),
                    function(i) lev_num[levels(k)[i] == lev])

    lev_a_num <- lapply(1:m, function(i)
                                sapply(1:length(levels(a[, i])),
                                       function(j) lev_num[levels(a[, i])[j] == lev]))

    levels(k) <- lev_k_num
    k <- as.numeric(paste(k))


    for (i in 1:m){
      levels(a[, i]) <- lev_a_num[[i]]
      a[, i] <- as.numeric(paste(a[, i]))
    }

    list(data = a, key = k)
  })


  bock_irt_mirt <- reactive({
    data <- adj_data_bock()$data
    key <- adj_data_bock()$key


    sv <- mirt(data, 1, 'nominal', pars = 'values', verbose = F)
    head(sv)

    # set all values to 0 and estimated
    sv$value[grepl('ak', sv$name)] <- 0
    sv$est[grepl('ak', sv$name)] <- TRUE

    nms <- colnames(data)
    for(i in 1:length(nms)){

      #set highest category based on key fixed to 3
      pick <- paste0('ak', key[i] - 1)
      index <- sv$item == nms[i] & pick == sv$name
      sv[index, 'value'] <- 3
      sv[index, 'est'] <- FALSE

      # set arbitrary lowest category fixed at 0
      if(pick == 'ak0') pick2 <- 'ak3'
      else pick2 <- paste0('ak', key[i] - 2)
      index2 <- sv$item == nms[i] & pick2 == sv$name
      sv[index2, 'est'] <- FALSE
    }

    fit <- mirt(data, 1, 'nominal', pars = sv, SE = T, verbose = F)
    fit
  })

  # *** CC ####
  bock_CC_Input <- reactive({
    plot(bock_irt_mirt(), type = "trace", facet_items = F)
  })
  output$bock_CC <- renderPlot({
    bock_CC_Input()
  })
  output$DP_bock_CC <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      png(file, height = 800, width = 1200, res = 100)
      plot(bock_irt_mirt(), type = "trace", facet_items = F)
      dev.off()
    }
  )

  # *** IIC ####
  bock_IIC_Input <- reactive({
    plot(bock_irt_mirt(), type = "infotrace", facet_items = F)
    # g <- recordPlot()
    # plot.new()
    # g
  })
  output$bock_IIC <- renderPlot({
    bock_IIC_Input()
  })
  output$DP_bock_IIC <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      png(file, height = 800, width = 1200, res = 100)
      plot(bock_irt_mirt(), type = "infotrace", facet_items = F)
      dev.off()
    }
  )

  # *** TIF ####
  bock_TIF_Input <- reactive({
    plot(bock_irt_mirt(), type = "infoSE")
    # g <- recordPlot()
    # plot.new()
    # g
  })
  output$bock_TIF <- renderPlot({
    bock_TIF_Input()
  })
  output$DP_bock_TIF <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      png(file, height = 800, width = 1200, res = 100)
      plot(bock_irt_mirt(), type = "infoSE")
      dev.off()
    }
  )


  # *** Table of parameters ####
  output$bock_coef_warning <- renderText({
    fit <- bock_irt_mirt()

    coeftab <- coef(fit, printSE = T)
    m <- length(coeftab) - 1

    dims <- sapply(coeftab, dim)[, -(m+1)]
    print(length(unique(dims[2, ])))
    if (length(unique(dims[2, ])) == 1){
      hide("bock_coef_warning")
    } else {
      show("bock_coef_warning")
    }
    paste("Sorry, for this dataset table is not available!")

  })

  bock_coef_Input <- reactive({
    fit <- bock_irt_mirt()

    coeftab <- coef(fit, printSE = T)
    m <- length(coeftab) - 1

    dims <- sapply(coeftab, dim)[, -(m+1)]
    if (length(unique(dims[2, ])) == 1){
      partab <- t(sapply(1:m, function(i) coeftab[[i]][1, ]))
      setab <- t(sapply(1:m, function(i) coeftab[[i]][2, ]))
      n <- ncol(partab)
      tab <- c()
      for (i in 1:n){
        tab <- cbind(tab, partab[, i], setab[, i])
      }
      namPAR <- colnames(partab)
      namSE <- paste("SE(", colnames(partab), ")", sep = "")

      colnames(tab) <- c(sapply(1:n, function(i) c(namPAR[i], namSE[i])))
      rownames(tab) <- paste("Item", 1:m)
    } else {
      tab <- NULL
    }

    tab
  })

  output$bock_coef <- renderTable({
    bock_coef_Input()
  },
  include.rownames = T,
  include.colnames = T)

  # *** Factor scores plot ####
  bock_factor_Input <- reactive({

    fs <- as.vector(fscores(bock_irt_mirt()))
    sts <- as.vector(scale(apply(correct_answ(), 1, sum)))

    df <- data.frame(fs, sts)

    ggplot(df, aes_string("sts", "fs")) +
      geom_point(size = 3) +
      labs(x = "Standardized total score", y = "Factor score") +
      theme_bw() +
      theme(text = element_text(size = 14),
            plot.title = element_text(face = "bold", vjust = 1.5),
            axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank()) +
      theme(legend.box.just = "left",
            legend.justification = c(1, 0),
            legend.position = c(1, 0),
            legend.box = "vertical",
            legend.key.size = unit(1, "lines"),
            legend.text.align = 0,
            legend.title.align = 0)
  })
  output$bock_factor <- renderPlot({
    bock_factor_Input()
  })
  output$DP_bock_factor <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = bock_factor_Input(), device = "png",
             height = 3, width = 9, dpi = 160)
    }
  )



  ###################
  # DIF/FAIRNESS ####
  ###################
  # * TOTAL SCORES ####
  # ** Summary of Total Scores for Groups ####
  resultsgroupInput<-reactive({
    sc_one  <- scored_test()[DIF_groups() == 1]
    sc_zero <- scored_test()[DIF_groups() == 0]
    tab <- t(data.frame(round(c(min(sc_zero), max(sc_zero), mean(sc_zero), median(sc_zero),
                                sd(sc_zero), skewness(sc_zero), kurtosis(sc_zero)), 2),
                        round(c(min(sc_one), max(sc_one), mean(sc_one), median(sc_one),
                                sd(sc_one), skewness(sc_one), kurtosis(sc_one)), 2)))
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

  # ** Histogram of total score for group = 1 (focal) ####
  histbyscoregroup1Input <- reactive({

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

    g<-ggplot(df, aes(x = sc)) +
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
    g
  })

  output$histbyscoregroup1 <- renderPlot ({
    histbyscoregroup1Input()
  })

  output$DP_histbyscoregroup1 <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = histbyscoregroup1Input(), device = "png", height=3, width=9, dpi=160)
    }
  )

  # ** Histogram of total score for group = 0 (reference) ####
  histbyscoregroup0Input <- reactive ({

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
    g<-ggplot(df, aes(x = sc)) +
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
    g
  })

  output$histbyscoregroup0 <- renderPlot ({
    histbyscoregroup0Input()
  })

  output$DP_histbyscoregroup0 <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = histbyscoregroup0Input(), device = "png", height=3, width=9, dpi=160)
    }
  )

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
  deltaplotInput <- reactive({
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
    p=p+ggtitle("Delta Plot")
    p
  })

  output$deltaplot <- renderPlot({
   deltaplotInput()
  })

  output$DP_deltaplot <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = deltaplotInput(), device = "png", height=3, width=9, dpi=160)
    }
  )

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
                       type = input$type_plot_DIF_logistic,
                       p.adjust.method = input$correction_method_logItems)
    mod
  })

  # ** Model for print ####
  model_DIF_logistic_print <- reactive({
    group <- DIF_groups()
    data <- correct_answ()

    mod <- difLogistic(Data = data, group = group, focal.name = 1,
                       type = input$type_print_DIF_logistic,
                       p.adjust.method = input$correction_method_logSummary)
    mod
  })

  # ** Output print ####
  output$print_DIF_logistic <- renderPrint({
    print(model_DIF_logistic_print())
  })

  # ** Plot ####
  plot_DIF_logisticInput <- reactive({
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

  output$plot_DIF_logistic <- renderPlot({
   plot_DIF_logisticInput()
  })

  output$DP_plot_DIF_logistic <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = plot_DIF_logisticInput(), device = "png", height=3, width=9, dpi=160)
    }
  )

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

  DIF_logistic_plotReport<-reactive({
    group <- DIF_groups()
    data <- correct_answ()

    mod <- difLogistic(Data = data, group = group, focal.name = 1,
                       type = input$type_print_DIF_logistic, p.adjust.method = input$correction_method_logItems)
    mod$DIFitems
    graflist = list()
    if (mod$DIFitems[1]!="No DIF item detected") {
      for (i in 1:length(mod$DIFitems)) {
        g<-plotDIFLogistic(data, group,
                            type = input$type_plot_DIF_logistic,
                            item =  mod$DIFitems[i],
                            IRT = F,
                            p.adjust.method = input$correction_method_logItems
            )
        g=g+ggtitle(paste0("DIF Logistic Plot for Item ", mod$DIFitems[i]))
        #g=ggplotGrob(g)
        graflist[[i]]<-g
      }
    } else {
      graflist=0
    }
    graflist
  })

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
                       p.adjust.method = input$correction_method_logZSummary,
                       all.cov = T)
    mod
  })

  # ** Output print ####
  output$print_DIF_logistic_IRT_Z <- renderPrint({
    print(model_DIF_logistic_IRT_Z_print())
  })

  # ** Plot ####
  plot_DIF_logistic_IRT_ZInput <- reactive ({
    group <- DIF_groups()
    data <- correct_answ()

    type <- input$type_plot_DIF_logistic
    plotDIFLogistic(data, group,
                    type = input$type_plot_DIF_logistic_IRT_Z,
                    item =  input$diflog_irtSlider,
                    IRT = T,
                    p.adjust.method = input$correction_method_logZItems)
  })

  output$plot_DIF_logistic_IRT_Z <- renderPlot({
    plot_DIF_logistic_IRT_ZInput()
  })

  output$DP_plot_DIF_logistic_IRT_Z <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = plot_DIF_logistic_IRT_ZInput(), device = "png", height=3, width=9, dpi=160)
    }
  )

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


  # * NLR DIF ####
  # ** Model for print ####
  model_DIF_NLR_print <- reactive({
    group <- DIF_groups()
    data <- correct_answ()

    type <- input$type_print_DIF_NLR
    adj.method <- input$correction_method_nlrSummary
    model <- "3PLcg"

    mod <- difNLR(Data = data, group = group, focal.name = 1,
                  model = model, type = type,
                  p.adjust.method = adj.method)
    mod
  })

  # ** Output print ####
  output$print_DIF_NLR <- renderPrint({
    print(model_DIF_NLR_print())
  })

  # ** Model for plot ####
  model_DIF_NLR_plot <- reactive({
    group <- DIF_groups()
    data <- correct_answ()

    type <- input$type_plot_DIF_NLR
    adj.method <- input$correction_method_nlrItems
    model <- "3PLcg"

    mod <- difNLR(Data = data, group = group, focal.name = 1,
                  model = model, type = type,
                  p.adjust.method = adj.method)
    mod
  })

  # ** Plot ####
  plot_DIF_NLRInput <- reactive({
    fit <- model_DIF_NLR_plot()
    item <- input$difnlrSlider

    plot(fit, item = item)[[1]] +
      theme(text = element_text(size = 14),
            plot.title = element_text(size = 14, face = "bold",
                                      vjust = 1.5))
  })

  # ** Output plot ####
  output$plot_DIF_NLR <- renderPlot({
    plot_DIF_NLRInput()
  })

  # ** Plot download ####
  output$DP_plot_DIF_NLR <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = plot_DIF_NLRInput(), device = "png",
             height = 3, width = 9, dpi = 160)
    }
  )

 # ** Table of coefficients ####
  output$tab_coef_DIF_NLR <- renderTable({
    item <- input$difnlrSlider
    fit <- model_DIF_NLR_plot()

    tab_coef <- fit$nlrPAR[item, c("a", "b", "aDif", "bDif", "c")]
    tab_sd <- fit$nlrSE[item, c("a", "b", "aDif", "bDif", "c")]

    tab <- t(rbind(tab_coef, tab_sd))

    rownames(tab) <- c('a', 'b', 'aDIF', 'bDIF', 'c')
    colnames(tab) <- c("Estimate", "SD")

    tab
  },
  include.rownames = T)

  # * IRT LORD ####
  # ** Model for plot ####
  model_DIF_IRT_Lord_plot <- reactive({
    group <- DIF_groups()
    data <- correct_answ()

    if (input$type_plot_DIF_IRT_lord == "3PL"){
      guess <- itemPar3PL(data)[, 3]
    }

    mod <- switch(input$type_plot_DIF_IRT_lord,
                  "1PL" = difLord(Data = data, group = group, focal.name = 1,
                                  model = "1PL",
                                  p.adjust.method = input$correction_method_DIF_IRT_lordItems),
                  "2PL" = difLord(Data = data, group = group, focal.name = 1,
                                  model = "2PL",
                                  p.adjust.method = input$correction_method_DIF_IRT_lordItems),
                  "3PL" = difLord(Data = data, group = group, focal.name = 1,
                                  model = "3PL", c = guess,
                                  p.adjust.method = input$correction_method_DIF_IRT_lordItems))
    mod
  })

  # ** Model for print ####
  model_DIF_IRT_Lord_print <- reactive({
    group <- DIF_groups()
    data <- correct_answ()

    if (input$type_print_DIF_IRT_lord == "3PL"){
      guess <- itemPar3PL(data)[, 3]
    }

    mod <- switch(input$type_print_DIF_IRT_lord,
                  "1PL" = difLord(Data = data, group = group, focal.name = 1,
                                  model = "1PL",
                                  p.adjust.method = input$correction_method_DIF_IRT_lordSummary),
                  "2PL" = difLord(Data = data, group = group, focal.name = 1,
                                  model = "2PL",
                                  p.adjust.method = input$correction_method_DIF_IRT_lordSummary),
                  "3PL" = difLord(Data = data, group = group, focal.name = 1,
                                  model = "3PL", c = guess,
                                  p.adjust.method = input$correction_method_DIF_IRT_lordSummary))
    mod
  })

  # ** Output print ####
  output$print_DIF_IRT_Lord <- renderPrint({
    print(model_DIF_IRT_Lord_print())
  })


  # ** Plot ####
  plot_DIF_IRT_LordInput <- reactive({
    fitLord <- model_DIF_IRT_Lord_plot()
    plotDIFirt(parameters = fitLord$itemParInit,
               item = input$difirt_lord_itemSlider)
  })

  output$plot_DIF_IRT_Lord <- renderPlot({
    plot_DIF_IRT_LordInput()
  })

  output$DP_plot_DIF_IRT_Lord <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = plot_DIF_IRT_LordInput(), device = "png", height=3, width=9, dpi=160)
    }
  )

  # ** Table with coefficients ####
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

  # ** Interpretation ####
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

  # ** Equation ####
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

    if (input$type_plot_DIF_IRT_raju == "3PL"){
      guess <- itemPar3PL(data)[, 3]
    }

    mod <- switch(input$type_plot_DIF_IRT_raju,
                  "1PL" = difRaju(Data = data, group = group, focal.name = 1,
                                  model = "1PL",
                                  p.adjust.method = input$correction_method_DIF_IRT_rajuItems),
                  "2PL" = difRaju(Data = data, group = group, focal.name = 1,
                                  model = "2PL",
                                  p.adjust.method = input$correction_method_DIF_IRT_rajuItems),
                  "3PL" = difRaju(Data = data, group = group, focal.name = 1,
                                  model = "3PL", c = guess,
                                  p.adjust.method = input$correction_method_DIF_IRT_rajuItems))
    mod
  })

  # ** Model for print ####
  model_DIF_IRT_Raju_print <- reactive({
    group <- DIF_groups()
    data <- correct_answ()

    if (input$type_print_DIF_IRT_raju == "3PL"){
      guess <- itemPar3PL(data)[, 3]
    }

    mod <- switch(input$type_print_DIF_IRT_raju,
                  "1PL" = difRaju(Data = data, group = group, focal.name = 1,
                                  model = "1PL",
                                  p.adjust.method = input$correction_method_DIF_IRT_rajuSummary),
                  "2PL" = difRaju(Data = data, group = group, focal.name = 1,
                                  model = "2PL",
                                  p.adjust.method = input$correction_method_DIF_IRT_rajuSummary),
                  "3PL" = difRaju(Data = data, group = group, focal.name = 1,
                                  model = "3PL", c = guess,
                                  p.adjust.method = input$correction_method_DIF_IRT_rajuSummary))
    mod
  })

  # ** Output print ####
  output$print_DIF_IRT_Raju <- renderPrint({
    print(model_DIF_IRT_Raju_print())
  })



  # ** Plot ####
  plot_DIF_IRT_RajuInput <- reactive({
    fitRaju <- model_DIF_IRT_Raju_plot()
    plotDIFirt(parameters = fitRaju$itemParInit, test = "Raju",
               item = input$difirt_raju_itemSlider)
  })

  output$plot_DIF_IRT_Raju <- renderPlot({
    plot_DIF_IRT_RajuInput()
  })

  output$DP_plot_DIF_IRT_Raju <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = plot_DIF_IRT_RajuInput(), device = "png", height=3, width=9, dpi=160)
    }
  )

  # ** Interpretation ####
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


  # ** Equation  ####
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

  # ** Table with coefficients ####
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

  # ** Table with coefficients output ####
  output$tab_coef_DIF_IRT_Raju <- renderTable({
    tab_coef_DIF_IRT_Raju()
  },
  include.rownames = T,
  include.colnames = T)

  # * DDF ####
  # ** Model for print ####
  model_DDF_print <- reactive({
    group <- DIF_groups()
    a <- test_answers()
    k <- test_key()

    adj.method <- input$correction_method_print_DDF
    type <- input$type_print_DDF

    mod <- ddfMLR(Data = a, group = group, focal.name = 1,
                  key = k, p.adjust.method = adj.method,
                  type = type)

    mod
  })

  # ** Output print ####
  output$print_DDF <- renderPrint({
    print(model_DDF_print())
  })

  # ** Model for plot ####
  model_DDF_plot <- reactive({
    group <- DIF_groups()
    a <- test_answers()
    k <- test_key()

    adj.method <- input$correction_method_plot_DDF
    type <- input$type_plot_DDF

    mod <- ddfMLR(Data = a, group = group, focal.name = 1,
                  key = k, p.adjust.method = adj.method,
                  type = type)

    mod
  })

  # ** Plot ####
  plot_DDFInput <- reactive({
    fit <- model_DDF_plot()
    item <- input$ddfSlider

    plot(fit, item = item)
  })

  plot_DDFReportInput<-reactive({
    group <- DIF_groups()
    a <- test_answers()
    k <- test_key()

    adj.method <- input$correction_method_plot_DDF
    type <- input$type_plot_DDF

    mod <- ddfMLR(Data = a, group = group, focal.name = 1,
                  key = k, p.adjust.method = adj.method,
                  type = type)

    graflist = list()
   # if (mod$DIFitems[[1]]!="No DDF item detected"){
      for (i in 1:length(mod$DDFitems)) {
        g<-plot(mod, item = mod$DDFitems[[i]], title = paste("\nDDF Multinomial Plot for Item", i))
        #g=ggplotGrob(g)
        graflist[[i]]<-g
      }
    #} else {
     # graflist=0
    #}
    graflist
  })


  # ** Output Plot ####
  output$plot_DDF <- renderPlot({
    plot_DDFInput()
  })

  # ** Plot download ####
  output$DP_plot_DDF <- downloadHandler(
    filename =  function() {
      paste("plot", input$name, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = plot_DDFInput(), device = "png",
             height = 3, width = 9, dpi = 160)
    }
  )

  # ** Table of coefficients ####
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

  # ** Equation ####
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


  # DOWNLOADN REPORT #####
  formatInput<-reactive({
    format<-input$report_format
    format
  })

  irt_typeInput<-reactive({
    type=input$irt_type_report
    type
  })

  irtInput<-reactive({
    type = input$irt_type_report

    if (type=="1pl") {out=raschInput()}
    if (type=="2pl") {out=twoparamInput()}
    if (type=="3pl") {out=threeparamInput()}
    out
  })

  irtiicInput<-reactive({
    type = input$irt_type_report

    if (type=="1pl") {out=raschiicInput()}
    if (type=="2pl") {out=twoparamiicInput()}
    if (type=="3pl") {out=threeparamiicInput()}
    out
  })

  irttifInput<-reactive({
    type = input$irt_type_report

    if (type=="1pl") {out=raschtifInput()}
    if (type=="2pl") {out=twoparamtifInput()}
    if (type=="3pl") {out=threeparamtifInput()}
    out
  })

  irtcoefInput<-reactive({
    type = input$irt_type_report

    if (type=="1pl") {out=raschcoefInput()}
    if (type=="2pl") {out=twoparamcoefInput()}
    if (type=="3pl") {out=threeparamcoefInput()}
    out
  })

  irtfactorInput<-reactive({
    type = input$irt_type_report

    if (type=="1pl") {out=raschFactorInput()}
    if (type=="2pl") {out=twoFactorInput()}
    if (type=="3pl") {out=threeFactorInput()}
    out
  })

  groupPresent<-reactive({
    if (length(dataset$group)>1) {
      groupLogical=TRUE
    } else {
      groupLogical=FALSE
    }
    groupLogical
  })

  output$report<-downloadHandler(
    filename=reactive({paste0("report.", input$report_format)}),
    content=function(file) {

      reportPath <- file.path(getwd(), paste0("report", formatInput(),".Rmd"))
      #file.copy("report.Rmd", tempReport, overwrite = TRUE)
      parameters<-list(a = test_answers(),
                       k = test_key(),
                       results = t(resultsInput()),
                       histogram_totalscores = histogram_totalscoresInput(),
                       corr_plot = corr_plotInput(),
                       scree_plot = scree_plotInput(),
                       difPlot = difplotInput(),
                       itemexam = itemexamInput(),
                       hist_distractor_by_group = hist_distractor_by_groupInput(),
                       graf = grafReportInput(),
                       logreg = logregInput(),
                       zlogreg = zlogregInput(),
                       zlogreg_irt = zlogreg_irtInput(),
                       nlsplot = nlsplotInput(),
                       multiplot = multiplotReportInput(),
                       irt_type = irt_typeInput(),
                       irt = irtInput(),
                       irtiic = irtiicInput(),
                       irttif = irttifInput(),
                       irtcoef = irtcoefInput(),
                       irtfactor = irtfactorInput(),
                       isGroupPresent = groupPresent(),
                       resultsgroup = {if (groupPresent()) {resultsgroupInput()}},
                       histbyscoregroup0 = {if (groupPresent()) {histbyscoregroup0Input()}},
                       histbyscoregroup1 = {if (groupPresent()) {histbyscoregroup1Input()}},
                       deltaplot = {if (groupPresent()) {deltaplotInput()}},
                       DP_text_normal = {if (groupPresent()) {deltaGpurn()}},
                       DIF_logistic_plot = {if (groupPresent()) {DIF_logistic_plotReport()}},
                       DIF_logistic_print = {if (groupPresent()) {model_DIF_logistic_print()}},
                       plot_DIF_logistic = {if (groupPresent()) {plot_DIF_logisticInput()}},
                       plot_DIF_logistic_IRT_Z = {if (groupPresent()) {plot_DIF_logistic_IRT_ZInput()}},
                       #plot_DIF_NLR = {if (groupPresent()) {plot_DIF_NLRInput()}},
                       plot_DIF_IRT_Lord = {if (groupPresent()) {plot_DIF_IRT_LordInput()}},
                       plot_DIF_IRT_Raju = {if (groupPresent()) {plot_DIF_IRT_RajuInput()}},
                       model_DDF_print = {if (groupPresent()) {model_DDF_print()}},
                       plot_DDFReportInput = {if (groupPresent()) {plot_DDFReportInput()}}
                       )
      rmarkdown::render(reportPath, output_file=file,
                        params = parameters, envir = new.env(parent = globalenv()))
    }
  )


}
