#%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# GLOBAL LIBRARY ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%

require(corrplot)
require(CTT)
require(deltaPlotR)
require(DT)
require(data.table)
require(difNLR)
require(difR)
require(ggplot2)
require(grid)
require(gridExtra)
require(knitr)
require(latticeExtra)
require(ltm)
require(mirt)
require(moments)
require(msm)
require(nnet)
require(psych)
require(psychometric)
require(reshape2)
require(rmarkdown)
require(ShinyItemAnalysis)
require(shinyjs)
require(stringr)
require(WrightMap)
require(xtable)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# DATA ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# data('GMAT', package = 'difNLR')
# data('GMATtest', package = 'difNLR')
# data('GMATkey', package = 'difNLR')
# test <- data.table(get("GMATtest"))
# key <- data.table(get("GMATkey"))

# maximum upload size set to 30MB
options(shiny.maxRequestSize = 30*1024^2)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# FUNCTIONS ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SERVER SCRIPT ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function(input, output, session) {

  dataset <- reactiveValues()

  dataset$answers <- NULL
  dataset$key <- NULL
  dataset$group <- NULL

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ### HITS COUNTER ######
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  output$counter <- renderText({
    if (!file.exists("counter.Rdata"))
    {counter <- 0}
    else {load(file = "counter.Rdata")}
    counter <- counter + 1
    save(counter, file = "counter.Rdata")
    paste0("Hits:", counter)
  })

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # DATA ADJUSTMENT ######
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  # LOAD ABCD DATA ######
  test_answers <- reactive ({
    if (is.null(input$data)) {
      a = input$dataSelect
      pos = regexpr("_", a)[1]
      datasetName = str_sub(a, 1, pos - 1)
      packageName = str_sub(a, pos + 1)

      do.call(data, args = list(paste0(datasetName, "test"), package = packageName))
      test = get(paste0(datasetName, "test"))

      # do.call(data, args = list(paste0(datasetName, "key"), package = packageName))
      # key = get(paste0(datasetName, "key"))
      key = test_key()

      test = test[, 1:length(key)]
      dataset$answers = test
    } else {
      test = dataset$answers
    }
    data.table(test)
  })

  # LOAD KEY ######
  test_key <- reactive({
    if ((is.null(input$key)) | (is.null(dataset$key))) {
      a = input$dataSelect
      pos = regexpr("_", a)[1]
      datasetName = str_sub(a, 1, pos - 1)
      packageName = str_sub(a, pos + 1)

      do.call(data, args = list(paste0(datasetName, "key"), package = packageName))
      key = get(paste0(datasetName, "key"))
      key = unlist(key)
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
      key = dataset$key
    }
    unlist(key)
  })

  # LOAD GROUPS ######
  DIF_groups <- reactive({
    if (is.null(input$data) | (is.null(dataset$group))) {
      a = input$dataSelect
      pos = regexpr("_", a)[1]
      datasetName = str_sub(a, 1, pos - 1)
      packageName = str_sub(a, pos + 1)

      do.call(data, args = list(paste0(datasetName, "test"), package = packageName))
      test = data.table(get(paste0(datasetName, "test")))

      if (datasetName == "GMAT"){
        group <- test[, "group"]
      } else {
        if (datasetName == "dataMedical"){
          group <- test[, "gender"]
        } else {
          group <- test[, ncol(test), with = FALSE]
        }
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
          need(nrow(dataset$answers) == length(dataset$group),
            "Length of group is not the same as number of observation in the main data set!"),
          errorClass = "error_dimension"
        )
      }
      group = dataset$group
    }
    group
  })

  # LOAD CRITERION VARIABLE ######
  criterion_variable <- reactive({
    if (is.null(input$data) | (is.null(dataset$criterion_variable))) {
      a = input$dataSelect
      pos = regexpr("_", a)[1]
      datasetName = str_sub(a, 1, pos - 1)
      packageName = str_sub(a, pos + 1)

      do.call(data, args = list(paste0(datasetName, "test"), package = packageName))
      test = data.table(get(paste0(datasetName, "test")))

      if (datasetName == "GMAT" | datasetName == "dataMedical"){
        criterion_variable <- test[, "criterion"]
      } else {
        criterion_variable <- "missing"
      }

      dataset$criterion_variable = criterion_variable

      validate(
        need(dataset$criterion_variable != "missing",
             "Sorry, for this dataset criterion variable is not available!"),
        errorClass = "warning_criterion_variable_missing"
      )
    } else {
      if (length(dataset$criterion_variable) == 1){
        if (dataset$criterion_variable == "missing"){
          validate(
            need(dataset$criterion_variable != "missing",
                 "Criterion variable is missing! Predictive validity analysis is not available!"),
            errorClass = "warning_criterion_variable_missing"
          )
        }
      } else {
        validate(
          need(nrow(dataset$answers) == length(dataset$criterion_variable),
               "Length of criterion variable is not the same as number of observation in the main data set!"),
          errorClass = "error_dimension"
        )
      }
      criterion_variable = dataset$criterion_variable
    }
    unlist(criterion_variable)
  })

  # LOADING DATA FROM CSV ######
  observeEvent(
    eventExpr = input$submitButton,
    handlerExpr = {
      key = NULL
      answ = NULL
      k = NULL
      group = NULL
      criterion_variable = NULL

      if (is.null(input$data)){
        key <- test_key()
        answ <- test[ , 1:length(key), with = FALSE]
        group <- DIF_groups()
        criterion_variable <- criterion_variable()
      } else {
        answ <- read.csv(input$data$datapath, header = input$header,
                         sep = input$sep, quote = input$quote)
        if (is.null(input$key)){
          key <- "missing"
        } else {
          key <- read.csv(input$key$datapath, header = input$header,
                          sep = input$sep)
          key <- as.character(unlist(key))
        }
        if (is.null(input$groups)){
          group <- "missing"
        } else {
          group <- read.csv(input$groups$datapath, header = input$header,
                            sep = input$sep)
          group <- unlist(group)
        }
        if (is.null(input$criterion_variable)){
          criterion_variable <- "missing"
        } else {
          criterion_variable <- read.csv(input$criterion_variable$datapath, header = input$header,
                                         sep = input$sep)
          criterion_variable <- unlist(criterion_variable)
        }
      }
      dataset$answers <- data.table(answ)
      dataset$key <- key
      dataset$group <- group
      dataset$criterion_variable <- criterion_variable
    }
  )

  # ITEM NUMBERS AND NAMES ######
  item_numbers <- reactive({
    if (!input$itemnam){
      nam <- 1:ncol(test_answers())
    } else {
      nam <- colnames(test_answers())
    }
    nam
  })
  item_names <- reactive({
    if (!input$itemnam){
      nam <- paste("Item", 1:ncol(test_answers()))
    } else {
      nam <- colnames(test_answers())
    }
    nam
  })

  # CORRECT ANSWER CLASSIFICATION ######
  correct_answ <- reactive({
    test <- test_answers()
    key <- unlist(test_key())

    df.key <- data.table(matrix(rep(key, dim(test)[1]),
                                ncol = dim(test)[2], nrow = dim(test)[1], byrow = T))
    correct <- data.table(matrix(as.numeric(test == df.key),
                          ncol = dim(test)[2], nrow = dim(test)[1]))
    if (!(input$missval)){
      correct[is.na(correct)] <- 0
    }
    colnames(correct) <- item_names()
    correct
  })

  # TOTAL SCORE CALCULATION ######
  scored_test <- reactive({
    sc <- apply(correct_answ(), 1, sum)
    sc
  })

  # DATA ######
  DPdata <- reactive ({
    dataset <- data.table(correct_answ(), DIF_groups())
    colnames(dataset) <- c(item_names(), 'group')
    dataset
  })

  # DATA HEAD ######
  output$headdata_print<-renderTable({
    data_table <- test_answers()
    colnames(data_table) <- item_names()
    head(data_table)
  })

  output$headdata <- DT::renderDataTable({
  # output$headdata <- shiny::renderDataTable({
    data_table <- test_answers()
    colnames(data_table) <- item_names()
    data_table
  },
  rownames = F,
  options = list(scrollX = TRUE,
                 pageLength = 6,
                 server = TRUE,
                 scrollCollapse = TRUE,
                 dom = 'tipr'))

  # KEY CONTROL ######

  output$key_print<-renderTable({
    key_table <- as.data.table(t(test_key()))
    colnames(key_table) <- item_names()
    key_table
  })

  output$key <- DT::renderDataTable({
  # output$key <- shiny::renderDataTable({
    key_table <- as.data.table(t(test_key()))
    colnames(key_table) <- item_names()
    key_table
  },
  rownames = F,
  options = list(scrollX = TRUE,
                 server = TRUE,
                 scrollCollapse = TRUE,
                 dom = 'tipr'))

  # SCORE 0-1 ######
  output$sc01_print<-renderTable({
    # total score
    sc <- data.table(scored_test())
    # scored data
    correct <- correct_answ()

    scored_table <- data.table(correct, sc)
    colnames(scored_table) <- c(item_names(), "Score")
    head(scored_table)
  })

  output$sc01 <- DT::renderDataTable({
  # output$sc01 <- shiny::renderDataTable({
    # total score
    sc <- data.table(scored_test())
    # scored data
    correct <- correct_answ()

    scored_table <- data.table(correct, sc)
    colnames(scored_table) <- c(item_names(), "Score")
    scored_table
  },
  rownames = F,
  options = list(scrollX = TRUE,
                 pageLength = 6,
                 server = TRUE,
                 scrollCollapse = TRUE,
                 dom = 'tipr'))

  # GROUP CONTROL ######
  output$group_print<-renderTable({
    group_table <- t(DIF_groups())
    colnames(group_table) <- 1:ncol(group_table)
    group_table
  })

  output$group <- DT::renderDataTable({
  # output$group <- shiny::renderDataTable({
    group_table <- t(DIF_groups())
    colnames(group_table) <- 1:ncol(group_table)
    group_table
  },
  rownames = F,
  options = list(scrollX = TRUE,
                 server = TRUE,
                 scrollCollapse = TRUE,
                 dom = 'tipr'))

  # CRITERION VARIABLE CONTROL ######
  output$critvar_print<-renderTable({
    critvar_table <- t(criterion_variable())
    colnames(critvar_table) <- 1:ncol(critvar_table)
    critvar_table
  })

  output$critvar <- DT::renderDataTable({
  # output$critvar <- shiny::renderDataTable({
    critvar_table <- t(criterion_variable())
    colnames(critvar_table) <- 1:ncol(critvar_table)
    critvar_table
  },
  rownames = F,
  options = list(scrollX = TRUE,
                 server = TRUE,
                 scrollCollapse = TRUE,
                 dom = 'tipr'))

  ##### ITEM SLIDERS ######
  observe({
    sliderList<-c(
      "validitydistractorSlider",
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

    updateSliderInput(session = session, inputId = "validitydistractorSlider", max = itemCount)
    updateSliderInput(session = session, inputId = "distractorSlider", max = itemCount)
    updateSliderInput(session = session, inputId = "logregSlider", max = itemCount)
    updateSliderInput(session = session, inputId = "zlogregSlider", max = itemCount)
    updateSliderInput(session = session, inputId = "zlogreg_irtSlider", max = itemCount)
    updateSliderInput(session = session, inputId = "nlsSlider", max = itemCount)
    updateSliderInput(session = session, inputId = "multiSlider", max = itemCount)
    updateSliderInput(session = session, inputId = "difMHSlider_item", max = itemCount)
    updateSliderInput(session = session, inputId = "diflogSlider", max = itemCount)
    updateSliderInput(session = session, inputId = "diflog_irtSlider", max = itemCount)
    updateSliderInput(session = session, inputId = "difnlrSlider", max = itemCount)
    updateSliderInput(session = session, inputId = "difirt_lord_itemSlider", max = itemCount)
    updateSliderInput(session = session, inputId = "difirt_raju_itemSlider", max = itemCount)
    updateSliderInput(session = session, inputId = "ddfSlider", max = itemCount)
    updateSliderInput(session = session, inputId = "inSlider2", max = itemCount,
                      value = round(median(scored_test(), na.rm = T)))
    updateSliderInput(session = session, inputId = "inSlider2group", max = itemCount,
                      value = round(median(scored_test()[DIF_groups() == 1], na.rm = T)))
    updateSliderInput(session = session, inputId = "difMHSlider_score", max = itemCount,
                      value = round(median(scored_test(), na.rm = T)))

  })

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # SUMMARY ######
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  source("Summary.R", local = T)

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # VALIDITY ######
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  source("Validity.R", local = T)

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # TRADITIONAL ANALYSIS ######
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  source("TraditionalAnalysis.R", local = T)

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # REGRESSION ######
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  source("Regression.R", local = T)

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # IRT MODELS WITH MIRT ######
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  source("IRT.R", local = T)

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # DIF/FAIRNESS ######
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  source("DIF.R", local = T)

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # REPORTS ######
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
    if (type=="rasch"){out=raschInput_mirt()}
    if (type=="1pl") {out=oneparamirtInput_mirt()}
    if (type=="2pl") {out=twoparamirtInput_mirt()}
    if (type=="3pl") {out=threeparamirtInput_mirt()}
    if (type=="none") {out=""}

    out
  })

  irtiicInput<-reactive({
    type = input$irt_type_report
    if (type=="rasch"){out=raschiicInput_mirt()}
    if (type=="1pl") {out=oneparamirtiicInput_mirt()}
    if (type=="2pl") {out=twoparamirtiicInput_mirt()}
    if (type=="3pl") {out=threeparamirtiicInput_mirt()}
    if (type=="none") {out=""}

    out
  })

  irttifInput<-reactive({
    type = input$irt_type_report
    if (type=="rasch"){out=raschtifInput_mirt()}
    if (type=="1pl") {out=oneparamirttifInput_mirt()}
    if (type=="2pl") {out=twoparamirttifInput_mirt()}
    if (type=="3pl") {out=threeparamirttifInput_mirt()}
    if (type=="none") {out=""}

    out
  })

  irtcoefInput<-reactive({
    type = input$irt_type_report
    if (type=="rasch"){out=raschcoefInput_mirt()}
    if (type=="1pl") {out=oneparamirtcoefInput_mirt()}
    if (type=="2pl") {out=twoparamirtcoefInput_mirt()}
    if (type=="3pl") {out=threeparamirtcoefInput_mirt()}
    if (type=="none") {out=""}

    out
  })

  irtfactorInput<-reactive({
    type = input$irt_type_report
    if (type=="rasch"){out=raschFactorInput_mirt()}
    if (type=="1pl") {out=oneparamirtFactorInput_mirt()}
    if (type=="2pl") {out=twoparamirtFactorInput_mirt()}
    if (type=="3pl") {out=threeparamirtFactorInput_mirt()}
    if (type=="none") {out=""}

    out
  })

  groupPresent<-reactive({
    if (length(unlist(DIF_groups())) > 1) {
      groupLogical = TRUE
    } else {
      groupLogical = FALSE
    }
    groupLogical
  })



  observeEvent(input$generate, {
    withProgress(message = "Creating content", value = 0, style = "notification", {
    list(a = test_answers(),
         k = test_key(),
         # total scores
         incProgress(0.05),
         results = t(totalscores_table_Input()),
         histogram_totalscores = totalscores_histogram_Input(),
         incProgress(0.05),
         # correlation structure
         corr_plot = {if (input$corr_report != "none") {corr_plot_Input()} else {""}},
         scree_plot = {if (input$corr_report != "none") {scree_plot_Input()} else {""}},
         incProgress(0.05),
         # item analysis
         difPlot = DDplot_Input(),
         itemexam = itemanalysis_table_Input(),
         incProgress(0.05),
         # distractors
         hist_distractor_by_group = distractor_histogram_Input(),
         graf = report_distractor_plot(),
         incProgress(0.1),
         # regression
         logreg = logreg_plot_Input(),
         zlogreg = z_logreg_plot_Input(),
         zlogreg_irt = z_logreg_irt_plot_Input(),
         nlsplot = nlr_plot_Input(),
         multiplot = multiplotReportInput(),
         incProgress(0.15),
         # irt
         wrightMap = oneparamirtWrightMapReportInput_mirt(),
         irt_type = irt_typeInput(),
         irt = irtInput(),
         irtiic = irtiicInput(),
         irttif = irttifInput(),
         irtcoef = irtcoefInput(),
         irtfactor = irtfactorInput(),
         incProgress(0.25),
         # DIF
         isGroupPresent = groupPresent(),
         histCheck = input$histCheck,
         resultsgroup = {if (groupPresent()) {if (input$histCheck) {resultsgroupInput()}}},
         histbyscoregroup0 = {if (groupPresent()) {if (input$histCheck) {histbyscoregroup0Input()}}},
         histbyscoregroup1 = {if (groupPresent()) {if (input$histCheck) {histbyscoregroup1Input()}}},
         deltaplotCheck = input$deltaplotCheck,
         deltaplot = {if (groupPresent()) {if (input$deltaplotCheck) {deltaplotInput_report()}}},
         DP_text_normal = {if (groupPresent()) {if (input$deltaplotCheck) {deltaGpurn_report()}}},
         logregCheck = input$logregCheck,
         DIF_logistic_plot = {if (groupPresent()) {if (input$logregCheck) {DIF_logistic_plotReport()}}},
         DIF_logistic_print = {if (groupPresent()) {if (input$logregCheck) {model_DIF_logistic_print_report()}}},
         #plot_DIF_logistic = {if (groupPresent()) {plot_DIF_logisticInput()}},
         #plot_DIF_logistic_IRT_Z = {if (groupPresent()) {plot_DIF_logistic_IRT_ZInput()}},
         #plot_DIF_NLR = {if (groupPresent()) {plot_DIF_NLRInput()}},
         #plot_DIF_IRT_Lord = {if (groupPresent()) {plot_DIF_IRT_LordInput()}},
         #plot_DIF_IRT_Raju = {if (groupPresent()) {plot_DIF_IRT_RajuInput()}},
         multiCheck = input$multiCheck,
         model_DDF_print = {if (groupPresent()) {if (input$multiCheck) {model_DDF_print_report()}}},
         plot_DDFReportInput = {if (groupPresent()) {if (input$multiCheck) {plot_DDFReportInput()}}},
         incProgress(0.3)
    )
    })

    output$download_report_button <- renderUI({

      if(is.null(input$generate)){return(NULL)}
      downloadButton("report", "Download report")

    })

  })




  output$report <- downloadHandler(
    filename = reactive({paste0("report.", input$report_format)}),
    content = function(file) {

      reportPath <- file.path(getwd(), paste0("report", formatInput(), ".Rmd"))
      # file.copy("report.Rmd", tempReport, overwrite = TRUE)
      parameters<-list(a = test_answers(),
                       k = test_key(),
                       # total scores
                       results = t(totalscores_table_Input()),
                       histogram_totalscores = totalscores_histogram_Input(),
                       # correlation structure
                       corr_plot = {if (input$corr_report != "none") {corr_plot_Input()} else {""}},
                       scree_plot = {if (input$corr_report != "none") {scree_plot_Input()} else {""}},
                       # item analysis
                       difPlot = DDplot_Input(),
                       itemexam = itemanalysis_table_Input(),
                       # distractors
                       hist_distractor_by_group = distractor_histogram_Input(),
                       graf = report_distractor_plot(),
                       # regression
                       logreg = logreg_plot_Input(),
                       zlogreg = z_logreg_plot_Input(),
                       zlogreg_irt = z_logreg_irt_plot_Input(),
                       nlsplot = nlr_plot_Input(),
                       multiplot = multiplotReportInput(),
                       # irt
                       wrightMap = oneparamirtWrightMapReportInput_mirt(),
                       irt_type = irt_typeInput(),
                       irt = irtInput(),
                       irtiic = irtiicInput(),
                       irttif = irttifInput(),
                       irtcoef = irtcoefInput(),
                       irtfactor = irtfactorInput(),
                       # DIF
                       isGroupPresent = groupPresent(),
                       histCheck = input$histCheck,
                       resultsgroup = {if (groupPresent()) {if (input$histCheck) {resultsgroupInput()}}},
                       histbyscoregroup0 = {if (groupPresent()) {if (input$histCheck) {histbyscoregroup0Input()}}},
                       histbyscoregroup1 = {if (groupPresent()) {if (input$histCheck) {histbyscoregroup1Input()}}},
                       deltaplotCheck = input$deltaplotCheck,
                       deltaplot = {if (groupPresent()) {if (input$deltaplotCheck) {deltaplotInput_report()}}},
                       DP_text_normal = {if (groupPresent()) {if (input$deltaplotCheck) {deltaGpurn_report()}}},
                       logregCheck = input$logregCheck,
                       DIF_logistic_plot = {if (groupPresent()) {if (input$logregCheck) {DIF_logistic_plotReport()}}},
                       DIF_logistic_print = {if (groupPresent()) {if (input$logregCheck) {model_DIF_logistic_print_report()}}},
                       #plot_DIF_logistic = {if (groupPresent()) {plot_DIF_logisticInput()}},
                       #plot_DIF_logistic_IRT_Z = {if (groupPresent()) {plot_DIF_logistic_IRT_ZInput()}},
                       #plot_DIF_NLR = {if (groupPresent()) {plot_DIF_NLRInput()}},
                       #plot_DIF_IRT_Lord = {if (groupPresent()) {plot_DIF_IRT_LordInput()}},
                       #plot_DIF_IRT_Raju = {if (groupPresent()) {plot_DIF_IRT_RajuInput()}},
                       multiCheck = input$multiCheck,
                       model_DDF_print = {if (groupPresent()) {if (input$multiCheck) {model_DDF_print_report()}}},
                       plot_DDFReportInput = {if (groupPresent()) {if (input$multiCheck) {plot_DDFReportInput()}}}
      )
      rmarkdown::render(reportPath, output_file = file,
                        params = parameters, envir = new.env(parent = globalenv()))
    }
  )


}
