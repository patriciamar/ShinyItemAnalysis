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
require(plotly)
require(psych)
require(psychometric)
require(reshape2)
require(rmarkdown)
require(shiny)
require(shinyBS)
require(ShinyItemAnalysis)
require(shinyjs)
require(stringr)
require(WrightMap)
require(xtable)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# DATA ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# maximum upload size set to 30MB
options(shiny.maxRequestSize = 30*1024^2)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# FUNCTIONS ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# WrightMap
source("functions/wrightMap.R")
source("functions/itemClassic.R")
source("functions/personHist.R")
source("functions/theme_shiny.R")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SERVER SCRIPT ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function(input, output, session) {

  dataset <- reactiveValues()

  dataset$answers <- NULL
  dataset$key <- NULL
  dataset$group <- NULL
  dataset$criterion_variable <- NULL

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
        if (datasetName == "dataMedical" | datasetName == "HCI"){
          group <- test[, "gender"]
        } else {
          group <- test[, ncol(test), with = FALSE]
        }
      }

      dataset$group = group
    } else {
      if (length(dataset$group) == 1){
        if (any(dataset$group == "missing")){
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
    unlist(group)
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

      if (datasetName == "GMAT"){
        criterion_variable <- test[, "criterion"]
      } else {
        if (datasetName == "dataMedical"){
          criterion_variable <- test[, "StudySuccess"]
        } else {
          if (datasetName == "HCI"){
            criterion_variable <- test[, "major"]
          } else {
            criterion_variable <- "missing"
          }
        }
      }

      dataset$criterion_variable = criterion_variable

      validate(need(dataset$criterion_variable != "missing",
                    "Sorry, for this dataset criterion variable is not available!"),
               errorClass = "warning_criterion_variable_missing")


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
  checkDataText_Input <- eventReactive(input$submitButton, {
    error_setting <- F
    ### key
    error_key <- ""
    if (length(dataset$key) == 1){
      if (dataset$key == "missing"){
        error_key <- "The key need to be provided!"
      }
    } else {
      if (ncol(dataset$answers) != length(dataset$key)){
        error_key <- "The length of key need to be the same as number of columns of the main dataset!"
        error_setting <- T
      }
    }
    ### group
    error_group <- ""
    warni_group <- ""
    if (any(dataset$group == "missing")){
      warni_group <- "The group variable is not provided! DIF and DDF analyses are not available!"
    } else {
      if (nrow(dataset$answers) != length(dataset$group)){
        error_group <- "The length of group need to be the same as number of observations in the main dataset!"
        error_setting <- T
      }
    }
    ### criterion variable
    error_criterion_variable <- ""
    warni_criterion_variable <- ""
    if (any(dataset$criterion_variable == "missing")){
      warni_criterion_variable <- "The criterion variable is not provided! Predictive validity analysis is not available!"
    } else {
      if (nrow(dataset$answers) != length(dataset$group)){
        error_criterion_variable <- "The length of criterion variable need to be the same as number of observations in the main dataset!"
        error_setting <- T
      }
    }

    str_errors <- c(error_key, error_group, error_criterion_variable)
    str_warnin <- c(warni_group, warni_criterion_variable)

    if (any(str_warnin != "")){
      str_warnin <- str_warnin[str_warnin != ""]
      str_warnin <- paste("<font color = 'orange'> &#33;", str_warnin, "</font>", collapse = "<br>")
    }

    if(all(str_errors == "")){
      paste(c("<font color = 'green'> &#10004; Your data were successfully uploaded. Check them in <b>Data exploration</b> tab. </font>",
              str_warnin), collapse = "<br>")
    } else {
      str_errors <- str_errors[str_errors != ""]
      str_errors <- paste("<font color = 'red'> &#10006;", str_errors, "</font>", collapse = "<br>")
      if (error_setting){
        paste(c(str_errors,
                "<font color = 'red'> Check <b>Data exploration</b> tab to get idea what went wrong or try another
                <b>Data specification</b> below. </font>"),
              collapse = "<br>")
      } else {
        paste(str_errors)
      }
    }
  })
  output$checkDataText <- renderUI({
    HTML(checkDataText_Input())
  })


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
  # checking uploaded scored data
  checkDataColumns01Text_Input <- eventReactive(input$submitButton, {
    data <- correct_answ()
    # are there any items with only 0
    all0 <- apply(data, 2, function(x) all(x == 0))
    if (any(all0)){
      txt0 <- paste("It seems that",
                    item_names()[all0],
                    "consists only of zeros.")
    } else {
      txt0 <- ""
    }
    # are there any items with only 1
    all1 <- apply(data, 2, function(x) all(x == 1))
    if (any(all1)){
      txt1 <- paste("It seems that",
                    item_names()[all1],
                    "consists only of ones.")
    } else {
      txt1 <- ""
    }
    # warning
    if (any(all0) | any(all1)){
      txt <- paste(c("Check your data!",
                     paste(txt0, collapse = "<br>"),
                     paste(txt1, collapse = "<br>"),
                     "Some analyses may not work properly. Consider removing such items."),
                   collapse = "<br>")
      txt <- paste("<font color = 'red'>", txt, "</font>")
    } else {
      txt <- ""
    }

    txt
  })
  output$checkDataColumns01Text <- renderUI({
    HTML(checkDataColumns01Text_Input())
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
  output$headdata_print <- renderTable({
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
  output$key_print <- renderTable({
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
  output$sc01_print <- renderTable({
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
  output$group_print <- renderTable({
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
  output$critvar_print <- renderTable({
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
    sliderList <- c(
      "validitydistractorSlider",
      "distractorSlider",
      "logregSlider",
      "zlogregSlider",
      "zlogreg_irtSlider",
      "slider_nlr_3P_item",
      "slider_nlr_4P_item",
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
    updateSliderInput(session = session, inputId = "slider_totalscores_histogram", max = itemCount,
                      value = round(median(scored_test(), na.rm = T)))
    updateNumericInput(session = session, inputId = "corr_plot_clust", value = 1, max = itemCount)
    updateNumericInput(session = session, inputId = "corr_plot_clust_report", value = 1, max = itemCount)
    updateSliderInput(session = session, inputId = "validitydistractorSlider", max = itemCount)
    updateSliderInput(session = session, inputId = "distractorSlider", max = itemCount)
    updateSliderInput(session = session, inputId = "logregSlider", max = itemCount)
    updateSliderInput(session = session, inputId = "zlogregSlider", max = itemCount)
    updateSliderInput(session = session, inputId = "zlogreg_irtSlider", max = itemCount)
    updateSliderInput(session = session, inputId = "slider_nlr_3P_item", max = itemCount)
    updateSliderInput(session = session, inputId = "slider_nlr_4P_item", max = itemCount)
    updateSliderInput(session = session, inputId = "multiSlider", max = itemCount)
    updateSliderInput(session = session, inputId = "difMHSlider_item", max = itemCount)
    updateSliderInput(session = session, inputId = "diflogSlider", max = itemCount)
    updateSliderInput(session = session, inputId = "diflog_irtSlider", max = itemCount)
    updateSliderInput(session = session, inputId = "difnlrSlider", max = itemCount)
    updateSliderInput(session = session, inputId = "difirt_lord_itemSlider", max = itemCount)
    updateSliderInput(session = session, inputId = "difirt_raju_itemSlider", max = itemCount)
    updateSliderInput(session = session, inputId = "ddfSlider", max = itemCount)

    updateSliderInput(session = session, inputId = "inSlider2group", max = itemCount,
                      value = round(median(scored_test()[DIF_groups() == 1], na.rm = T)))
    updateSliderInput(session = session, inputId = "difMHSlider_score", max = itemCount,
                      value = round(median(scored_test(), na.rm = T)))

  })

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # SUMMARY ######
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  source("server/Summary.R", local = T)

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # VALIDITY ######
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  source("server/Validity.R", local = T)

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # TRADITIONAL ANALYSIS ######
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  source("server/TraditionalAnalysis.R", local = T)

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # REGRESSION ######
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  source("server/Regression.R", local = T)

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # IRT MODELS WITH MIRT ######
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  source("server/IRT.R", local = T)

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # DIF/FAIRNESS ######
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  source("server/DIF.R", local = T)

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # REPORTS ######
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  # update dataset name in Reports page
  dataName <- reactive({
    if (is.null(input$data)) {
      a <- input$dataSelect
      pos <- regexpr("_", a)[1]
      name <- str_sub(a, 1, pos - 1)
    } else {
        name <- ""
    }
    name
  })

  observe({
    updateTextInput(session = session,
                    inputId = "reportDataName",
                    value = paste(dataName(), "dataset"))
  })


  formatInput <- reactive({
    format <- input$report_format
    format
  })

  irt_typeInput <- reactive({
    type <- input$irt_type_report
    type
  })

  irtInput <- reactive({
    type = input$irt_type_report
    if (type == "rasch"){out = raschInput_mirt()}
    if (type == "1pl")  {out = oneparamirtInput_mirt()}
    if (type == "2pl")  {out = twoparamirtInput_mirt()}
    if (type == "3pl")  {out = threeparamirtInput_mirt()}
    if (type == "4pl")  {out = irt_4PL_icc_Input()}
    if (type == "none") {out = ""}

    out
  })

  irtiicInput <- reactive({
    type = input$irt_type_report
    if (type == "rasch"){out = raschiicInput_mirt()}
    if (type == "1pl")  {out = oneparamirtiicInput_mirt()}
    if (type == "2pl")  {out = twoparamirtiicInput_mirt()}
    if (type == "3pl")  {out = threeparamirtiicInput_mirt()}
    if (type == "4pl")  {out = irt_4PL_iic_Input()}
    if (type == "none") {out = ""}

    out
  })

  irttifInput <- reactive({
    type = input$irt_type_report
    if (type == "rasch"){out = raschtifInput_mirt()}
    if (type == "1pl")  {out = oneparamirttifInput_mirt()}
    if (type == "2pl")  {out = twoparamirttifInput_mirt()}
    if (type == "3pl")  {out = threeparamirttifInput_mirt()}
    if (type == "4pl")  {out = irt_4PL_tif_Input()}
    if (type == "none") {out = ""}

    out
  })

  irtcoefInput <- reactive({
    type = input$irt_type_report
    if (type == "rasch"){out = raschcoefInput_mirt()}
    if (type == "1pl")  {out = oneparamirtcoefInput_mirt()}
    if (type == "2pl")  {out = twoparamirtcoefInput_mirt()}
    if (type == "3pl")  {out = threeparamirtcoefInput_mirt()}
    if (type == "4pl")  {out = irt_4PL_coef_Input()}
    if (type == "none") {out = ""}

    out
  })

  irtfactorInput <- reactive({
    type = input$irt_type_report
    if (type == "rasch"){out = raschFactorInput_mirt()}
    if (type == "1pl")  {out = oneparamirtFactorInput_mirt()}
    if (type == "2pl")  {out = twoparamirtFactorInput_mirt()}
    if (type == "3pl")  {out = threeparamirtFactorInput_mirt()}
    if (type == "4pl")  {out = irt_4PL_factorscores_plot_Input()}
    if (type == "none") {out = ""}

    out
  })

  # ** Double slider inicialization for DD plot report ######
  observe({
    val <- input$DDplotNumGroupsSlider_report
    updateSliderInput(session, "DDplotRangeSlider_report",
                      min = 1,
                      max = val,
                      step = 1,
                      value = c(1, min(3, val)))
  })

  groupPresent <- reactive({
    (any(dataset$group != "missing") | is.null(dataset$group))
  })

  criterionPresent <- reactive({
    (any(dataset$criterion_variable != "missing") | is.null(dataset$criterion_variable))
  })


  observeEvent(input$generate, {
    withProgress(message = "Creating content", value = 0, style = "notification", {
      list(# header
           author = input$reportAuthor,
           dataset = input$reportDataName,
           # datasets
           a = test_answers(),
           k = test_key(),
           # total scores
           incProgress(0.05),
           results = t(totalscores_table_Input()),
           histogram_totalscores = totalscores_histogram_Input(),
           # standard scores
           standardscores_table = scores_tables_Input(),
           incProgress(0.05),
           # validity section
           corr_plot = {if (input$corr_report) {corr_plot_Input()} else {""}},
           corr_plot_numclust = ifelse(input$customizeCheck, input$corr_plot_clust_report, input$corr_plot_clust),
           corr_plot_clustmethod = ifelse(input$customizeCheck, input$corr_plot_clustmethod_report, input$corr_plot_clustmethod),
           scree_plot = {if (input$corr_report) {scree_plot_Input()} else {""}},
           isCriterionPresent = criterionPresent(),
           validity_check = input$predict_report,
           validity_plot = {if (input$predict_report) {if (criterionPresent()) {validity_plot_Input()} else {""}}},
           validity_table = {if (input$predict_report) {if (criterionPresent()) {validity_table_Input()} else {""}}},
           incProgress(0.05),
           # item analysis
           difPlot = DDplot_Input_report(),
           DDplotRange1 = ifelse(input$customizeCheck, input$DDplotRangeSlider_report[[1]], input$DDplotRangeSlider[[1]]),
           DDplotRange2 = ifelse(input$customizeCheck, input$DDplotRangeSlider_report[[2]], input$DDplotRangeSlider[[2]]),
           DDplotNumGroups = ifelse(input$customizeCheck, input$DDplotNumGroupsSlider_report, input$DDplotNumGroupsSlider),
           itemexam = itemanalysis_table_Input(),
           incProgress(0.05),
           # distractors
           hist_distractor_by_group = distractor_histogram_Input(),
           graf = report_distractor_plot(),
           incProgress(0.25),
           # regression
           logreg = logreg_plot_Input(),
           zlogreg = z_logreg_plot_Input(),
           zlogreg_irt = z_logreg_irt_plot_Input(),
           nlsplot = nlr_3P_plot_Input(),
           multiplot = multiplotReportInput(),
           incProgress(0.05),
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
           ### presence of group vector
           isGroupPresent = groupPresent(),
           ### histograms by group
           histCheck = input$histCheck,
           resultsgroup = {if (groupPresent()) {if (input$histCheck) {resultsgroupInput()}}},
           histbyscoregroup0 = {if (groupPresent()) {if (input$histCheck) {histbyscoregroup0Input()}}},
           histbyscoregroup1 = {if (groupPresent()) {if (input$histCheck) {histbyscoregroup1Input()}}},
           ### delta plot
           deltaplotCheck = input$deltaplotCheck,
           deltaplot = {if (groupPresent()) {if (input$deltaplotCheck) {deltaplotInput_report()}}},
           DP_text_normal = {if (groupPresent()) {if (input$deltaplotCheck) {deltaGpurn_report()}}},
           ### logistic regression
           logregCheck = input$logregCheck,
           DIF_logistic_plot = {if (groupPresent()) {if (input$logregCheck) {DIF_logistic_plotReport()}}},
           DIF_logistic_print = {if (groupPresent()) {if (input$logregCheck) {model_DIF_logistic_print_report()}}},
           ### DDF multinomial
           multiCheck = input$multiCheck,
           DDF_multinomial_print = {if (groupPresent()) {if (input$multiCheck) {model_DDF_print_report()}}},
           DDF_multinomial_plot = {if (groupPresent()) {if (input$multiCheck) {plot_DDFReportInput()}}},
           incProgress(0.25)
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
      parameters <- list(# header
                         author = input$reportAuthor,
                         dataset = input$reportDataName,
                         # datasets
                         a = test_answers(),
                         k = test_key(),
                         # total scores
                         results = t(totalscores_table_Input()),
                         histogram_totalscores = totalscores_histogram_Input(),
                         # standard scores
                         standardscores_table = scores_tables_Input(),
                         # validity section
                         corr_plot = {if (input$corr_report) {corr_plot_Input()} else {""}},
                         corr_plot_numclust = ifelse(input$customizeCheck, input$corr_plot_clust_report, input$corr_plot_clust),
                         corr_plot_clustmethod = ifelse(input$customizeCheck, input$corr_plot_clustmethod_report, input$corr_plot_clustmethod),
                         scree_plot = {if (input$corr_report) {scree_plot_Input()} else {""}},
                         isCriterionPresent = criterionPresent(),
                         validity_check = input$predict_report,
                         validity_plot = {if (input$predict_report) {if (criterionPresent()) {validity_plot_Input()} else {""}}},
                         validity_table = {if (input$predict_report) {if (criterionPresent()) {validity_table_Input()} else {""}}},
                         # item analysis
                         difPlot = DDplot_Input_report(),
                         DDplotRange1 = ifelse(input$customizeCheck, input$DDplotRangeSlider_report[[1]], input$DDplotRangeSlider[[1]]),
                         DDplotRange2 = ifelse(input$customizeCheck, input$DDplotRangeSlider_report[[2]], input$DDplotRangeSlider[[2]]),
                         DDplotNumGroups = ifelse(input$customizeCheck, input$DDplotNumGroupsSlider_report, input$DDplotNumGroupsSlider),
                         itemexam = itemanalysis_table_Input(),
                         # distractors
                         hist_distractor_by_group = distractor_histogram_Input(),
                         graf = report_distractor_plot(),
                         # regression
                         logreg = logreg_plot_Input(),
                         zlogreg = z_logreg_plot_Input(),
                         zlogreg_irt = z_logreg_irt_plot_Input(),
                         nlsplot = nlr_3P_plot_Input(),
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
                         ### presence of group vector
                         isGroupPresent = groupPresent(),
                         ### histograms by groups
                         histCheck = input$histCheck,
                         resultsgroup = {if (groupPresent()) {if (input$histCheck) {resultsgroupInput()}}},
                         histbyscoregroup0 = {if (groupPresent()) {if (input$histCheck) {histbyscoregroup0Input()}}},
                         histbyscoregroup1 = {if (groupPresent()) {if (input$histCheck) {histbyscoregroup1Input()}}},
                         ### delta plot
                         deltaplotCheck = input$deltaplotCheck,
                         DIF_deltaplot = {if (groupPresent()) {if (input$deltaplotCheck) {deltaplotInput_report()}}},
                         DIF_deltaplot_text = {if (groupPresent()) {if (input$deltaplotCheck) {deltaGpurn_report()}}},
                         ### logistic regression
                         logregCheck = input$logregCheck,
                         DIF_logistic_plot = {if (groupPresent()) {if (input$logregCheck) {DIF_logistic_plotReport()}}},
                         DIF_logistic_print = {if (groupPresent()) {if (input$logregCheck) {model_DIF_logistic_print_report()}}},
                         ### multinomial regression
                         multiCheck = input$multiCheck,
                         DDF_multinomial_print = {if (groupPresent()) {if (input$multiCheck) {model_DDF_print_report()}}},
                         DDF_multinomial_plot = {if (groupPresent()) {if (input$multiCheck) {plot_DDFReportInput()}}}
      )
      rmarkdown::render(reportPath, output_file = file,
                        params = parameters, envir = new.env(parent = globalenv()))
    }
  )


}
