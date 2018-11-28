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
require(ggdendro)
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
require(xtable)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# DATA ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# maximum upload size set to 30MB
options(shiny.maxRequestSize = 30*1024^2)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SERVER SCRIPT ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function(input, output, session) {

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ### REACTIVE VALUES ######
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  # * Datasets ####
  dataset <- reactiveValues()

  dataset$binary <- NULL
  dataset$ordinal <- NULL
  dataset$nominal <- NULL
  dataset$data_type <- NULL

  dataset$key <- NULL
  dataset$minimal <- NULL
  dataset$maximal <- NULL

  dataset$group <- NULL
  dataset$criterion <- NULL

  dataset$data_status <- NULL

  dataset$total_score <- NULL
  dataset$z_score <- NULL

  # * Setting ####
  setting_figures <- reactiveValues()

  setting_figures$text_size <- 12
  setting_figures$height <- 4
  setting_figures$width <- 8
  setting_figures$dpi <- 600

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
  # DATA UPLOAD ######
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  # * Load toy data ######
  observeEvent(input$dataSelect, {

    inputData <- input$dataSelect
    pos <- regexpr("_", inputData)[1]
    datasetName <- str_sub(inputData, 1, pos - 1)
    packageName <- str_sub(inputData, pos + 1)

    if (datasetName == "dataMedicalgraded") {
      do.call(data, args = list(paste0(datasetName), package = packageName))
      dataOrdinal <- get(paste0(datasetName))

      group <- dataOrdinal[, "gender"]
      criterion <- dataOrdinal[, "StudySuccess"]

      dataOrdinal <- dataOrdinal[, 1:(dim(dataOrdinal)[2] - 2)]
      dataNominal <- dataOrdinal

      dataType <- "ordinal"

      key <- sapply(dataOrdinal, max)
      df.key <- sapply(key, rep, each = nrow(dataOrdinal))
      dataBinary <- matrix(as.numeric(dataOrdinal >= df.key),
                           ncol = ncol(dataOrdinal), nrow = nrow(dataOrdinal))

    } else {
      do.call(data, args = list(paste0(datasetName, "test"), package = packageName))
      dataNominal <- get(paste0(datasetName, "test"))

      dataType <- "nominal"

      do.call(data, args = list(paste0(datasetName, "key"), package = packageName))
      key <- as.character(unlist(get(paste0(datasetName, "key"))))

      group <- dataNominal[, length(key) + 1]

      if (datasetName %in% c("GMAT2", "MSATB")){
        criterion <- "missing"
      } else {
        criterion <- dataNominal[, length(key) + 2]
      }

      dataNominal <- dataNominal[, 1:length(key)]
      dataOrdinal <- mirt::key2binary(dataNominal, key)
      dataBinary <- mirt::key2binary(dataNominal, key)
    }

    dataset$nominal <- as.data.table(dataNominal)
    dataset$ordinal <- as.data.table(dataOrdinal)
    dataset$binary <- as.data.table(dataBinary)

    dataset$data_type <- dataType

    if (input$data_type == "ordinal"){
      dataset$minimal <- sapply(dataset$ordinal, min)
      dataset$maximal <- sapply(dataset$ordinal, max)
    } else {
      dataset$minimal <- NULL
      dataset$maximal <- NULL
    }

    dataset$key <- key
    dataset$group <- group
    dataset$criterion <- criterion

    dataset$total_score <- apply(dataset$binary, 1, sum)
    dataset$z_score <- as.vector(scale(dataset$total_score))
  })

  # * Load data from csv files ####
  observeEvent(input$submitButton, {

      inputData <- NULL
      inputKey <- NULL
      inputGroup <- NULL
      inputCriterion <- NULL
      inputOrdinalMin <- NULL
      inputOrdinalMax <- NULL

      inputData_type <- input$data_type

      # loading main data
      if (is.null(input$data)){
        dataset$data_status <- "missing"
      } else {
        inputData <- read.csv(input$data$datapath,
                              header = input$header,
                              sep = input$sep,
                              quote = input$quote)
        dataset$data_status <- "OK"
      }

      # loading max/min values for ordinal data
      if (input$data_type == "ordinal"){
        ### minimal values
        if (is.null(input$minOrdinal)) {
          if (input$globalMin == "") {
            inputOrdinalMin <- sapply(inputData, min)
          } else {
            inputOrdinalMin <- rep(input$globalMin, ncol(inputData))
          }
        } else {
          inputOrdinalMin <- read.csv(input$minOrdinal$datapath,
                                      header = input$header,
                                      sep = input$sep,
                                      quote = input$quote)
        }

        ### maximal values
        if (is.null(input$maxOrdinal)) {
          if (input$globalMax == "") {
            inputOrdinalMax <- sapply(inputData, max)
          } else {
            inputOrdinalMax <- rep(input$globalMax, ncol(inputData))
          }
        } else {
          inputOrdinalMax <- read.csv(input$maxOrdinal$datapath,
                                      header = input$header,
                                      sep = input$sep,
                                      quote = input$quote)
        }
      }

      # loading key
      if (is.null(input$key)){
        if (input$globalCut == "") {
          if (input$data_type == "binary"){
            inputKey <- rep(1, ncol(inputData))
          } else {
            if (input$data_type == "ordinal"){
              inputKey <- inputOrdinalMax
            } else {
              inputKey <- "missing"
            }
          }
        } else {
          inputKey <- rep(as.numeric(input$globalCut), ncol(inputData))
        }
      } else {
        inputKey <- read.csv(input$key$datapath,
                             header = input$header,
                             sep = input$sep,
                             quote = input$quote)
        inputKey <- as.character(unlist(inputKey))
      }

      dataset$key <- inputKey

      # loading group
      if (is.null(input$groups)){
        inputGroup <- "missing"
        } else {
          inputGroup <- read.csv(input$groups$datapath,
                                 header = input$header,
                                 sep = input$sep,
                                 quote = input$quote)
          inputGroup <- unlist(inputGroup)
        }

      # loading criterion
      if (is.null(input$criterion_variable)){
          inputCriterion <- "missing"
        } else {
          inputCriterion <- read.csv(input$criterion_variable$datapath,
                                     header = input$header,
                                     sep = input$sep,
                                     quote = input$quote)
          inputCriterion <- unlist(inputCriterion)
        }

      # changing reactiveValues
      ### main data
      dataset$nominal <- inputData

      if (input$data_type == "nominal"){
        dataset$ordinal <- as.data.table(mirt::key2binary(dataset$nominal, inputKey))
        dataset$binary <- as.data.table(mirt::key2binary(dataset$nominal, inputKey))
      } else {
        if (input$data_type == "ordinal"){
          dataset$ordinal <- as.data.table(dataset$nominal)
          df.key <- sapply(inputKey, rep, each = nrow(inputData))
          dataset$binary <- as.data.table(matrix(as.numeric(inputData >= df.key),
                                                 ncol = ncol(inputData), nrow = nrow(inputData)))
        } else {
          dataset$ordinal <- as.data.table(dataset$nominal)
          dataset$binary <- as.data.table(dataset$nominal)
        }
      }

      dataset$nominal <- as.data.table(dataset$nominal)
      dataset$data_type <- inputData_type
      ### min/max values
      if (input$data_type == "ordinal"){
        dataset$minimal <- inputOrdinalMin
        dataset$maximal <- inputOrdinalMax
      } else {
        dataset$minimal <- NULL
        dataset$maximal <- NULL
      }
      ### group
      dataset$group <- inputGroup
      ### criterion
      dataset$criterion <- inputCriterion

      ### calculation of total score and z-score
      dataset$total_score <- apply(dataset$binary, 1, sum)
      dataset$z_score <- as.vector(scale(dataset$total_score))
    }
  )

  # * Creating reactive() for data and checking ####
  nominal <- reactive({
    # validate(need(dataset$data_status != "missing",
    #               "&#10006;No data found! Please, upload data. Selected toy dataset is still in use."),
    #          errorClass = "error_dimension")
    #
    # validate(need(dataset$nominal != "missing", "Please query data from server"))
    dataset$nominal
  })
  ordinal <- reactive({
    dataset$ordinal
  })
  binary <- reactive({
    dataset$binary
  })

  key <- reactive({
    if (length(dataset$key) == 1) {
      validate(need(dataset$key != "missing", "Key is missing!"),
               errorClass = "error_key_missing")
    } else {
      validate(need(ncol(dataset$nominal) == length(dataset$key),
                    "The length of key need to be the same as number of columns in the main dataset!"),
               errorClass = "error_dimension")
    }
    dataset$key
  })

  minimal <- reactive({
    ### bad minimal values dimension
    validate(need(ncol(dataset$nominal) == length(dataset$minimal),
                  "The length of minimal values need to be the same as number of items in the main dataset!"),
             errorClass = "error_dimension")
    dataset$minimal
  })
  maximal <- reactive({
    ### bad maximal values dimension
    validate(need(ncol(dataset$nominal) == length(dataset$maximal),
                  "The length of maximal values need to be the same as number of items in the main dataset!"),
             errorClass = "error_dimension")
    dataset$maximal
  })

  group <- reactive({
    ### bad group dimension and warning for missing group
    if (length(dataset$group) == 1 & any(dataset$group == "missing")){
      validate(need(dataset$group != "missing",
                    "Group is missing! DIF and DDF analyses are not available!"),
               errorClass = "warning_group_missing")
    } else {
      validate(need(nrow(dataset$nominal) == length(dataset$group),
                    "The length of group need to be the same as number of observation in the main dataset!"),
               errorClass = "error_dimension")
    }
    dataset$group
  })

  criterion <- reactive({
    ### bad criterion dimension and warning for missing criterion
    if (length(dataset$criterion) == 1 & any(dataset$criterion == "missing")){
      validate(need(dataset$criterion != "missing",
                    "Criterion variable is missing! Predictive validity analysis is not available!"),
               errorClass = "warning_criterion_variable_missing")
    } else {
      validate(need(nrow(dataset$nominal) == length(dataset$criterion),
                    "The length of criterion variable need to be the same as number of observation in the main dataset!"),
               errorClass = "error_dimension")
    }
    dataset$criterion
  })

  total_score <- reactive({
    dataset$total_score
  })

  z_score <- reactive({
    dataset$z_score
  })

  # * Item numbers and item names ######
  item_numbers <- reactive({
    if (!input$itemnam){
      nam <- 1:ncol(dataset$nominal)
    } else {
      nam <- colnames(dataset$nominal)
    }
    nam
  })

  item_names <- reactive({
    if (!input$itemnam){
      nam <- paste("Item", 1:ncol(dataset$nominal))
    } else {
      nam <- colnames(dataset$nominal)
    }
    nam
  })

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # ITEM SLIDERS ######
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  observe({
    sliderList <- c(
      "slider_totalscores_histogram",
      "corr_plot_clust",
      "corr_plot_clust_report",
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
      "DIF_NLR_item_plot",
      "difirt_lord_itemSlider",
      "difirt_raju_itemSlider",
      "ddfSlider",
      "inSlider2group",
      "reportSlider",
      "difMHSlider_score"
      )

    itemCount = ncol(ordinal())
    updateSliderInput(session = session, inputId = "slider_totalscores_histogram", max = itemCount, value = round(median(total_score(), na.rm = T)))
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
    updateSliderInput(session = session, inputId = "DIF_NLR_item_plot", max = itemCount)
    updateSliderInput(session = session, inputId = "difirt_lord_itemSlider", max = itemCount)
    updateSliderInput(session = session, inputId = "difirt_raju_itemSlider", max = itemCount)
    updateSliderInput(session = session, inputId = "ddfSlider", max = itemCount)
    updateSliderInput(session = session, inputId = "inSlider2group", max = itemCount,
                      value = round(median(dataset$total_score[dataset$group == 1], na.rm = T)))
    updateSliderInput(session = session, inputId = "difMHSlider_score", max = itemCount,
                      value = round(median(dataset$total_score, na.rm = T)))

  })

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # DATA PAGE ######
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  source("server/Data.R", local = T)

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # SUMMARY ######
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  source("server/Summary.R", local = T)

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # RELIABILITY ######
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  source("server/Reliability.R", local = T)

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

  # * Update dataset name in Reports page ####
  dataName <- reactive({
    if (is.null(input$data)) {
      a <- input$dataSelect
      pos <- regexpr("_", a)[1]
      name <- str_sub(a, 1, pos - 1)
      if (name == "dataMedical"){
        name <- "Medical 100"
      }
      if (name == "dataMedicalgraded"){
        name <- "Medical Graded"
      }
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
  # * Report format ####
  formatInput <- reactive({
    format <- input$report_format
    format
  })
  # * Setting for report ####
  # ** IRT models ####
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
  # ** Group present ####
  groupPresent <- reactive({
    (any(dataset$group != "missing") | is.null(dataset$group))
  })
  # ** Critetion present ####
  criterionPresent <- reactive({
    (any(dataset$criterion != "missing") | is.null(dataset$criterion))
  })


  observeEvent(input$generate, {
    withProgress(message = "Creating content", value = 0, style = "notification", {
      list(# header
           author = input$reportAuthor,
           dataset = input$reportDataName,
           # datasets
           a = nominal(),
           k = key(),
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
                         a = nominal(),
                         k = key(),
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


  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # SETTING ######
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  source("server/Setting.R", local = T)
}
