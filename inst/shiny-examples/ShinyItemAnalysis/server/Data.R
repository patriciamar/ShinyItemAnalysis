# DATA CHECKING ####

# * Error and warning messages for upload ####
checkDataText_Input <- eventReactive(input$submitButton, {
  error_setting <- F
  submitCounter$Click <- 1
  removeCounter$Click <- 0

  if (dataset$data_status == "missing") {
    str_errors <- "No data found! Please, upload data. Default dataset GMAT is now in use."
    str_warnin <- ""
  } else {
    ### key
    error_key <- ""
    if (length(key()) == 1) {
      if (key() == "missing") {
        error_key <- "The key needs to be provided!"
      }
    } else {
      if (ncol(nominal()) != length(key())) {
        error_key <- "The length of key needs to be the same as number of columns of the main dataset!"
        error_setting <- T
      }
    }
    ### group
    error_group <- ""
    warni_group <- ""
    if (any(group() == "missing", na.rm = T)) {
      warni_group <- "The group variable is not provided! DIF and DDF analyses are not available!"
    } else {
      if (nrow(nominal()) != length(group())) {
        error_group <- "The length of group needs to be the same as number of observations in the main dataset!"
        error_setting <- T
      }
    }
    ### criterion variable
    error_criterion_variable <- ""
    warni_criterion_variable <- ""
    if (any(criterion() == "missing", na.rm = T)) {
      warni_criterion_variable <- "The criterion variable is not provided! Predictive validity analysis is not available!"
    } else {
      if (nrow(nominal()) != length(criterion())) {
        error_criterion_variable <- "The length of criterion variable needs to be the same as number of observations in the main dataset!"
        error_setting <- T
      }
    }
    ### DIF matching
    error_DIFmatching_variable <- ""
    warni_DIFmatching_variable <- ""
    if (any(DIFmatching() == "missing", na.rm = T)) {
      warni_DIFmatching_variable <- "The DIF matching variable is not provided! DIF analyses will use total scores!"
    } else {
      if (nrow(nominal()) != length(DIFmatching())) {
        error_DIFmatching_variable <- "The length of DIF matching variable needs to be the same as number of observations in the main dataset!"
        error_setting <- T
      }
    }
    str_errors <- c(error_key, error_group, error_criterion_variable, error_DIFmatching_variable)
    str_warnin <- c(warni_group, warni_criterion_variable, warni_DIFmatching_variable)
  }
  if (any(str_warnin != "")) {
    str_warnin <- str_warnin[str_warnin != ""]
    str_warnin <- paste("<font color = 'orange'> &#33;", str_warnin, "</font>", collapse = "<br>")
  }

  if (all(str_errors == "")) {
    paste(c(
      "<font color = 'green'> &#10004; Your data were successfully uploaded. Check them in <b>Data exploration</b> tab. </font>",
      str_warnin
    ), collapse = "<br>")
  } else {
    str_errors <- str_errors[str_errors != ""]
    str_errors <- paste("<font color = 'red'> &#10006;", str_errors, "</font>", collapse = "<br>")
    if (error_setting) {
      paste(c(
        str_errors,
        "<font color = 'red'> Check <b>Data exploration</b> tab to get idea what went wrong or try another
              <b>Data specification</b> below. </font>"
      ),
      collapse = "<br>"
      )
    } else {
      paste(str_errors)
    }
  }
})

output$checkDataText <- renderUI({
  HTML(checkDataText_Input())
})

# * Click counter ####
submitCounter <- reactiveValues()
removeCounter <- reactiveValues()
submitCounter$Click <- 0
removeCounter$Click <- 0

# * Render remove button after data load ####
output$removeBut_output <- renderUI({
  if (submitCounter$Click > 0) {
    tagList(actionButton(
      inputId = "removeButton",
      label = "Unload data",
      class = "btn btn-large btn-primary",
      icon = icon("trash"),
      width = "150px"
    ))
  }
})

observeEvent(input$key,
  {
    dataset$key_upload_status <- "uploaded"
  },
  priority = 1000
)

# * Remove loaded data ####
observeEvent(input$removeButton, {
  # reset function reset values in input
  # html function change text in corresponding html tag
  useShinyjs()
  reset("data")

  if (input$data_type == "nominal") {
    reset("key_nominal")
  } else {
    reset("key_ordinal")
  }

  reset("groups")
  reset("criterion_variable")
  reset("dif_matching")
  reset("maxOrdinal")
  reset("minOrdinal")
  reset("globalMax")
  reset("globalMin")
  reset("globalCut")
  reset("submitButton")

  dataset$key_upload_status <- "reset"

  html("removedItemsText", html = "")
  html("checkDataText", html = "")
  html("checkDataColumns01Text", html = "")
  html("renderdeleteButtonColumns01", html = "")
  html("removedItemsText", html = "")
  html("checkGroupText", html = "")
  html("renderdeleteButtonGroup", html = "")
  html("removedGroupText", html = "")

  submitCounter$Click <- 0
  removeCounter$Click <- 1
  removeUI(selector = "#removeButton")
  removeCounter$Click <- 0
})

# * Checking uploaded scored data ####
checkDataColumns01_Input <- reactive({
  data <- binary()
  # are there any items with only 0
  all0 <- apply(data, 2, function(x) all(x == 0))
  all1 <- apply(data, 2, function(x) all(x == 1))

  list(all0 = all0, all1 = all1)
})

# * Render button for excluding such items ####
output$renderdeleteButtonColumns01 <- renderUI({
  all0 <- checkDataColumns01_Input()$all0
  all1 <- checkDataColumns01_Input()$all1

  if (input$submitButton & (any(all0) | any(all1))) {
    tagList(
      actionButton(
        inputId = "deleteButtonColumns01",
        label = "Remove items",
        class = "btn btn-large btn-primary",
        icon = icon("trash"),
        width = "150px"
      )
    )
  }
})

# * Removing such items ####
observeEvent(input$deleteButtonColumns01, {
  ok0 <- (!checkDataColumns01_Input()$all0)
  ok1 <- (!checkDataColumns01_Input()$all1)

  dataset$key <- key()[(ok0 & ok1)]

  dataset$nominal <- nominal()[, (ok0 & ok1), with = F]
  dataset$ordinal <- ordinal()[, (ok0 & ok1), with = F]
  dataset$binary <- binary()[, (ok0 & ok1), with = F]
})

# * Text for check of uploaded scored data ####
checkDataColumns01Text_Input <- eventReactive((input$submitButton | input$deleteButtonColumns01), {
  all0 <- checkDataColumns01_Input()$all0
  all1 <- checkDataColumns01_Input()$all1

  if (any(all0)) {
    txt0 <- paste(
      "It seems that",
      item_names()[all0],
      "consists only of zeros."
    )
  } else {
    txt0 <- ""
  }
  if (any(all1)) {
    txt1 <- paste(
      "It seems that",
      item_names()[all1],
      "consists only of ones."
    )
  } else {
    txt1 <- ""
  }

  # warning
  if (any(all0) | any(all1)) {
    txt <- paste(c(
      "Check your data!",
      paste(txt0, collapse = "<br>"),
      paste(txt1, collapse = "<br>"),
      "Some analyses may not work properly. Consider removing such items.
                   For this purpose you can use button <b>Remove items</b> on the right side. <br><br>"
    ),
    collapse = "<br>"
    )
    txt <- paste("<font color = 'red'>", txt, "</font>")
  } else {
    txt <- ""
  }

  txt
})

output$checkDataColumns01Text <- renderUI({
  HTML(checkDataColumns01Text_Input())
})

# * Confirmation about items removal ####
removedItemsText_Input <- eventReactive(input$deleteButtonColumns01, {
  txt <- "Items were removed."
  txt <- paste("<font color = 'green'>", txt, "</font>")
  txt
})

output$removedItemsText <- renderUI({
  HTML(removedItemsText_Input())
})

# * Checking uploaded group membership ####
checkGroup_Input <- reactive({
  group <- group()
  # are there any missing values?
  NAgroup <- is.na(group)
  NAgroup
})

# * Text for check of uploaded group membership ####
checkGroupText_Input <- eventReactive((input$submitButton | input$deleteButtonGroup), {
  NAgroup <- checkGroup_Input()

  if (any(NAgroup)) {
    txt <- paste(
      sum(NAgroup),
      ifelse(sum(NAgroup) == 1,
        "observation has",
        "observations have"
      ),
      "missing group membership. <br>
                 Some analyses may not work properly. Consider removing such items.
                 For this purpose you can use button <b>Remove data</b> on the right side. <br><br>"
    )
    txt <- paste("<font color = 'red'>", txt, "</font>")
  } else {
    txt <- ""
  }
  txt
})

output$checkGroupText <- renderUI({
  HTML(checkGroupText_Input())
})

# * Confirmation about group with NA values removal ####
removedGroupText_Input <- eventReactive(input$deleteButtonGroup, {
  txt <- "Rows with missing group membership removed."
  txt <- paste("<font color = 'green'>", txt, "</font>")
  txt
})

output$removedGroupText <- renderUI({
  HTML(removedGroupText_Input())
})

# * Removing such data ####
observeEvent(input$deleteButtonGroup, {
  OKgroup <- (!checkGroup_Input())

  dataset$group <- dataset$group[OKgroup]
  # exclude when criterion is missing
  if (length(dataset$criterion) == length(OKgroup)) {
    dataset$criterion <- dataset$criterion[OKgroup]
  }

  dataset$nominal <- dataset$nominal[OKgroup]
  dataset$ordinal <- dataset$ordinal[OKgroup]
  dataset$binary <- dataset$binary[OKgroup]
})

# * Render button for excluding data with missing group membership ####
output$renderdeleteButtonGroup <- renderUI({
  NAgroup <- checkGroup_Input()

  if (input$submitButton & any(NAgroup)) {
    tagList(
      actionButton(
        inputId = "deleteButtonGroup",
        label = "Remove data",
        class = "btn btn-large btn-primary",
        icon = icon("trash"),
        width = "150px"
      )
    )
  }
})

# DATA DESCRIPTION ####
data_description_Input <- reactive({
  data_name <- input$dataSelect
  txt <- switch(data_name,
    GMAT_difNLR = "<code>GMAT</code> <a href='https://doi.org/10.1187/cbe.16-10-0307' target='_blank'>(Martinkova et al., 2017)</a>
                is a generated dataset based on parameters of real Graduate Management Admission Test (GMAT; Kingston et al., 1985)
                from the <code>difNLR</code> package. The dataset represents responses of 2,000 subjects (1,000 males coded as
                <code>'0'</code>, 1,000 females coded as <code>'1'</code>) to a multiple-choice test of 20 items. It also contains
                generated continuous criterion intended to be predicted by the test. As discussed in
                <a href='https://doi.org/10.1187/cbe.16-10-0307' target='_blank'> Martinkova et al. (2017)</a>, the dataset was simulated
                in order to provide an example of DIF items present even in case of identical total score distributions. To replicate the
                example provided in <a href='https://doi.org/10.1187/cbe.16-10-0307' target='_blank'> Martinkova et al. (2017)</a>,
                select the <code>GMAT</code> dataset and go to <code>DIF/Fairness</code> section. ",
    GMAT2_difNLR = "<code>GMAT2</code> <a href='https://doi.org/10.1111/jedm.12158' target='_blank'>(Drabinova & Martinkova, 2017)</a> is
                a simulated dataset based on parameters of real Graduate Management Admission Test (GMAT; Kingston et al., 1985) from the
                <code>difNLR</code> package. First two items were simulated to function differently in uniform and non-uniform way respectively.
                The dataset represents responses of 1,000 subjects (500 males coded as <code>'0'</code>, 500 females coded as <code>'1'</code>)
                to a multiple-choice test of 20 items. ",
    MSATB_difNLR = "<code>MSAT-B</code> <a href='https://doi.org/10.1111/jedm.12158' target='_blank'>(Drabinova & Martinkova, 2017)</a> is
                a subset of real Medical School Admission Test in Biology (MSAT-B) in Czech Republic from the <code>difNLR</code> package.
                The dataset represents responses of 1,407 subjects (484 males coded as <code>'0'</code>, 923 females coded as <code>'1'</code>)
                to a multiple-choice test of 20 items. The first item was previously detected as functioning differently. For more details
                on item selection see <a href='https://doi.org/10.1111/jedm.12158' target='_blank'>Drabinova and Martinkova (2017)</a>.
                To replicate the example provided in <a href='https://doi.org/10.1111/jedm.12158' target='_blank'> Drabinova and Martinkova
                (2017)</a>, select the <code>MSAT-B</code> dataset and go to <code>DIF/Fairness</code> section. ",
    dataMedical_ShinyItemAnalysis = "<code>Medical 100</code> <a href='https://doi.org/10.5817/TF2017-9-129' target='_blank'>
                (Martinkova et al., 2017)</a> is a real <code>dataMedical</code> dataset of an admission test to medical
                school from the <code>ShinyItemAnalysis</code> package. The data set represents responses of 2,392 subjects
                (750 males coded as <code>'0'</code>, 1,633 females coded as <code>'1'</code>, and 9 subjects without gender
                specification coded as <code>'NA'</code>) to a multiple-choice test of 100 items. The dataset contains criterion
                variable &ndash; indicator whether student studies standardly or not. In <a href='https://doi.org/10.5817/TF2017-9-129'
                target='_blank'> Martinkova et al. (2017)</a>, the dataset was used to demonstrate functionalities of earlier
                version of the <code>ShinyItemAnalysis</code> interactive app. ",
    dataMedicalgraded_ShinyItemAnalysis = "<code>Medical 100 Graded</code> is a real <code>dataMedicalgraded</code> dataset
                of an admission test to medical school from the <code>ShinyItemAnalysis</code> package. The dataset represents
                responses of 2,392 subjects (750 males coded as <code>'0'</code>, 1,633 females coded as <code>'1'</code>, and
                9 subjects without gender specification coded as <code>'NA'</code>) to a multiple-choice test of 100 items. Each
                item is graded with 0 to 4 points. Maximum of 4 points were set if all correct answers and none of incorrect
                answers were selected. This dataset contains criterion variable &ndash; indicator whether student studies standardly
                or not. For analyses where dichotomous items are necessary (e.g., logistic models in Regression, IRT models, or
                DIF detection methods) binarization of the data is applied &ndash; <code>'1'</code> means that student gained maximum
                number of points, i.e., 4; otherwise the item is scored as <code>'0'</code>. ",
    HCI_ShinyItemAnalysis = "<code>HCI</code> <a href='http://dx.doi.org/10.1187/cbe.16-10-0305' target='_blank'>(McFarland et al.,
                2017)</a> is a real dataset of Homeostasis Concept Inventory (HCI) from the <code>ShinyItemAnalysis</code> package.
                The dataset represents responses of 651 subjects (405 males coded as <code>'0'</code>, 246 females coded as
                <code>'0'</code>) to a multiple-choice test of 20 items. <code>HCI</code> contains criterion variable &ndash; indicator
                whether student plans to major in the life sciences. In <a href='http://dx.doi.org/10.1187/cbe.16-10-0305'
                target='_blank'> McFarland et al. (2017)</a>, the dataset was used to perform complex validation of the HCI
                assessment instrument. The dataset was also used for demonstrations of the <code>ShinyItemAnalysis</code> package
                and earlier version of this online app in The R Journal paper by <a href='https://doi.org/10.32614/RJ-2018-074'
                target='_blank'> Martinkova and Hladka (2018)</a>. ",
    Science_mirt = "<code>Science</code> dataset represents responses of 392 subjects on a 4-item test describing attitude to science
                and technology from the <code>mirt/ltm</code> packages. Selected items are <code>Comfort</code>, <code>Work</code>,
                <code>Future</code>, and <code>Benefit</code>. All items are measured on the same scale with response categories:
                <code>'strongly disagree'</code>, <code>'disagree to some extent'</code>, <code>'agree to some extent'</code>, and
                <code>'strongly agree'</code>. See Karlheinz and Melich (1992) for more details. For analyses where dichotomous items
                are necessary (e.g., logistic models in Regression, IRT models, or DIF detection methods) binarization of the data is
                applied &ndash; <code>'1'</code> means that respondent <code>'strongly agrees'</code> with given item; otherwise
                the item is scored <code>'0'</code>. ",
    LearningToLearn_ShinyItemAnalysis_6 = "<code>Learning To Learn 6</code> <a href='https://doi.org/10.1016/j.learninstruc.2019.101286'
                target='_blank'> (Martinkova et al., 2020)</a> is a subset of longitudinal <code>LearningToLearn</code> dataset from the
                <code>ShinyItemAnalysis</code> package. It consists of answers to the Learning to Learn test in Grade 6 only. The same
                respondents were also tested in Grade 9 &ndash; respective data is available in the <code>Learning To Learn 9</code>
                dataset. The dataset represents binary-coded responses of 782 subjects (391 from basic school track, BS, coded here as
                <code>'0'</code>; 391 from selecive academic track, AS, coded here as <code>'1'</code>) to (mostly) multiple-choice test
                consisting of 41 items within 7 subscales. This dataset was created using propensity score matching algorithm to achieve
                similar characteristics in both tracks. For further details see <a href='https://doi.org/10.1016/j.learninstruc.2019.101286'
                target='_blank'> Martinkova, Hladka, and Potuznikova (2020)</a>.",
    LearningToLearn_ShinyItemAnalysis_9 = "<code>Learning To Learn 9</code> <a href='https://doi.org/10.1016/j.learninstruc.2019.101286'
                target='_blank'> (Martinkova et al., 2020)</a> is a subset of longitudinal <code>LearningToLearn</code> dataset from the
                <code>ShinyItemAnalysis</code> package. It consists of answers to the Learning to Learn test in Grade 9 only. The same
                respondents were also tested in Grade 6 &ndash; respective data is available in <code>Learning To Learn 6</code> dataset.
				        The dataset represents binary-coded responses of 782 subjects (391 from basic school track, BS, coded here as <code>'0'</code>;
				        391 from selecive academic track, AS, coded here as <code>'1'</code>) to (mostly) multiple-choice test consisting of 41
				        items within 7 subscales. The dataset was created using propensity score matching algorithm to achieve similar
				        characteristics in both tracks. Matching variable for the DIF analysis is the total score gained in Grade 6. In their paper,
				        <a href='https://doi.org/10.1016/j.learninstruc.2019.101286' target='_blank'> Martinkova et al. (2020)</a> found significant
				        DIF in change present in some of the items, while no significant difference in change was present in total scores. The dataset
				        was also used for demonstration purposes in The R Journal paper on the <code>difNLR</code> package
                <a href='https://doi.org/10.32614/RJ-2020-014' target='_blank'>(Hladka & Martinkova, 2020)</a>. ",
    AIBS_ShinyItemAnalysis = "<code>AIBS Grant Peer Review Scoring</code> dataset <a href='https://doi.org/10.6084/m9.figshare.12728087'
                target='_blank'> (Gallo, 2021)</a> comes from a scientific peer review of biomedical applications from a collaborative
                biomedical research funding program (2014&ndash2017). The data presented in this app include anonymized proposal ID, reviewer ID,
                and an overall score from three reviewers, scored on a scale from 1.0 (best) to 5.0 (worst) with a 0.1 gradation. The
                dataset was used by <a href='https://rss.onlinelibrary.wiley.com/loi/1467985x' target='_blank'>Erosheva, Martinkova, and Lee</a>
                to demonstrate issues with estimation of inter-rater reliability (IRR) in range-restricted samples. To try interactively
                the range-restricted IRR analysis, select the <code>AIBS</code> dataset and go to the <code>Reliability/Restricted range</code>
                section."
  )
  txt
})

output$data_description <- renderUI({
  HTML(data_description_Input())
})

# KEY INPUT ####
# output$data_key_file_input <- renderUI({
#   label <- switch(input$data_type,
#                  "nominal" = "Choose key (CSV file)",
#                  "ordinal" = "Choose cut-score (CSV file)")
#   fileInput(inputId = "key",
#             label = label,
#             accept = c("text/csv",
#                        "text/comma-separated-values",
#                        "text/tab-separated-values",
#                        "text/plain",
#                        ".csv",
#                        ".tsv"))
# })



# BASIC SUMMARY ####

# * Main dataset ####

# ** Dimension ####
output$data_rawdata_dim <- renderText({
  if (input$dataSelect == "AIBS_ShinyItemAnalysis") {
    txt <- paste0("The AIBS is not a multi-item dataset. It consists of ", nrow(continuous()),
      " observations on the ", ncol(continuous()), " variables. "
    )
  } else {
    txt <- paste0(
      "The main dataset consists of ", nrow(continuous()),
      " observations on the ", ncol(continuous()), " ", data_type(), " items. "
    )
  }
  txt
})

# ** Ordinal data summary ####
data_ordinal_summary <- reactive({
  data_table <- ordinal()

  if (data_type() == "ordinal") {
    data_table <- as.data.frame(sapply(data_table, as.numeric))
    key <- key()

    data_table_summary <- data.table(
      item_names(),
      sapply(data_table, min, na.rm = TRUE),
      sapply(data_table, median, na.rm = TRUE),
      sapply(data_table, mean, na.rm = TRUE),
      sapply(data_table, max, na.rm = TRUE),
      sapply(data_table, sd, na.rm = TRUE),
      as.numeric(key)
    )
    rownames(data_table_summary) <- item_names()
    colnames(data_table_summary) <- c("Name", "Min", "Median", "Mean", "Max", "SD", "Cut")
    data_table_summary
  } else {
    colnames(data_table) <- item_names()
    summary(data_table)
  }
})

# ** Nominal data summary ####
data_nominal_summary <- reactive({
  data_table <- nominal()
  colnames(data_table) <- item_names()
  summary(data_table)
})

# ** Binary data summary ####
data_binary_summary <- reactive({
  data_table <- binary()
  colnames(data_table) <- item_names()
  summary(data_table)
})

output$data_binary_summary <- renderPrint({
  data_binary_summary()
})

# ** Continuous data summary ####
data_continuous_summary <- reactive({
  data_table <- continuous()
  summary(data_table)
})

# ** Main dataset summary ####
output$data_rawdata_summary <- renderPrint({
  if (dataset$data_type == "ordinal") {
    data_ordinal_summary()
  } else if (dataset$data_type == "nominal") {
    data_nominal_summary()
  } else if (dataset$data_type == "continuous") {
    data_continuous_summary()
  } else {
    data_binary_summary
  }
})

# * Group ####
output$data_group_summary <- renderPrint({
  gr <- as.factor(group())
  summary(gr)
})

# * Criterion ####
output$data_criterion_summary <- renderPrint({
  criterion <- criterion() # has to be called ousite S3 methods, otherwise returns ugly warnigs
  summary(criterion)
})

# * DIF matching ####
output$data_DIFmatching_summary <- renderPrint({
  DIFmatching <- DIFmatching()
  summary(DIFmatching)
})


# TEST OF APPLICATION ####
# source('tests/helper_functions/DataTest.R',local = T)
#
# exportTestValues(data = switch(dataset$data_type,
#                               "ordinal" = data_ordinal_summary_Input(),
#                               "nominal" = data_nominal_summary_Input(),
#                               "binary" = data_binary_summary_Input()),
#                 tab_bin = data_binary_summary_Input(),
#                 group = if (check_group() == 'OK'){
#                   data_group_summary_Input()
#                 } else {
#                   check_group()
#                 },
#                 crit = if (check_crit() == 'OK') {
#                   data_criterion_summary_Input()
#                 } else {
#                   check_crit()
#                 })

# DATA EXPLORATION ####

# * Main dataset ####
output$headdata <- DT::renderDataTable(
  {
    data_table <- nominal()
    colnames(data_table) <- item_names()
    data_table
  },
  rownames = FALSE,
  style = "bootstrap",
  options = list(
    scrollX = TRUE,
    pageLength = 6,
    server = TRUE,
    scrollCollapse = TRUE,
    dom = "tipr"
  )
)

# * Key ####
output$key <- DT::renderDataTable(
  {
    key_table <- key()
    key_table <- as.data.table(t(key_table))
    colnames(key_table) <- item_names()
    key_table
  },
  rownames = FALSE,
  style = "bootstrap",
  options = list(
    scrollX = TRUE,
    server = TRUE,
    scrollCollapse = TRUE,
    dom = "tipr"
  )
)

# * Binary data with total score ####
output$sc01 <- DT::renderDataTable(
  {
    data_table <- binary()
    scored_table <- data.table(data_table, total_score())
    colnames(scored_table) <- c(item_names(), "Score")
    scored_table
  },
  rownames = FALSE,
  style = "bootstrap",
  options = list(
    scrollX = TRUE,
    pageLength = 6,
    server = TRUE,
    scrollCollapse = TRUE,
    dom = "tipr"
  )
)

# * Group ####
output$group <- DT::renderDataTable(
  {
    group_table <- group()
    group_table <- t(group_table)
    colnames(group_table) <- 1:ncol(group_table)
    group_table
  },
  rownames = FALSE,
  style = "bootstrap",
  options = list(
    scrollX = TRUE,
    server = TRUE,
    scrollCollapse = TRUE,
    dom = "tipr"
  )
)

# * Criterion variable ####
output$critvar <- DT::renderDataTable(
  {
    critvar_table <- criterion()
    critvar_table <- t(critvar_table)
    colnames(critvar_table) <- 1:ncol(critvar_table)
    critvar_table
  },
  rownames = FALSE,
  style = "bootstrap",
  options = list(
    scrollX = TRUE,
    server = TRUE,
    scrollCollapse = TRUE,
    dom = "tipr"
  )
)

# * DIF matching variable ####
output$difvar <- DT::renderDataTable(
  {
    difvar_table <- DIFmatching()
    difvar_table <- t(difvar_table)
    colnames(difvar_table) <- 1:ncol(difvar_table)
    difvar_table
  },
  rownames = FALSE,
  style = "bootstrap",
  options = list(
    scrollX = TRUE,
    server = TRUE,
    scrollCollapse = TRUE,
    dom = "tipr"
  )
)
