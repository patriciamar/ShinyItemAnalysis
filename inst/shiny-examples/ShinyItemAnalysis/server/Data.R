# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# DATA UPLOAD ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * REACTIVE VALUES ####

# ** Datasets ####
dataset <- reactiveValues()

dataset$binary <- NULL
dataset$ordinal <- NULL
dataset$nominal <- NULL
dataset$continuous <- NULL

dataset$name <- NULL
dataset$data_type <- NULL

dataset$key <- NULL
dataset$minimal <- NULL
dataset$maximal <- NULL

dataset$group <- NULL
dataset$criterion <- NULL
dataset$DIFmatching <- NULL
dataset$rank <- NULL

dataset$data_status <- NULL
dataset$key_upload_status <- "toy"

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * LOADING TOY DATA ####

# toy data is uploaded when user clicks on different toy dataset or in case that
# user clicks on "Unload data" button
observeEvent(c(input$data_toydata, data_csvdata_current_status$unloaded == 1), {

  toydata <- input$data_toydata
  toydata_name <- strsplit(toydata, split = "_")[[1]][1]
  toydata_package <- strsplit(toydata, split = "_")[[1]][2]
  toydata_subset <- strsplit(toydata, split = "_")[[1]][3]

  dataset$data_status <- "OK"

  if (toydata_name == "LearningToLearn" & toydata_subset == "6") {
    # ** Learning to learn, grade 6 ####
    toydata_data_type <- "binary"

    do.call(data, args = list(paste0(toydata_name), package = toydata_package))
    toydata_binary <- get(paste0(toydata_name))[19:59] # for 6th grade, items only

    toydata_ordinal <- toydata_binary
    toydata_continuous <- toydata_ordinal
    toydata_nominal <- toydata_ordinal

    toydata_group <- get(paste0(toydata_name))[, "track_01"]
    toydata_criterion <- "missing"
    toydata_DIFmatching <- "missing"

    toydata_minimal <- NULL
    toydata_maximal <- NULL

    toydata_key <- rep(1, ncol(toydata_binary))

  } else if (toydata_name == "LearningToLearn" & toydata_subset == "9") {
    # ** Learning to learn, grade 9 ####
    toydata_data_type <- "binary"

    do.call(data, args = list(paste0(toydata_name), package = toydata_package))
    toydata_binary <- get(paste0(toydata_name))
    toydata_binary <- toydata_binary[60:100] # for 9th grade, items only

    toydata_ordinal <- toydata_binary
    toydata_continuous <- toydata_ordinal
    toydata_nominal <- toydata_ordinal

    toydata_group <- get(paste0(toydata_name))[, "track_01"]
    toydata_criterion <- "missing"
    toydata_DIFmatching <- get(paste0(toydata_name))[, "score_6"]

    toydata_minimal <- NULL
    toydata_maximal <- NULL

    toydata_key <- rep(1, ncol(toydata_binary))

  } else if (toydata_name == "dataMedicalgraded") {
    # ** Medical graded ####
    toydata_data_type <- "ordinal"

    do.call(data, args = list(paste0(toydata_name), package = toydata_package))
    toydata_ordinal <- get(paste0(toydata_name))

    toydata_group <- toydata_ordinal[, "gender"]
    toydata_criterion <- toydata_ordinal[, "StudySuccess"]
    toydata_DIFmatching <- "missing"

    toydata_ordinal <- toydata_ordinal[, 1:(ncol(toydata_ordinal) - 2)]
    toydata_continuous <- toydata_ordinal
    toydata_nominal <- toydata_ordinal

    toydata_minimal <- sapply(toydata_ordinal, min, na.rm = TRUE)
    toydata_maximal <- sapply(toydata_ordinal, max, na.rm = TRUE)

    toydata_key <- toydata_maximal
    # key2binary is much more faster than the old approach, but it is
    # only usable when maximum score is considered as the key
    toydata_binary <- mirt::key2binary(toydata_ordinal, toydata_key)

  } else if (toydata_name == "Science") {
    # ** Science ####
    toydata_data_type <- "ordinal"

    do.call(data, args = list(paste0(toydata_name), package = toydata_package))

    toydata_ordinal <- get(paste0(toydata_name))
    toydata_continuous <- toydata_ordinal
    toydata_nominal <- toydata_ordinal

    toydata_group <- "missing"
    toydata_criterion <- "missing"
    toydata_DIFmatching <- "missing"

    toydata_minimal <- sapply(toydata_ordinal, min, na.rm = TRUE)
    toydata_maximal <- sapply(toydata_ordinal, max, na.rm = TRUE)

    toydata_key <- toydata_maximal
    # key2binary is much more faster than the old approach, but it is
    # only usable when maximum score is considered as the key
    toydata_binary <- mirt::key2binary(toydata_ordinal, toydata_key)

  } else if (toydata_name == "AIBS") {
    # ** AIBS ####
    toydata_data_type <- "continuous"

    do.call(data, args = list(paste0(toydata_name), package = toydata_package))
    toydata_continuous <- get(paste0(toydata_name))

    toydata_nominal <- "missing"
    toydata_ordinal <- "missing"
    toydata_binary <- "missing"

    toydata_group <- "missing"
    toydata_criterion <- "missing"
    toydata_DIFmatching <- "missing"

    toydata_minimal <- "missing"
    toydata_maximal <- "missing"

    toydata_key <- "missing"

  } else {
    # ** Nominal datasets - GMAT, HCI, MSATB, Medical 100 ####
    toydata_data_type <- "nominal"

    do.call(data, args = list(paste0(toydata_name, "test"), package = toydata_package))
    toydata_nominal <- get(paste0(toydata_name, "test"))

    toydata_minimal <- NULL
    toydata_maximal <- NULL

    do.call(data, args = list(paste0(toydata_name, "key"), package = toydata_package))
    toydata_key <- as.character(unlist(get(paste0(toydata_name, "key"))))

    toydata_group <- toydata_nominal[, length(toydata_key) + 1]
    if (toydata_name %in% c("MSATB")) {
      toydata_criterion <- "missing"
    } else {
      toydata_criterion <- toydata_nominal[, length(toydata_key) + 2]
    }
    toydata_DIFmatching <- "missing"

    toydata_nominal <- toydata_nominal[, 1:length(toydata_key)]
    toydata_ordinal <- mirt::key2binary(toydata_nominal, toydata_key)
    toydata_continuous <- toydata_ordinal
    toydata_binary <- toydata_ordinal
  }

  # ** Saving into reactiveValues ####
  dataset$nominal <- as.data.table(toydata_nominal)
  dataset$ordinal <- as.data.table(toydata_ordinal)
  dataset$binary <- as.data.table(toydata_binary)
  dataset$continuous <- as.data.table(toydata_continuous)

  dataset$name <- toydata_name
  dataset$data_type <- toydata_data_type

  dataset$minimal <- toydata_minimal
  dataset$maximal <- toydata_maximal

  dataset$key <- toydata_key
  dataset$group <- toydata_group
  dataset$criterion <- toydata_criterion
  dataset$DIFmatching <- toydata_DIFmatching
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * LOADING DATA FROM CSV FILES ####

# toy data is uploaded when user clicks on "Upload data" button
observeEvent(input$data_upload, {

  csvdata_data <- NULL
  csvdata_key <- NULL
  csvdata_group <- NULL
  csvdata_criterion <- NULL
  csvdata_DIFmatching <- NULL # DIF matching
  csvdata_minimal <- NULL
  csvdata_maximal <- NULL

  csvdata_data_type <- input$data_csvdata_data_type

  # ** Loading main data ####
  if (is.null(input$data_csvdata_main)) {
    # in case no data uploaded, data_status is "missing" and no other parts
    # (e.g., key, group, etc.) are uploaded. Default dataset GMAT is uploaded
    dataset$data_status <- "missing"

    updateSelectInput(
      session = session,
      inputId = "data_toydata",
      selected = "GMAT_difNLR"
    )
  } else {
    # otherwise main data is uploaded from csv file and other files are uploaded
    # as well
    csvdata_data <- read.csv(
      input$data_csvdata_main$datapath,
      header = input$data_csvdata_header,
      sep = input$data_csvdata_sep,
      quote = input$data_csvdata_quote,
      stringsAsFactors = TRUE
    )
    dataset$data_status <- "OK"
    csvdata_data_name <- gsub(".csv", "", input$data_csvdata_main[[1]])

    # ** Loading minimal/maximual values for ordinal data ####
    if (input$data_csvdata_data_type == "ordinal") {
      # changing factors to numeric
      csvdata_data[] <- sapply(csvdata_data, function(x) as.numeric(paste(x)))

      # *** Minimal values upload ####
      if (is.null(input$data_csvdata_minimal)) {
        # in case no csv file with minimal values is provided, global minimum is
        # checked
        if (input$data_csvdata_minimal_global == "") {
          # in case no global minimal value is provided, minimal values are
          # calculated from data
          csvdata_minimal <- sapply(csvdata_data, min, na.rm = TRUE)
        } else {
          # otherwise, global minimal value is applied for all items
          csvdata_minimal <- rep(
            input$data_csvdata_minimal_global,
            ncol(csvdata_data)
          )
        }
      } else {
        # otherwise, value is read from csv file for each item
        csvdata_minimal <- read.csv(
          input$data_csvdata_minimal$datapath,
          header = input$data_csvdata_header,
          sep = input$data_csvdata_sep,
          quote = input$data_csvdata_quote
        )
      }

      # *** Maximal values upload ####
      if (is.null(input$data_csvdata_maximal)) {
        # in case no csv file with maximal values is provided, global maximum is
        # checked
        if (input$data_csvdata_maximal_global == "") {
          # in case no global maximal value is provided, maximal values are
          # calculated from data
          csvdata_maximal <- sapply(csvdata_data, max, na.rm = TRUE)
        } else {
          # otherwise, global maximal value is applied for all items
          csvdata_maximal <- rep(
            input$data_csvdata_maximal_global,
            ncol(csvdata_data)
          )
        }
      } else {
        # otherwise, value is read from csv file for each item
        csvdata_maximal <- read.csv(
          input$data_csvdata_maximal$datapath,
          header = input$data_csvdata_header,
          sep = input$data_csvdata_sep,
          quote = input$data_csvdata_quote
        )
      }
    }

    # ** Loading key ####
    # based on type of data, key for either nominal or ordinal (cut score) data
    # is in use
    data_csvdata_key_in_use <- if (input$data_csvdata_data_type == "nominal") {
      input$data_csvdata_key_nominal
    } else {
      input$data_csvdata_cutscore_ordinal
    }

    if (is.null(data_csvdata_key_in_use) | dataset$key_upload_status == "reset") {
      # if there is no key provided (check what does dataset$key_upload_status
      # == "reset" mean) then the key is based on data (for binary/ordinal),
      # global cut-score (ordinal) or set as missing (for nominal)
      if (input$data_csvdata_data_type == "binary") {
        csvdata_key <- rep(1, ncol(csvdata_data))
      } else if (input$data_csvdata_data_type == "ordinal") {
        if (input$data_csvdata_cutscore_ordinal_global != "") {
          csvdata_key <- rep(
            as.numeric(paste(input$data_csvdata_cutscore_ordinal_global)),
            ncol(csvdata_data)
          )
        } else {
          csvdata_key <- csvdata_maximal
        }
      } else if (input$data_csvdata_data_type == "nominal") {
        csvdata_key <- "missing"
      }
    } else {
      # otherwise it is uploaded from the csv file
      csvdata_key <- read.csv(
        data_csvdata_key_in_use$datapath,
        header = input$data_csvdata_header,
        sep = input$data_csvdata_sep,
        quote = input$data_csvdata_quote
      )
      csvdata_key <- as.character(unlist(csvdata_key))
    }

    # data_csvdata_key_nominal is key of correct answers
    # data_csvdata_cutscore_ordinal is cut-score to binarize data

    # inpKey <- ifelse(input$data_csvdata_data_type == "nominal",
    #   ifelse(is.null(input$data_csvdata_key_nominal), 0, input$data_csvdata_key_nominal),
    #   ifelse(is.null(input$data_csvdata_cutscore_ordinal), 0, input$data_csvdata_cutscore_ordinal)
    # )

    # AH: this is a very messy part and I do not fully understand what is
    # going on here. This is also caused by no comments and by naming of
    # variables which is not intuitive.

    # there also needs to be check of dimensions of the main dataset and
    # the length of key
    # if (inpKey[[1]] == 0 | dataset$key_upload_status == "reset") {
    #   if (input$data_csvdata_cutscore_ordinal_global == "") {
    #     if (input$data_csvdata_data_type == "binary") {
    #       csvdata_key <- rep(1, ncol(csvdata_data))
    #     } else {
    #       if (input$data_csvdata_data_type == "ordinal") {
    #         csvdata_key <- csvdata_maximal
    #       } else {
    #         # csvdata_key <- "missing"
    #         csvdata_key <- read.csv(input$data_csvdata_key_nominal$datapath,
    #           header = input$data_csvdata_header,
    #           sep = input$data_csvdata_sep,
    #           quote = input$data_csvdata_quote
    #         )
    #         csvdata_key <- as.character(unlist(csvdata_key))
    #       }
    #     }
    #   } else {
    #     csvdata_key <- rep(as.numeric(paste(input$data_csvdata_cutscore_ordinal_global)), ncol(csvdata_data))
    #   }
    # } else {
    #   if (input$data_csvdata_data_type == "nominal") {
    #     csvdata_key <- read.csv(input$data_csvdata_key_nominal$datapath,
    #       header = input$data_csvdata_header,
    #       sep = input$data_csvdata_sep,
    #       quote = input$data_csvdata_quote
    #     )
    #     csvdata_key <- as.character(unlist(csvdata_key))
    #   } else {
    #     csvdata_key <- read.csv(input$data_csvdata_cutscore_ordinal$datapath,
    #       header = input$data_csvdata_header,
    #       sep = input$data_csvdata_sep,
    #       quote = input$data_csvdata_quote
    #     )
    #     csvdata_key <- as.character(unlist(csvdata_key))
    #   }
    # }
    # dataset$key <- csvdata_key

    # ** Loading group ####
    if (is.null(input$data_csvdata_group)) {
      # in case there is no group variable to upload, it is set to "missing"
      # otherwise uploaded from the csv file
      csvdata_group <- "missing"
    } else {
      csvdata_group <- read.csv(
        input$data_csvdata_group$datapath,
        header = input$data_csvdata_header,
        sep = input$data_csvdata_sep,
        quote = input$data_csvdata_quote
      )
      csvdata_group <- unlist(csvdata_group)
    }

    # ** Loading criterion ####
    if (is.null(input$data_csvdata_criterion)) {
      # in case there is no criterion variable to upload, it is set to "missing"
      # otherwise uploaded from the csv file
      csvdata_criterion <- "missing"
    } else {
      csvdata_criterion <- read.csv(
        input$data_csvdata_criterion$datapath,
        header = input$data_csvdata_header,
        sep = input$data_csvdata_sep,
        quote = input$data_csvdata_quote
      )
      csvdata_criterion <- unlist(csvdata_criterion)
    }

    # ** Loading DIFmatching ####
    if (is.null(input$data_csvdata_DIFmatching)) {
      # in case there is no observed score / DIF matching variable to upload, it
      # is set to "missing" otherwise uploaded from the csv file
      csvdata_DIFmatching <- "missing"
    } else {
      csvdata_DIFmatching <- read.csv(
        input$data_csvdata_DIFmatching$datapath,
        header = input$data_csvdata_header,
        sep = input$data_csvdata_sep,
        quote = input$data_csvdata_quote
      )
      csvdata_DIFmatching <- unlist(csvdata_DIFmatching)
    }

    # ** Saving into reactiveValues ####
    if (input$data_csvdata_data_type == "nominal") {
      # nominal data (uploaded):
      # binary data created using nominal and key
      # ordinal data created using nominal and key (i.e., the same as binary)
      # continuous data created using nominal and key (i.e., the same as binary)
      dataset$nominal <- csvdata_data
      dataset$binary <- mirt::key2binary(dataset$nominal, csvdata_key)
      dataset$ordinal <- dataset$binary
      dataset$continuous <- dataset$binary
    } else if (input$data_csvdata_data_type == "ordinal") {
      # ordinal data (uploaded):
      # binary data created using ordinal and cut-score
      # (key2binary cannot be used here as we take values grater or equal)
      # nominal data the same as ordinal
      # continuous data the same as ordinal
      dataset$ordinal <- csvdata_data
      df.key <- sapply(csvdata_key, rep, each = nrow(dataset$ordinal))
      dataset$binary <- matrix(as.numeric(dataset$ordinal >= df.key),
        ncol = ncol(dataset$ordinal), nrow = nrow(dataset$ordinal)
      )
      colnames(dataset$binary) <- colnames(dataset$ordinal)
      dataset$nominal <- dataset$ordinal
      dataset$continuous <- dataset$ordinal
    } else if (input$data_csvdata_data_type == "binary") {
      # binary data (uploaded):
      # ordinal data the same as binary
      # nominal data the same as binary
      # continuous data the same as binary
      dataset$binary <- csvdata_data
      dataset$nominal <- dataset$binary
      dataset$ordinal <- dataset$binary
      dataset$continuous <- dataset$binary
    }
    dataset$binary <- as.data.table(dataset$binary)
    dataset$nominal <- as.data.table(dataset$nominal)
    dataset$ordinal <- as.data.table(dataset$ordinal)
    dataset$continuous <- as.data.table(dataset$continuous)

    dataset$name <- csvdata_data_name
    dataset$data_type <- csvdata_data_type

    # min/max values
    if (input$data_csvdata_data_type == "ordinal") {
      dataset$minimal <- csvdata_minimal
      dataset$maximal <- csvdata_maximal
    } else {
      dataset$minimal <- NULL
      dataset$maximal <- NULL
    }

    dataset$key <- csvdata_key
    dataset$group <- csvdata_group
    dataset$criterion <- csvdata_criterion
    dataset$DIFmatching <- csvdata_DIFmatching
  }
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * REACTIVES FOR DATA ####

# ** Nominal data ####
nominal <- reactive({
  data <- dataset$nominal
  # no item present in the main dataset
  validate(
    need(
      data != "missing",
      "There is no item data present in this dataset. This analysis is not available. "
    ),
    errorClass = "validation-error"
  )
  data
})

# ** Continuous data ####
continuous <- reactive({
  data <- dataset$continuous

  if (!input$data_csvdata_keep_missing) {
    data[is.na(data)] <- 0
  }
  data
})

# ** Ordinal data ####
ordinal <- reactive({
  data <- dataset$ordinal
  # no item present in the main dataset
  validate(
    need(
      data != "missing",
      "There is no item data present in this dataset. This analysis is not available. "
    ),
    errorClass = "validation-error"
  )

  if (!input$data_csvdata_keep_missing) {
    data[is.na(data)] <- 0
  }
  data
})

# ** Binary data ####
binary <- reactive({
  data <- dataset$binary
  # no item present in the main dataset
  validate(
    need(
      data != "missing",
      "There is no item data present in this dataset. This analysis is not available. "
    ),
    errorClass = "validation-error"
  )

  if (!input$data_csvdata_keep_missing) {
    data[is.na(data)] <- 0
  }
  data
})

# ** Data type ####
data_type <- reactive({
  dataset$data_type
})

# ** Key ####
key <- reactive({
  if (length(dataset$key) == 1) {
    validate(need(dataset$key != "missing", "The key needs to be provided for nominal datasets!"),
      errorClass = "validation-error"
    )
  } else {
    # incorrect dimension of key
    validate(need(
      ncol(nominal()) == length(dataset$key),
      "The length of the key needs to be the same as the number of the items of the main dataset!"
    ),
    errorClass = "validation-error"
    )
  }
  dataset$key
})

# ** Minimal values ####
minimal <- reactive({
  if (!is.null(dataset$minimal)) {
    # incorrect dimension of minimal values
    validate(need(
      ncol(nominal()) == length(dataset$minimal),
      "The length of minimal values needs to be the same as the number of items in the main dataset!"
    ),
    errorClass = "validation-error"
    )
  }
  dataset$minimal
})

# ** Maximal values ####
maximal <- reactive({
  if (!is.null(dataset$minimal)) {
    # incorrect dimension of maximal values
    validate(need(
      ncol(nominal()) == length(dataset$maximal),
      "The length of maximal values needs to be the same as the number of items in the main dataset!"
    ),
    errorClass = "validation-error"
    )
  }
  dataset$maximal
})

# ** Group ####
group <- reactive({
  if (length(dataset$group) == 1 & any(dataset$group == "missing")) {
    # missing group
    validate(need(
      dataset$group != "missing",
      "The group variable is not provided in your data, the DIF and DDF analyses are not available."
    ),
    errorClass = "validation-warning"
    )
  } else {
    # incorrect dimension of group variable
    validate(need(
      nrow(nominal()) == length(dataset$group),
      "The length of the group variable needs to be the same as the number of observations in the main dataset!"
    ),
    errorClass = "validation-error"
    )
  }
  dataset$group
})

# ** Criterion ####
criterion <- reactive({
  if (length(dataset$criterion) == 1 & any(dataset$criterion == "missing")) {
    # missing criterion
    validate(need(
      dataset$criterion != "missing",
      "The criterion variable is not provided in your data, the criterion validity analysis is not available."
    ),
    errorClass = "validation-warning"
    )
  } else {
    # incorrect dimension of criterion variable
    validate(need(
      nrow(nominal()) == length(dataset$criterion),
      "The length of the criterion variable needs to be the same as the number of observations in the main dataset!"
    ),
    errorClass = "validation-error"
    )
  }
  dataset$criterion
})

# criterion variable without validation (used by ItemAnalysis())
crit_wo_val <- reactive({
  dataset$criterion
})

# ** Observed score / DIF matching ####
DIFmatching <- reactive({
  if (length(dataset$DIFmatching) == 1 & any(dataset$DIFmatching == "missing")) {
    # missing DIF matching variable
    validate(need(
      dataset$DIFmatching != "missing",
      "The observed score variable is not provided in your data, the analyses will use the total scores."
    ),
    errorClass = "validation-warning"
    )
  } else {
    # incorrect dimension of DIF matching variable
    validate(need(
      nrow(nominal()) == length(dataset$DIFmatching), # changed to binary from nominal
      "The length of the observed score variable needs to be the same as the number of observations in the main dataset!"
    ),
    errorClass = "validation-error"
    )
  }
  dataset$DIFmatching
})

# ** Total score ####
total_score <- reactive({
  data <- continuous()
  if (input$data_toydata == "AIBS_ShinyItemAnalysis") {
    # for AIBS dataset, variable "Score" is used as total score
    data$Score
  } else {
    rowSums(data)
  }
})

# ** Standardized total score ####
z_score <- reactive({
  scale(total_score())
})

# ** Warning, if total_score() or z_score() have NAs
na_score <- reactive({
  if (any(is.na(total_score())) | any(is.na(z_score()))) {
    txt <- "<font color = 'orange'>
				For this analysis, observations with missing values have been omitted.
				</font>"
  } else {
    txt <- ""
  }
  txt
})

# ** Warning in report, if total_score() or z_score() have NAs
na_score_reports <- reactive({
  if (any(is.na(total_score())) | any(is.na(z_score()))) {
    txt <- "<font color = 'orange'>
				For some analysis methods, observations with missing values have been omitted.
				</font>"
  } else {
    txt <- ""
  }
  txt
})

output$report_na_alert <- renderUI({
  HTML(na_score_reports())
})

# ** Item numbers ####
item_numbers <- reactive({
  if (!input$data_csvdata_keep_itemnames) {
    nam <- 1:ncol(dataset$nominal)
  } else {
    nam <- colnames(dataset$nominal)
  }
  nam
})

# ** Item names ####
item_names <- reactive({
  if (!input$data_csvdata_keep_itemnames) {
    nam <- paste("Item", 1:ncol(dataset$nominal))
  } else {
    nam <- colnames(dataset$nominal)
  }
  nam
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# DATA CHECKING ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * REACTIVE VALUES ####

# ** Current status of upload/unload for data from csv files ####
data_csvdata_current_status <- reactiveValues()
data_csvdata_current_status$uploaded <- 0
data_csvdata_current_status$unloaded <- 0

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * CHECKING UPLOADED DATA FROM CSV FILES ####

# ** Error and warning messages for upload ####
data_check_text <- eventReactive(input$data_upload, {
  data_csvdata_current_status$uploaded <- 1
  data_csvdata_current_status$unloaded <- 0

  # this part writes information about uploaded dataset and variables here we
  # need to use reactiveValues instead of reactives to check uploaded data as
  # the reactives already use validate() and need() and not all of the messages
  # are printed and user does not know what may be wrong.

  # main data
  error_data <- ""
  if (dataset$data_status == "missing") {
    error_data <- "No data found! Please, upload your data. The default GMAT dataset is now in use."
  }

  # key
  error_key <- ""
  if (length(dataset$key) == 1) {
    error_key <- "The key needs to be provided for nominal datasets!"
  } else if (ncol(dataset$nominal) != length(dataset$key)) {
    error_key <- "The length of the key needs to be the same as the number of the items of the main dataset!"
  }

  # group
  error_group <- ""
  warning_group <- ""
  if (any(dataset$group == "missing", na.rm = TRUE)) {
    warning_group <- "The group variable is not provided in your data, the DIF and DDF analyses are not available."
  } else if (nrow(dataset$nominal) != length(dataset$group)) {
    error_group <- "The length of the group variable needs to be the same as the number of the observations in the main dataset!"
  }

  # criterion variable
  error_criterion <- ""
  warning_criterion <- ""
  if (any(dataset$criterion == "missing", na.rm = TRUE)) {
    warning_criterion <- "The criterion variable is not provided in your data, the criterion validity analysis is not available."
  } else if (nrow(dataset$nominal) != length(dataset$criterion)) {
    error_criterion <- "The length of the criterion variable needs to be the same as the number of observations in the main dataset!"
  }

  # criterion variable
  error_matching <- ""
  warning_matching <- ""
  if (any(dataset$DIFmatching == "missing", na.rm = TRUE)) {
    warning_matching <- "The observed score variable is not provided in your data, the analyses will use the total scores."
  } else if (nrow(dataset$nominal) != length(dataset$DIFmatching)) {
    error_matching <- "The length of the observed score variable needs to be the same as the number of observations in the main dataset!"
  }

  errors <- c(error_data, error_key, error_group, error_criterion, error_matching)
  errors <- errors[errors != ""]
  if (length(errors) > 0) {
    errors <- paste("<font color = 'red'> &#10006;", errors, "</font>", collapse = "<br>")
  } else {
    errors <- "<font color = 'green'> &#10004; Your data were successfully uploaded. Check the <b>Data exploration</b> tab. </font>"
  }

  warnings <- c(warning_group, warning_criterion, warning_matching)
  warnings <- warnings[warnings != ""]
  if (length(warnings) > 0) {
    warnings <- paste("<font color = 'orange'>", warnings, "</font>", collapse = "<br>")
  }

  paste(errors, "<br><br>", warnings)
})

output$data_check_text <- renderUI({
  HTML(data_check_text())
})

# * REMOVING DATA FROM CSV FILES ####

# ** Render remove button after data upload ####
output$data_unload_button <- renderUI({
  # if current csv data status is uploaded, creates "Unload data" button
  if (data_csvdata_current_status$uploaded > 0) {
    tagList(actionButton(
      inputId = "data_unload",
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

# ** Remove loaded data after click on "Unload data" button ####
observeEvent(input$data_unload, {
  # reset function reset values in input
  # html function change text in corresponding html tag
  useShinyjs()

  reset("data_csvdata_main")

  if (input$data_csvdata_data_type == "nominal") {
    reset("data_csvdata_key_nominal")
  } else {
    reset("data_csvdata_cutscore_ordinal")
  }
  reset("data_csvdata_cutscore_ordinal_global")

  reset("data_csvdata_maximal")
  reset("data_csvdata_minimal")
  reset("data_csvdata_maximal_global")
  reset("data_csvdata_minimal_global")

  reset("data_csvdata_group")
  reset("data_csvdata_criterion")
  reset("data_csvdata_DIFmatching")

  reset("data_upload")

  dataset$key_upload_status <- "reset"

  html("data_check_text", html = "")

  # binary data consisting only of zeros or ones
  html("data_check_binary_all01_text", html = "")
  reset("data_remove_binary_all01_button")
  html("data_check_binary_all01_confirmation", html = "")

  # group with missing values
  html("data_check_group_withNA_text", html = "")
  reset("data_remove_group_withNA_button")
  html("data_remove_group_withNA_confirmation", html = "")

  data_csvdata_current_status$uploaded <- 0 # reset, data is not uploaded
  data_csvdata_current_status$unloaded <- 1 # data is unloaded
  removeUI(selector = "#data_unload")
  data_csvdata_current_status$unloaded <- 0 # reset, data is not unloaded
})

# ** CHECKING BINARY ITEMS WHETHER THEY CONSIST ONLY OF ZEROS OR ONES ####

# ** Binary items consisting only of zeros or ones ####
data_check_binary_all01 <- reactive({
  data <- binary()

  all0 <- sapply(data, function(x) all(x == 0, na.rm = TRUE))
  all1 <- sapply(data, function(x) all(x == 1, na.rm = TRUE))

  list(all0 = all0, all1 = all1)
})

# ** Text with message about binary items consisting only of zeros or ones ####
data_check_binary_all01_text <- eventReactive((input$data_upload | input$data_remove_binary_all01), {
  all0 <- data_check_binary_all01()$all0
  all1 <- data_check_binary_all01()$all1

  if (any(all0)) {
    txt0 <- paste(
      "It seems that items",
      paste(item_names()[all0], collapse = ", "),
      "consists only of zeros. "
    )
  } else {
    txt0 <- ""
  }
  if (any(all1)) {
    txt1 <- paste(
      "It seems that items",
      paste(item_names()[all1], collapse = ", "),
      "consists only of ones. "
    )
  } else {
    txt1 <- ""
  }

  # warning
  if (any(all0) | any(all1)) {
    txt <- paste(
      "<br><br>Check your data!",
      txt0, "<br>", txt1,
      "<br>Some analyses may not work properly. Consider removing such items.
      For this purpose, you can use the <b>Remove items</b> button on the right side. <br><br>"
    )
    txt <- paste("<font color = 'red'>", txt, "</font>")
  } else {
    txt <- ""
  }

  txt
})

output$data_check_binary_all01_text <- renderUI({
  HTML(data_check_binary_all01_text())
})

# ** Render button for removing binary items consisting only of zeros or ones ####
output$data_remove_binary_all01_button <- renderUI({
  all0 <- data_check_binary_all01()$all0
  all1 <- data_check_binary_all01()$all1

  if (input$data_upload & (any(all0) | any(all1))) {
    tagList(
      actionButton(
        inputId = "data_remove_binary_all01",
        label = "Remove items",
        class = "btn btn-large btn-primary",
        icon = icon("trash"),
        width = "150px"
      )
    )
  }
})

# ** Removing binary items consisting only of zeros or ones ####
observeEvent(input$data_remove_binary_all01, {
  ok0 <- (!data_check_binary_all01()$all0)
  ok1 <- (!data_check_binary_all01()$all1)

  dataset$key <- key()[(ok0 & ok1)]

  dataset$nominal <- nominal()[, (ok0 & ok1), with = FALSE]
  dataset$ordinal <- ordinal()[, (ok0 & ok1), with = FALSE]
  dataset$binary <- binary()[, (ok0 & ok1), with = FALSE]
})

# ** Confirmation about binary items consisting only of zeros or ones removal ####
data_check_binary_all01_confirmation <- eventReactive(input$data_remove_binary_all01, {
  txt <- "Items were removed."
  txt <- paste("<font color = 'green'>", txt, "</font>")
  txt
})

output$data_check_binary_all01_confirmation <- renderUI({
  HTML(data_check_binary_all01_confirmation())
})

# ** CHECKING GROUP ####

# ** Checking uploaded group variable includes missing values ####
data_check_group_withNA <- reactive({
  group <- group()
  # are there any missing values?
  NAgroup <- is.na(group)
  NAgroup
})

# ** Text for check of uploaded group membership ####
data_check_group_withNA_text <- eventReactive(((input$data_upload)), {
  txt <- ""
  if (all(dataset$group != "missing", na.rm = TRUE)) {
    NAgroup <- data_check_group_withNA()
    if (any(NAgroup)) {
      txt <- paste(
        sum(NAgroup),
        ifelse(sum(NAgroup) == 1,
          "observation has",
          "observations have"
        ),
        "missing group membership. <br>
                 Some analyses may not work properly. Consider removing such items.
                 For this purpose, you can use the <b>Remove data</b> button on the right side. <br><br>"
      )
      txt <- paste("<font color = 'red'>", txt, "</font>")
    }
  }
  txt
})

output$data_check_group_withNA_text <- renderUI({
  HTML(data_check_group_withNA_text())
})

# ** Render button for removing data with missing group variable ####
output$data_remove_group_withNA_button <- renderUI({
  if (all(dataset$group != "missing", na.rm = TRUE)) {
    if (input$data_upload & any(data_check_group_withNA())) {
      tagList(
        actionButton(
          inputId = "data_remove_group_withNA",
          label = "Remove data",
          class = "btn btn-large btn-primary",
          icon = icon("trash"),
          width = "150px"
        )
      )
    }
  }
})

# ** Removing data with missing group variable ####
observeEvent(input$data_remove_group_withNA, {
  OKgroup <- (!data_check_group_withNA())

  dataset$group <- dataset$group[OKgroup]
  # exclude when criterion is missing
  if (length(dataset$criterion) == length(OKgroup)) {
    dataset$criterion <- dataset$criterion[OKgroup]
  }

  dataset$nominal <- dataset$nominal[OKgroup]
  dataset$ordinal <- dataset$ordinal[OKgroup]
  dataset$binary <- dataset$binary[OKgroup]
})

# ** Confirmation about removing data with missing group variable ####
data_check_group_withNA_confirmation <- eventReactive(input$data_remove_group_withNA, {
  txt <- "Rows with the missing group membership were removed."
  txt <- paste("<font color = 'green'>", txt, "</font>")
  txt
})

output$data_check_group_withNA_confirmation <- renderUI({
  HTML(data_check_group_withNA_confirmation())
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# DATA DESCRIPTION ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data_description_Input <- reactive({
  data_name <- input$data_toydata
  txt <- switch(data_name,
    GMAT_difNLR = "<code>GMAT</code> <a href='https://doi.org/10.1187/cbe.16-10-0307' target='_blank'>(Martinkova et al., 2017)</a>
                is a generated dataset based on the parameters of a real Graduate Management Admission Test (GMAT; Kingston et al., 1985)
                from the <code>difNLR</code> package. This dataset represents the responses of 2,000 subjects (1,000 males coded as
                <code>'0'</code>, 1,000 females coded as <code>'1'</code>) to a multiple-choice test of 20 items. It also contains
                generated continuous criterion intended to be predicted by the test. As discussed in
                <a href='https://doi.org/10.1187/cbe.16-10-0307' target='_blank'> Martinkova et al. (2017)</a>, the dataset was simulated
                in order to provide an example of DIF items present even in the case of identical total score distributions. To replicate the
                example provided in <a href='https://doi.org/10.1187/cbe.16-10-0307' target='_blank'> Martinkova et al. (2017)</a>,
                select the <code>GMAT</code> dataset and go to <code>DIF/Fairness</code> section. ",
    GMAT2_difNLR = "<code>GMAT2</code> <a href='https://doi.org/10.1111/jedm.12158' target='_blank'>(Drabinova & Martinkova, 2017)</a> is
                a simulated dataset based on the parameters of a real Graduate Management Admission Test (GMAT; Kingston et al., 1985) from the
                <code>difNLR</code> package. First two items were simulated to function differently in the uniform and the non-uniform way respectively.
                The dataset represents the responses of 1,000 subjects (500 males coded as <code>'0'</code>, 500 females coded as <code>'1'</code>)
                to a multiple-choice test of 20 items. ",
    MSATB_difNLR = "<code>MSAT-B</code> <a href='https://doi.org/10.1111/jedm.12158' target='_blank'>(Drabinova & Martinkova, 2017)</a> is
                a subset of a real Medical School Admission Test in Biology (MSAT-B) in the Czech Republic from the <code>difNLR</code> package.
                The dataset represents the responses of 1,407 subjects (484 males coded as <code>'0'</code>, 923 females coded as <code>'1'</code>)
                to a multiple-choice test of 20 items. The first item was previously detected as functioning differently. For more details
                on the item selection, see <a href='https://doi.org/10.1111/jedm.12158' target='_blank'>Drabinova and Martinkova (2017)</a>.
                To replicate the example provided in <a href='https://doi.org/10.1111/jedm.12158' target='_blank'> Drabinova and Martinkova
                (2017)</a>, select the <code>MSAT-B</code> dataset and go to the <code>DIF/Fairness</code> section. ",
    dataMedical_ShinyItemAnalysis = "<code>Medical 100</code> <a href='https://doi.org/10.5817/TF2017-9-129' target='_blank'>
                (Martinkova et al., 2017)</a> is a real <code>dataMedical</code> dataset of an admission test to a medical
                school from the <code>ShinyItemAnalysis</code> package. The data set represents the responses of 2,392 subjects
                (750 males coded as <code>'0'</code>, 1,633 females coded as <code>'1'</code>, and 9 subjects without gender
                specification coded as <code>'NA'</code>) to a multiple-choice test of 100 items. The dataset contains a criterion
                variable &ndash; an indicator whether the student studied standardly or not. In <a href='https://doi.org/10.5817/TF2017-9-129'
                target='_blank'> Martinkova et al. (2017)</a>, the dataset was used to demonstrate the earlier
                version of the <code>ShinyItemAnalysis</code> interactive app. ",
    dataMedicalgraded_ShinyItemAnalysis = "<code>Medical 100 Graded</code> is a real <code>dataMedicalgraded</code> dataset
                of an admission test to a medical school from the <code>ShinyItemAnalysis</code> package. The dataset represents
                the responses of 2,392 subjects (750 males coded as <code>'0'</code>, 1,633 females coded as <code>'1'</code>, and
                9 subjects without gender specification coded as <code>'NA'</code>) to a multiple-choice test of 100 items. Each
                item is graded with 0 to 4 points. Maximum of 4 points were set if all correct answers and none of incorrect
                answers were selected. This dataset contains a criterion variable &ndash; an indicator whether the student studied standardly
                or not. For analyses where dichotomous items are necessary (e.g., logistic models in Regression, IRT models, or
                DIF detection methods), data are binarized &ndash; <code>'1'</code> means that student gained maximum
                number of points, i.e., 4; otherwise the item is scored as <code>'0'</code>. ",
    HCI_ShinyItemAnalysis = "<code>HCI</code> <a href='http://dx.doi.org/10.1187/cbe.16-10-0305' target='_blank'>(McFarland et al.,
                2017)</a> is a real dataset of the Homeostasis Concept Inventory (HCI) from the <code>ShinyItemAnalysis</code> package.
                The dataset represents the responses of 651 subjects (405 males coded as <code>'0'</code>, 246 females coded as
                <code>'0'</code>) to a multiple-choice test of 20 items. The <code>HCI</code> dataset contains a criterion variable &ndash;
                an indicator whether the student plans to major in the life sciences. In <a href='http://dx.doi.org/10.1187/cbe.16-10-0305'
                target='_blank'> McFarland et al. (2017)</a>, the dataset was used to perform complex a validation of the HCI
                assessment instrument. The dataset was also used for demonstrations of the <code>ShinyItemAnalysis</code> package
                and an earlier version of this online app in The R Journal paper by <a href='https://doi.org/10.32614/RJ-2018-074'
                target='_blank'> Martinkova and Hladka (2018)</a>. ",
    Science_mirt = "<code>Science</code> dataset comes from the <code>mirt/ltm</code> packages. It represents the responses of 392 subjects
                on a 4-item test describing attitude to science and technology. Selected items are <code>Comfort</code>, <code>Work</code>,
                <code>Future</code>, and <code>Benefit</code>. All items are measured on the same scale with the response categories:
                <code>'strongly disagree'</code>, <code>'disagree to some extent'</code>, <code>'agree to some extent'</code>, and
                <code>'strongly agree'</code>. See Karlheinz and Melich (1992) for more details. For analyses where dichotomous items
                are necessary (e.g., the logistic regression models in Regression, the IRT models, or the DIF detection methods),
                data is binarized &ndash; <code>'1'</code> means that respondent <code>'strongly agrees'</code> with a given item; otherwise
                the item is scored as <code>'0'</code>. ",
    LearningToLearn_ShinyItemAnalysis_6 = "<code>Learning To Learn 6</code> <a href='https://doi.org/10.1016/j.learninstruc.2019.101286'
                target='_blank'> (Martinkova et al., 2020)</a> is a subset of the longitudinal <code>LearningToLearn</code> dataset from the
                <code>ShinyItemAnalysis</code> package. It consists of answers to the Learning to Learn test in Grade 6 only. The same
                respondents were also tested in Grade 9 &ndash; respective data is available in the <code>Learning To Learn 9</code>
                dataset. The dataset represents the binary-coded responses of 782 subjects (391 from basic school track, BS, coded here as
                <code>'0'</code>; 391 from selecive academic track, AS, coded here as <code>'1'</code>) to the multiple-choice test
                consisting of 41 items within 7 subscales. This dataset was created using the propensity score matching algorithm to achieve
                similar characteristics in both tracks. For further details, see <a href='https://doi.org/10.1016/j.learninstruc.2019.101286'
                target='_blank'> Martinkova, Hladka, and Potuznikova (2020)</a>.",
    LearningToLearn_ShinyItemAnalysis_9 = "<code>Learning To Learn 9</code> <a href='https://doi.org/10.1016/j.learninstruc.2019.101286'
                target='_blank'> (Martinkova et al., 2020)</a> is a subset of the longitudinal <code>LearningToLearn</code> dataset from the
                <code>ShinyItemAnalysis</code> package. It consists of answers to the Learning to Learn test in Grade 9 only. The same
                respondents were also tested in Grade 6 &ndash; respective data is available in the <code>Learning To Learn 6</code> dataset.
				        The dataset represents the binary-coded responses of 782 subjects (391 from basic school track, BS, coded here as <code>'0'</code>;
				        391 from selecive academic track, AS, coded here as <code>'1'</code>) to the multiple-choice test consisting of 41
				        items within 7 subscales. The dataset was created using the propensity score matching algorithm to achieve similar
				        characteristics in both tracks. The dataset also includes observed total score from Grade 6, which is offered in the Regression
				        section and in the DIF/Fairness section as the matching variable. In their paper,
				        <a href='https://doi.org/10.1016/j.learninstruc.2019.101286' target='_blank'> Martinkova et al. (2020)</a> found significant
				        DIF in change present in some of the items, while no significant difference in the change was present in the total scores.
				        The dataset was also used for demonstration purposes in The R Journal paper on the <code>difNLR</code> package
                <a href='https://doi.org/10.32614/RJ-2020-014' target='_blank'>(Hladka & Martinkova, 2020)</a>. ",
    AIBS_ShinyItemAnalysis = "<code>AIBS Grant Peer Review Scoring</code> dataset <a href='https://doi.org/10.6084/m9.figshare.12728087'
                target='_blank'> (Gallo, 2021)</a> comes from the scientific peer review of biomedical applications from an intramural collaborative
                biomedical research funding program (2014-2017). The data presented in this app include anonymized proposal ID, reviewer ID,
                and an overall score from three reviewers, scored on a scale from 1.0 (best) to 5.0 (worst) with a 0.1 gradation. The
                dataset was used by <a href='http://doi.org/10.1111/rssa.12681' target='_blank'> Erosheva, Martinkova, and Lee (2021)</a> to
                demonstrate issues with the estimation of inter-rater
                reliability (IRR) in range-restricted samples. To try interactively the range-restricted IRR analysis, select the
                <code>AIBS</code> dataset and go to the <code>Reliability/Restricted range</code> section."
  )
  txt
})

output$data_description <- renderUI({
  HTML(data_description_Input())
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# BASIC SUMMARY ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# * Main dataset ####

# ** Dimension ####
output$data_rawdata_dim <- renderText({
  if (input$data_toydata == "AIBS_ShinyItemAnalysis") {
    txt <- paste0(
      "The AIBS is not a multi-item dataset. It consists of ", nrow(continuous()),
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
  if (data_type() == "ordinal") {
    data_ordinal_summary()
  } else if (data_type() == "nominal") {
    data_nominal_summary()
  } else if (data_type() == "continuous") {
    data_continuous_summary()
  } else {
    data_binary_summary()
  }
})

# * Group ####
output$data_group_summary <- renderPrint({
  gr <- as.factor(group())
  summary(gr)
})

# * Criterion ####
output$data_criterion_summary <- renderPrint({
  criterion <- criterion() # has to be called outside the S3 methods, otherwise returns ugly warnings
  summary(criterion)
})

# * DIF matching ####
output$data_DIFmatching_summary <- renderPrint({
  DIFmatching <- DIFmatching()
  summary(DIFmatching)
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# TEST OF APPLICATION ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
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

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# DATA EXPLORATION ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# * Main dataset ####
output$data_exploration_main <- DT::renderDataTable(
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
output$data_exploration_key <- DT::renderDataTable(
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
output$data_exploration_binary <- DT::renderDataTable(
  {
    data_table <- binary()
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

# all vector variables into one DT
output$data_exploration_variables <- DT::renderDataTable(
  {
    tibble(
      Group = ifelse(dataset$group != "missing", group(), "NA"),
      Criterion = ifelse(dataset$criterion != "missing", criterion(), "NA"),
      "Total score" = total_score(),
      "Observed score vector" = ifelse(dataset$DIFmatching != "missing", DIFmatching(), "NA")
    )
  },
  rownames = TRUE,
  style = "bootstrap",
  options = list(
    columnDefs = list(list(className = "dt-right", targets = "_all")),
    scrollX = TRUE,
    server = TRUE,
    pageLength = 6,
    scrollCollapse = TRUE,
    dom = "tipr"
  )
)
