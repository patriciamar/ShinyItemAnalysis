# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# RELIABILITY ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * SPEARMAN-BROWN FORMULA ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Update numeric inputs ######
observe({
  rel1 <- round(as.numeric(reliability_cronbachalpha_table_Input()[, 1]), 2)
  rel2 <- min(0.99, rel1 + 0.1)

  items1 <- ncol(binary())
  items2 <- items1 + 10


  updateNumericInput(
    session = session,
    inputId = "reliability_SBformula_reliability_original",
    value = rel1
  )
  updateNumericInput(
    session = session,
    inputId = "reliability_SBformula_reliability_new",
    value = rel2
  )
  updateNumericInput(
    session = session,
    inputId = "reliability_SBformula_items_original",
    value = items1
  )
  updateNumericInput(
    session = session,
    inputId = "reliability_SBformula_items_new",
    value = items2
  )
})

# ** Calculation of reliability ####
output$reliability_SBformula_reliability_text <- renderUI({
  rel_ori <- input$reliability_SBformula_reliability_original
  ite_ori <- input$reliability_SBformula_items_original

  ite_new <- input$reliability_SBformula_items_new

  m <- ite_new / ite_ori

  rel_new <- psychometric::SBrel(Nlength = m, rxx = rel_ori)

  txt <- paste(
    "Reliability of a test with", ite_new,
    ifelse(ite_new == 1, "item", "items"),
    "would be <b>",
    round(rel_new, 3), "</b>"
  )
  HTML(txt)
})

# ** Calculation of test length ####
output$reliability_SBformula_items_text <- renderText({
  rel_ori <- input$reliability_SBformula_reliability_original
  ite_ori <- input$reliability_SBformula_items_original

  rel_new <- input$reliability_SBformula_reliability_new


  m <- psychometric::SBlength(rxxp = rel_new, rxx = rel_ori)
  ite_new <- ceiling(m * ite_ori)

  txt <- paste(
    "It is necessary to have <b>", ite_new,
    ifelse(ite_new == 1, "item", "items"),
    "</b> to gain reliability of",
    round(rel_new, 3)
  )
  HTML(txt)
})


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * SPLIT-HALF ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * BETA ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Split-half number of split halves heading ######
output$reliability_splithalf_number_label <- renderText({
  method <- input$reliability_splithalf_method

  if (method %in% c("average", "worst")) {
    txt <- "Number of split halves"
  } else {
    txt <- "Number of split halves (for histogram)"
  }

  txt
})

# ** Split-half items ######
reliability_splithalf_items <- reactive({
  method <- input$reliability_splithalf_method

  i_num <- item_numbers()

  n <- length(i_num)

  if (method == "firstlast") {
    k <- ceiling(n / 2)
    items1 <- 1:k
  } else {
    if (method == "evenodd") {
      items1 <- seq(1, n, 2)
    } else {
      if (method == "random") {
        samp <- sample(1:n, ceiling(n / 2))
        items1 <- sort(samp)
      } else {
        if (method == "worst") {
          split <- reliability_splithalf_raw()
          items1 <- which(split$minAB[, "A"] == 1)
        } else {
          items1 <- NULL
        }
      }
    }
  }

  items2 <- setdiff(1:n, items1)

  list(items1 = items1, items2 = items2)
})

# ** Split-half text items ######
output$reliability_splithalf_text <- renderUI({
  items <- reliability_splithalf_items()

  items1 <- items$items1
  items2 <- items$items2

  if (is.null(items1)) {
    txt <- NULL
  } else {
    txt <- paste(
      "The first subset contains: <b>",
      paste(item_names()[items1], collapse = ", "), "</b> <br>",
      "The second subset contains: <b>",
      paste(item_names()[items2], collapse = ", "), "</b> <br>"
    )
  }

  HTML(txt)
})

# ** Update numeric inputs ######
observe({
  data <- ordinal()
  n <- ncol(data)

  k <- ceiling(n / 2)
  num <- ifelse(k == n / 2, choose(n, k) / 2, choose(n, k))

  updateNumericInput(
    session = session,
    inputId = "reliability_splithalf_method",
    max = choose(n, k)
  )
})

# ** Split-half all possible split-halves ######
output$reliability_splithalf_allpossible_text <- renderUI({
  data <- ordinal()
  n <- ncol(data)

  k <- ceiling(n / 2)
  num <- ifelse(k == n / 2, choose(n, k) / 2, choose(n, k))

  txt <- paste("<b>Note:</b> For a dataset with <b>", n, "items</b>,
               there are <b>", num, "</b> possible split-halves")

  HTML(txt)
})

# ** Split-half correlation and reliability calculation for average/worst method ######
reliability_splithalf_raw <- reactive({
  data <- ordinal()

  n.sample <- input$reliability_splithalf_number
  split <- psych::splitHalf(data, raw = T, n.sample = n.sample)

  split
})

# ** Split-half correlation and reliability calculation ######
reliability_splithalf_estimate <- reactive({
  items <- reliability_splithalf_items()

  items1 <- items$items1
  items2 <- items$items2

  i_num <- item_numbers()

  n <- length(i_num)

  data <- ordinal()

  if (is.null(items1)) {
    split <- reliability_splithalf_raw()

    rel.y <- mean(split$raw)
    n <- length(split$raw)
    rel.low <- rel.y - 1.96 * sd(split$raw) / sqrt(n)
    rel.upp <- rel.y + 1.96 * sd(split$raw) / sqrt(n)
  } else {
    df1 <- data[, items1, with = F]
    df2 <- data[, items2, with = F]

    ts1 <- apply(df1, 1, sum)
    ts2 <- apply(df2, 1, sum)

    cor.y <- cor(ts1, ts2)
    z.r <- 0.5 * log((1 + cor.y) / (1 - cor.y))
    n <- length(ts1)
    z.low <- z.r - 1.96 * sqrt(1 / (n - 3))
    z.upp <- z.r + 1.96 * sqrt(1 / (n - 3))


    cor.low <- (exp(2 * z.low) - 1) / (exp(2 * z.low) + 1)
    cor.upp <- (exp(2 * z.upp) - 1) / (exp(2 * z.upp) + 1)

    rel.y <- 2 * cor.y / (1 + cor.y)
    rel.low <- 2 * cor.low / (1 + cor.low)
    rel.upp <- 2 * cor.upp / (1 + cor.upp)
  }

  list(
    rel.y = rel.y,
    rel.low = rel.low,
    rel.upp = rel.upp
  )
})


# ** Split-half correlation and reliability calculation ######
output$reliability_splithalf_table <- renderTable(
  {
    tab <- reliability_splithalf_estimate()
    tab <- unlist(tab)
    tab <- data.table(
      "Estimate" = tab[1],
      "Confidence interval" = paste0(
        "(", sprintf("%.3f", tab[2]), ", ",
        sprintf("%.3f", tab[3]), ")"
      )
    )

    tab
  },
  digits = 3
)

# ** Split-halves histogram ######
reliability_splithalf_histogram_Input <- reactive({
  data <- ordinal()

  split <- reliability_splithalf_raw()

  val <- reliability_splithalf_estimate()[[1]]

  val <- round(val, 2)
  col <- as.factor(val - 0.005 < split$raw & split$raw < val + 0.005)
  brk <- sort(unique(round(split$raw, 2)))

  ggplot(data.frame(rel = split$raw), aes(x = rel, fill = col)) +
    geom_histogram(col = "black", binwidth = 0.01) +
    scale_fill_manual(values = c("grey45", "red")) +
    scale_x_continuous(breaks = brk) +
    ylab("Count") +
    xlab("Reliability estimate") +
    theme_app()
})

# ** Split-halves histogram output ######
output$reliability_splithalf_histogram <- renderPlotly({
  g <- reliability_splithalf_histogram_Input()
  p <- ggplotly(g)

  for (i in 1:length(p$x$data)) {
    text <- p$x$data[[i]]$text
    text <- gsub("count", "Count", text)
    text <- gsub("rel", "Reliability", text)
    text <- gsub("col: FALSE", "", text)
    text <- gsub("col: TRUE", "", text)
    p$x$data[[i]]$text <- text
  }

  p$elementId <- NULL
  p %>% plotly::config(displayModeBar = FALSE)
})

# ** DB for Split-halves histogram ######
output$DB_reliability_splithalf_histogram <- downloadHandler(
  filename = function() {
    "fig_reliability_splithalf.png"
  },
  content = function(file) {
    ggsave(file,
      plot = reliability_splithalf_histogram_Input() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * ALPHA ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Cronbach's alpha table ######
reliability_cronbachalpha_table_Input <- reactive({
  data <- ordinal()

  a <- ShinyItemAnalysis:::cronbach_alpha(data)

  tab <- data.table(
    "Estimate" = sprintf("%.3f", a$estimate),
    "Confidence interval" = paste0(
      "(", sprintf("%.3f", a$ci[1]), ", ",
      sprintf("%.3f", a$ci[2]), ")"
    )
  )

  tab
})

# ** Output Cronbach's alpha table ######
output$reliability_cronbachalpha_table <- renderTable(
  {
    reliability_cronbachalpha_table_Input()
  },
  digits = 3,
  include.rownames = F,
  include.colnames = T
)

# # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# # * INTRA-CLASS CORRELATION ######
# # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# # ** Intra-class correlation table ######
# output$reliability_icc_table <- renderTable({
#   # TODO: add select raters (items) input
#   # ordinal()
#   aibs_long() %>% # TODO general
#     pivot_wider(ID, values_from = Score, names_from = RevCode) %>% # TODO general
#     select(-ID) %>% # TODO general
#     psych::ICC() %>%
#     pluck("results")
# })
#
#
# # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# # * Restricted-range ICC ######
# # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# # ** Usable data definition ######
# aibs_long <- reactive({ # TODO general
#   validate(need(
#     input$data_toydata == "AIBS_ShinyItemAnalysis",
#     "At this moment, the method is compatible with the 'AIBS Grant Peer Review Scoring' dataset only. "
#   ),
#   errorClass = "validation-error"
#   )
#   continuous()
# })
#
# k_max <- reactive({
#   aibs_long()$ScoreRankAdj %>% max(na.rm = TRUE) # TODO general
# })
#
# # transform percentage to Ks (check against Ks, not percents)
# n_sel <- reactive({
#   round(input$reliability_restricted_proportion * .01 * k_max())
# })
#
# # ** Update slider input ######
# observe({
#   updateSliderInput(
#     session,
#     inputId = "reliability_restricted_proportion",
#     min = ceiling(200 / k_max())
#   )
# })
#
#
# # this chunk ensures slider is reset to 100% whenever direction or BS samples are changed
# # however, it is rather lenient and causes some issues, e.g. double estimation on BS num change
# # in addition, user request should be fulfilled without any unsolicited actions, such as slider change
#
# # observeEvent(c(
# #   input$data_toydata,
# #   input$reliability_restricted_clear,
# #   input$reliability_restricted_direction,
# #   input$reliability_restricted_bootsamples
# # ), {
# #   updateSliderInput(
# #     session,
# #     inputId = "reliability_restricted_proportion",
# #     value = 100
# #   )
# # })
#
# # ** Caterpillar plot input ######
# reliability_restricted_caterpillarplot_input <- reactive({
#   aibs_long() %>%
#     mutate(hl = case_when(
#       input$reliability_restricted_direction == "top" & ScoreRankAdj <= n_sel() ~ "sol",
#       input$reliability_restricted_direction == "bottom" & ScoreRankAdj > (k_max() - n_sel()) ~ "sol",
#       TRUE ~ "alp"
#     ) %>% factor(levels = c("alp", "sol"))) %>%
#     ggplot(aes(x = ScoreRankAdj, y = Score, group = ID, alpha = hl)) +
#     geom_line(col = "gray") +
#     geom_point(aes(text = paste0(
#       "ID: ", ID, "\n", # TODO generalize
#       "Rank: ", ScoreRankAdj, "\n", # TODO generalize
#       "Rating: ", Score, "\n" # add avg score? not in every dataset, computed only in ICCrestricted
#     )), shape = 1, size = 1.5) +
#     stat_summary(
#       fun = mean, fun.args = list(na.rm = TRUE), geom = "point", col = "red",
#       shape = 5, size = 2.5, stroke = .35
#     ) +
#     scale_alpha_discrete(range = c(.3, 1), drop = FALSE) +
#     coord_cartesian(ylim = c(1, 5)) +
#     labs(x = "Rated subject/object rank", y = "Rating (score)") +
#     theme_app()
# })
#
# # ** Plotly output for caterpillar plot ######
# output$reliability_restricted_caterpillarplot <- renderPlotly({
#   reliability_restricted_caterpillarplot_input() %>%
#     ggplotly(tooltip = c("text")) %>%
#     plotly::config(displayModeBar = FALSE)
# })
#
# # ** DB for caterpillar plot ######
# output$DB_reliability_restricted_caterpillarplot <- downloadHandler(
#   filename = function() {
#     "fig_reliability_caterpillar.png"
#   },
#   content = function(file) {
#     ggsave(file,
#       plot = reliability_restricted_caterpillarplot_input() +
#         theme(text = element_text(size = setting_figures$text_size)),
#       device = "png",
#       height = setting_figures$height, width = setting_figures$width,
#       dpi = setting_figures$dpi
#     )
#   }
# )
#
#
# reliability_restricted_res <- reactiveValues(vals = NULL) # init ICCs bank
#
# observeEvent(
#   # do not depend on "reliability_restricted_res"
#   c(
#     input$data_toydata,
#     input$reliability_restricted_clear,
#     input$reliability_restricted_proportion,
#     input$reliability_restricted_direction,
#     input$reliability_restricted_bootsamples
#   ),
#   {
#     data <- aibs_long()
#
#     isolate({
#       entries <- reliability_restricted_res$vals %>%
#         names()
#
#       # propose a new entry
#       new_entry <- paste0(
#         "dir-", input$reliability_restricted_direction,
#         "_bs-", input$reliability_restricted_bootsamples,
#         "_sel-", n_sel()
#       )
#
#       # check if proposed not already available, else compute
#       if (new_entry %in% entries) {
#         message("rICC computation: using already computed value...")
#       } else {
#         reliability_restricted_res[["vals"]][[new_entry]] <- ICCrestricted(
#           data,
#           case = "ID", # TODO general
#           var = "Score", # TODO general
#           rank = "ScoreRankAdj", # TODO general
#           dir = input$reliability_restricted_direction,
#           sel = n_sel(),
#           nsim = input$reliability_restricted_bootsamples
#         )
#       }
#     })
#   }
# )
#
# # ** Clearing ICC computed entries ######
# observeEvent(input$reliability_restricted_clear, {
#   cat("Clearing ICC bank...")
#   reliability_restricted_res$vals <- NULL
# })
#
# # ** ICC plot - current choice ######
# reliability_restricted_iccplot_curr <- reactive({
#   plt_data <- reliability_restricted_res$vals %>%
#     bind_rows(.id = "name") %>%
#     filter(str_detect(
#       .data$name,
#       paste0(
#         "dir-", input$reliability_restricted_direction,
#         "_bs-", input$reliability_restricted_bootsamples
#       )
#     ))
#
#   # translate empty tibble to NULL for further use in req()
#   if (nrow(plt_data) == 0) {
#     NULL
#   } else {
#     plt_data
#   }
# })
#
# # ** Reliability plot input ######
# reliability_restricted_iccplot_input <- reactive({
#   req(aibs_long()) # propagate validation msg to the plot
#   req(reliability_restricted_iccplot_curr())
#
#   curr_plt_name <- paste0(
#     "dir-", input$reliability_restricted_direction,
#     "_bs-", input$reliability_restricted_bootsamples,
#     "_sel-", n_sel()
#   )
#
#   reliability_restricted_iccplot_curr() %>%
#     mutate(hl = if_else(name == curr_plt_name, "sol", "alp") %>%
#       factor(levels = c("alp", "sol"))) %>%
#     ggplot(aes(prop_sel, ICC1, ymin = ICC1_LCI, ymax = ICC1_UCI, alpha = hl)) + # TODO general
#     geom_linerange() + # separate as plotly messes up otherwise
#     geom_point(aes(text = paste0(
#       ifelse(prop_sel == 1, "Complete range",
#         paste0(
#           "Proportion of ", dir, " subjects/objects: ", scales::percent(prop_sel, .01)
#         )
#       ), "\n",
#       "ICC1: ", round(ICC1, 2), "\n",
#       "LCI: ", round(ICC1_LCI, 2), "\n",
#       "UCI: ", round(ICC1_UCI, 2)
#     ))) +
#     scale_x_continuous(labels = scales::percent) +
#     scale_alpha_discrete(range = c(.25, 1), drop = FALSE) +
#     labs(
#       x = paste0("Proportion of ", input$reliability_restricted_direction, " rated subjects/objects"),
#       y = "Reliability"
#     ) +
#     coord_cartesian(ylim = c(0, 1), xlim = c(0, 1)) + # TODO general
#     theme_app()
# })
#
# # ** Reliability plot render ######
# output$reliability_restricted_iccplot <- renderPlotly({
#   reliability_restricted_iccplot_input() %>%
#     ggplotly(tooltip = "text") %>%
#     plotly::config(displayModeBar = FALSE)
# })
#
# # ** DB for ICC reliability plot ######
# output$DB_reliability_restricted_iccplot <- downloadHandler(
#   filename = function() {
#     "fig_reliability_resctricted.png"
#   },
#   content = function(file) {
#     ggsave(file,
#       plot = reliability_restricted_iccplot_input() +
#         theme(text = element_text(size = setting_figures$text_size)),
#       device = "png",
#       height = setting_figures$height, width = setting_figures$width,
#       dpi = setting_figures$dpi
#     )
#   }
# )
#
# # ** DB for ICC reliability table ######
# output$DB_reliability_restricted_iccdata <- downloadHandler(
#   filename = function() {
#     "range_restricted_reliability_data.csv"
#   },
#   content = function(file) {
#     data <- reliability_restricted_iccplot_curr() %>% select(-name)
#     write.csv(data, file, row.names = FALSE)
#   }
# )
#
# output$icc_text <- renderText({
#   req(aibs_long())
#
#   # isolate({
#   full <- reliability_restricted_res[["vals"]][[paste0(
#     "dir-", input$reliability_restricted_direction,
#     "_bs-", input$reliability_restricted_bootsamples,
#     "_sel-", k_max()
#   )]]
#   # })
#
#   curr <- reliability_restricted_res[["vals"]][[paste0(
#     "dir-", input$reliability_restricted_direction,
#     "_bs-", input$reliability_restricted_bootsamples,
#     "_sel-", n_sel()
#   )]]
#
#
#   full_part <- if (is.null(full)) {
#     "Please, set the slider to 100% in order to estimate and display reliability the complete dataset."
#   } else {
#     paste0(
#       "For the complete dataset, the estimated reliability is ",
#       round(full$ICC1, 2), ", with 95% CI of [", round(full$ICC1_LCI, 2), ", ", round(full$ICC1_UCI, 2), "]."
#     )
#   }
#   curr_part <- if (identical(curr, full) | is.null(curr)) {
#     "Set the slider to different value to see how the estimate changes for other subset of the data."
#   } else {
#     paste0(
#       "For the ", round(curr$prop_sel * 100), "%",
#       " (that is ", curr$n_sel, ") of ", curr$dir, " subjects/objects (proposals in the case of AIBS dataset),",
#       " the estimated reliability is ",
#       round(curr$ICC1, 2), ", with 95% CI of [", round(curr$ICC1_LCI, 2), ", ", round(curr$ICC1_UCI, 2), "]."
#     )
#   }
#
#   paste(full_part, curr_part)
# })
