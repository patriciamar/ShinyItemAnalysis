
# DATA --------------------------------------------------------------------

IRT_bock_data <- reactive({
  d <- nominal()
  d %>% modify(as.factor) # not nominal data to nominal
})

IRT_bock_key <- reactive({
  as.factor(key())
})


# PARAMETRIZATION ---------------------------------------------------------

# collapse "models"
# (those pairs are identical, the reparametrization occurs at coef)
IRT_bock_parametrization <- reactive({
  switch(input$IRT_bock_parametrization,
    blis = "blis",
    blirt = "blis",
    thissen = "thissen",
    bock = "thissen"
  )
})


# MODEL FIT ---------------------------------------------------------------

# so when one model is fitted, the second model of each pair is not refitted
# unnecessarily
IRT_bock_fit_and_orig_levels <- reactive({
  switch(IRT_bock_parametrization(),
    blis = {
      fit <- fit_blis(IRT_bock_data(), IRT_bock_key(), SE = TRUE)
      orig_levels <- fit@orig_levels # extract already constructed orig_levels
      list(fit = fit, orig_levels = orig_levels)
    },
    thissen = {
      # convert to integer and store orig_levels
      d_int_plus_key <- nominal_to_int(
        IRT_bock_data(), IRT_bock_key()
      )
      pars <- obtain_nrm_def(d_int_plus_key)

      fit <- mirt(
        d_int_plus_key[["Data"]], 1,
        itemtype = "nominal", SE = TRUE, pars = pars, verbose = FALSE
      )
      orig_levels <- d_int_plus_key[["orig_levels"]]

      list(fit = fit, orig_levels = orig_levels)
    }
  )
}) %>% # cache on data and collapsed parametrization
  bindCache(IRT_bock_data(), IRT_bock_parametrization()) %>% # possibly also key??
  bindEvent(IRT_bock_data(), IRT_bock_parametrization()) # invalidate only at nrm_mod() change


# NOTE orig_levels is the same for the particular data,
# but we compute it twice (at most) here; not so expensive, though


# COEFF. TABLE ------------------------------------------------------------

IRT_bock_summary_coef <- reactive({
  is_irt <- input$IRT_bock_parametrization %in% c("blirt", "bock")

  coefs <- IRT_bock_fit_and_orig_levels()[["fit"]] %>%
    coef(printSE = TRUE, IRTpars = is_irt, labels = TRUE, mark_correct = FALSE)

  # get rid of non-item info
  coefs[["GroupPars"]] <- NULL

  coefs <- map2(
    coefs, IRT_bock_fit_and_orig_levels()[["orig_levels"]],
    ~ {
      colnames(.x) <- c(
        # if thissen param, paste overall slope on top
        if (input$IRT_bock_parametrization == "thissen") "a*_",
        paste0("a_", .y), paste0("b_", .y) # use "b" parname
      )
      .x
    }
  )

  # get item with maximum number of parameters
  max_parnum_item <- coefs %>%
    map(length) %>%
    which.max()
  # set master names to determine the order of columns below
  master_parnames <- colnames(coefs[[max_parnum_item]])

  # turn into tibble with par/SE in "zig-zag" pattern
  coefs <- coefs %>%
    map_dfr(~ as_tibble(.x, rownames = "type"), .id = "item") %>%
    relocate(item, type, all_of(master_parnames)) %>% # order columns
    pivot_wider(item,
      names_from = type, values_from = c(-item, -type),
      names_glue = "{if_else(type == 'par', '', type)}_{.value}" # if par, use empty string
    )

  # turn to data.frame with rownames
  coefs <- coefs %>% column_to_rownames("item")

  cf_nms <- colnames(coefs)

  # if slope/intercept, replace a and d with betas
  if (!is_irt) {
    cf_nms <- cf_nms %>%
      str_replace("a(?!\\*)", "\\\\beta_1") %>% # don't do anything with Thissen a*
      str_replace("b_", "\\\\beta_0_")
  }

  colnames(coefs) <- cf_nms %>%
    str_split("_", n = 3) %>% # max to 3 if data levels use "_"
    map_chr(
      ~ paste0(
        .x[1L],
        if (.x[1L] == "SE") "(", # if SE, add "("
        "\\(\\mathit{", .x[2L], # parameter
        "_{", .x[3L], "}}\\)", # index
        if (.x[1L] == "SE") ")" # if SE, enclose with ")"
      )
    )

  coefs
}) %>% # cache on raw parametrization user input and data
  bindCache(IRT_bock_data(), input$IRT_bock_parametrization) %>% # possibly also key??
  bindEvent(IRT_bock_data(), input$IRT_bock_parametrization)




# IIC plot for summary ---------------------------------------------------------

# ** Plot of IIC ####
IRT_bock_summary_iic <- reactive({
  fit <- IRT_bock_fit_and_orig_levels()[["fit"]]

  # names from model
  mod_item_names <- fit@Data$data %>% colnames()

  d <- map2_dfr(
    mod_item_names,
    item_names(), # names from user
    ~ tibble(
      Ability = IRT_thetas_for_plots(), # vector only
      Information = iteminfo(extract.item(fit, .x), IRT_thetas_for_plots()),
      Item = .y,
    )
  )

  # plotly labels
  d <- d %>% mutate(label = paste0(
    "Ability = ", round(Ability, 3), "\n",
    "Information = ", round(Information, 3), "\n",
    "Item = ", Item
  ))

  d %>% ggplot(aes(
    x = Ability, y = Information,
    color = Item, group = Item, text = label
  )) +
    geom_line() +
    theme_app()
}) %>%
  bindCache(IRT_bock_fit_and_orig_levels()) %>%
  bindEvent(IRT_bock_fit_and_orig_levels())

# cache also plotly conversion
IRT_bock_summary_iic_plotly <- reactive({
  g <- IRT_bock_summary_iic()
  p <- ggplotly(g, tooltip = "text")

  p$elementId <- NULL
  p %>% plotly::config(displayModeBar = FALSE)
}) %>%
  bindCache(IRT_bock_fit_and_orig_levels()) %>%
  bindEvent(IRT_bock_fit_and_orig_levels())

output$IRT_bock_summary_iic <- renderPlotly({
  IRT_bock_summary_iic_plotly()
})

# ** Download plot of IIC ####
output$IRT_bock_summary_iic_download <- downloadHandler(
  filename = function() {
    "fig_IRT_bock_IIC.png"
  },
  content = function(file) {
    ggsave(file,
      plot = IRT_bock_summary_iic() +
        theme(
          text = element_text(size = setting_figures$text_size),
          legend.position = "right", legend.key.size = unit(0.8, "lines")
        ),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Plot of TIC ####
IRT_bock_summary_tic <- reactive({
  fit <- IRT_bock_fit_and_orig_levels()[["fit"]]
  thetas <- IRT_thetas_for_plots()

  test_info_se <- tibble(Ability = thetas, Information = testinfo(fit, thetas), SE = 1 / sqrt(Information))

  ggplot(test_info_se, aes(x = Ability)) +
    geom_line(aes(y = Information, col = "info")) +
    geom_line(aes(y = SE, col = "se")) +
    scale_color_manual(values = c("blue", "pink"), labels = c("Information", "SE")) +
    scale_y_continuous("Information", sec.axis = sec_axis(~., name = "SE")) +
    theme(axis.title.y = element_text(color = "pink")) +
    theme_app()
}) %>%
  bindCache(IRT_bock_fit_and_orig_levels()) %>%
  bindEvent(IRT_bock_fit_and_orig_levels())


IRT_bock_summary_tic_plotly <- reactive({
  g <- IRT_bock_summary_tic()
  p <- ggplotly(g)

  p$x$data[[1]]$text <- gsub("<br />colour: info", "", p$x$data[[1]]$text)
  p$x$data[[2]]$text <- gsub("<br />colour: se", "", p$x$data[[2]]$text)

  p$elementId <- NULL
  p %>% plotly::config(displayModeBar = FALSE)
}) %>%
  bindCache(IRT_bock_fit_and_orig_levels()) %>%
  bindEvent(IRT_bock_fit_and_orig_levels())

output$IRT_bock_summary_tic <- renderPlotly({
  IRT_bock_summary_tic_plotly()
})

# ** Download plot of TIC ####
output$IRT_bock_summary_tic_download <- downloadHandler(
  filename = function() {
    "fig_IRT_bock_TIC.png"
  },
  content = function(file) {
    ggsave(file,
      plot = IRT_bock_summary_tic() +
        theme(
          text = element_text(size = setting_figures$text_size),
          legend.position = "right", legend.key.size = unit(0.8, "lines")
        ),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Table of parameters ####
output$IRT_bock_summary_coef <- renderTable(
  IRT_bock_summary_coef(),
  rownames = TRUE, striped = TRUE, na = ""
)

# ** Download of coef tab ####
output$IRT_bock_summary_coef_download <- downloadHandler(
  filename = function() {
    "IRT_bock_coefs.csv"
  },
  content = function(file) {
    tab <- IRT_bock_summary_coef()

    # remove math
    names(tab) <- names(tab) %>%
      str_remove_all("\\\\\\(\\\\mathit\\{\\\\") %>%
      str_remove_all("\\\\\\)") %>%
      str_remove_all("[{}}]")

    write.csv(tab, file)
  }
)


# ability estimates with scores an zscores --------------------------------

IRT_bock_summary_fscores_zscores <- reactive({
  fit <- IRT_bock_fit_and_orig_levels()[["fit"]]
  fscore_with_ses <- fscores(fit, full.scores.SE = TRUE)
  colnames(fscore_with_ses) <- c("F-score", "SE(F-score)")

  tab <- data.frame(
    `Total score` = total_score(),
    `Z-score` = z_score(),
    fscore_with_ses,
    check.names = F
  )

  rownames(tab) <- paste("Respondent", 1L:nrow(tab))
  tab
}) %>%
  bindCache(IRT_bock_fit_and_orig_levels()) %>%
  bindEvent(IRT_bock_fit_and_orig_levels())

# intermediate reactive to be used in a table, corr. text and plot
IRT_bock_summary_ability <- reactive({
  IRT_bock_summary_fscores_zscores()
}) %>%
  bindCache(IRT_bock_fit_and_orig_levels()) %>%
  bindEvent(IRT_bock_fit_and_orig_levels())

# output table
output$IRT_bock_summary_ability <- renderTable(
  {
    factors <- IRT_bock_summary_ability()
    head(factors, n = 6)
  },
  rownames = TRUE
)

# download button
output$IRT_bock_summary_ability_download <- downloadHandler(
  filename = function() {
    "IRT_bock_abilities.csv"
  },
  content = function(file) {
    write.csv(IRT_bock_summary_ability(), file)
  }
)

# z-score f-score corr. estimate
IRT_bock_summary_ability_correlation <- reactive({
  tab <- IRT_bock_summary_fscores_zscores()

  cor(tab[["F-score"]], tab[["Z-score"]], use = "pairwise.complete.obs")
}) %>%
  bindCache(IRT_bock_fit_and_orig_levels()) %>%
  bindEvent(IRT_bock_fit_and_orig_levels())

# output corr. text with the estimate
output$IRT_bock_summary_ability_correlation_text <- renderText({
  paste0(
    "This scatterplot shows the relationship between the standardized total
         score (Z-score) and the factor score estimated by the IRT model. The
         Pearson correlation coefficient between these two scores is ",
    sprintf("%.3f", IRT_bock_summary_ability_correlation()), ". "
  )
})

# z-score f-score scatterplot
IRT_bock_summary_ability_plot <- reactive({
  df <- IRT_bock_summary_fscores_zscores()

  ggplot(df, aes(`Z-score`, `F-score`)) +
    geom_point(size = 3) +
    labs(x = "Standardized total score", y = "Factor score") +
    theme_app()
}) %>%
  bindCache(IRT_bock_fit_and_orig_levels()) %>%
  bindEvent(IRT_bock_fit_and_orig_levels())

# intermediate plotly object (that can be cached, ggplot -> plotly is expensive)
IRT_bock_summary_ability_plotly <- reactive({
  g <- IRT_bock_summary_ability_plot()
  p <- ggplotly(g)

  p$x$data[[1]]$text <- gsub("zscore", "Z-score", p$x$data[[1]]$text)
  p$x$data[[1]]$text <- gsub("fscore", "F-score", p$x$data[[1]]$text)

  p$elementId <- NULL
  p %>% plotly::config(displayModeBar = FALSE)
}) %>%
  bindCache(IRT_bock_fit_and_orig_levels()) %>%
  bindEvent(IRT_bock_fit_and_orig_levels())

# output plotly
output$IRT_bock_summary_ability_plot <- renderPlotly({
  IRT_bock_summary_ability_plotly()
})

# *** Download factor scores plot ####
output$IRT_bock_summary_ability_plot_download <- downloadHandler(
  filename = function() {
    "fig_IRT_bock_abilities.png"
  },
  content = function(file) {
    ggsave(file,
      plot = IRT_bock_summary_ability_plot() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)


# ITEMS -------------------------------------------------------------------

# update Item slider to accommodate number of items of the dataset used
observe({
  item_count <- ncol(IRT_bock_data())
  updateSliderInput(
    session = session,
    inputId = "IRT_bock_items",
    max = item_count
  )
})


# ** Plot of ICC for selected item ####
IRT_bock_items_icc <- reactive({
  item <- input$IRT_bock_items

  fit <- IRT_bock_fit_and_orig_levels()[["fit"]]
  orig_levels <- IRT_bock_fit_and_orig_levels()[["orig_levels"]][[item]]
  item_key <- orig_levels[attr(orig_levels, "key", exact = TRUE)]

  thetas <- IRT_thetas_for_plots()

  probs <- as_tibble(probtrace(extract.item(fit, item), thetas))

  names(probs) <- orig_levels

  probs <- probs %>%
    bind_cols(theta = thetas) %>%
    pivot_longer(-theta, names_to = "key", values_to = "probs") %>%
    mutate(
      correct = key == item_key,
      label = paste0(
        "Ability = ", round(theta, 3), "\n",
        "Probability = ", round(probs, 3), "\n",
        "Response = ", key, if_else(correct, " (correct)", " (distractor)")
      )
    )

  probs %>%
    ggplot(aes(theta, probs, col = key, linetype = correct, group = key)) +
    geom_line(aes(text = label)) +
    labs(
      x = "Ability", y = "Probability of correct answer",
      title = item_names()[item], col = "Resp.", linetype = "Corr. resp."
    ) +
    scale_linetype_manual(values = c(`FALSE` = "dashed", `TRUE` = "solid")) +
    coord_cartesian(ylim = c(0, 1)) +
    theme_app()
})

output$IRT_bock_items_icc <- renderPlotly({
  g <- IRT_bock_items_icc()
  p <- ggplotly(g, tooltip = "text")

  p$elementId <- NULL
  p %>% plotly::config(displayModeBar = FALSE)
})

# ** Download plot of ICC for selected item ####
output$IRT_bock_items_icc_download <- downloadHandler(
  filename = function() {
    item <- input$IRT_bock_items
    paste0("fig_IRT_bock_ICC, ", item_names()[item], ".png")
  },
  content = function(file) {
    ggsave(file,
      plot = IRT_bock_items_icc() +
        theme(
          text = element_text(size = setting_figures$text_size),
          legend.position = "right", legend.key.size = unit(0.8, "lines")
        ),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Plot of IIC for selected item ####
IRT_bock_items_iic <- reactive({
  item <- input$IRT_bock_items
  fit <- IRT_bock_fit_and_orig_levels()[["fit"]]
  thetas <- IRT_thetas_for_plots()

  infos <- tibble(
    Ability = thetas,
    Information = iteminfo(extract.item(fit, item), thetas)
  )

  infos %>% ggplot(aes(Ability, Information)) +
    geom_line() +
    ggtitle(item_names()[item]) +
    theme_app()
})

output$IRT_bock_items_iic <- renderPlotly({
  g <- IRT_bock_items_iic()
  p <- ggplotly(g)

  p$elementId <- NULL
  p %>% plotly::config(displayModeBar = FALSE)
})

# ** Download plot of IIC for selected item ####
output$IRT_bock_items_iic_download <- downloadHandler(
  filename = function() {
    item <- input$IRT_bock_items
    paste0("fig_IRT_bock_IIC_", item_names()[item], ".png")
  },
  content = function(file) {
    ggsave(file,
      plot = IRT_bock_items_iic() +
        theme(
          text = element_text(size = setting_figures$text_size),
          legend.position = "right", legend.key.size = unit(0.8, "lines")
        ),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Table of parameters for selected item ####
IRT_bock_items_coef <- reactive({
  item <- input$IRT_bock_items
  IRT_bock_summary_coef()[item, ]
})

output$IRT_bock_items_coef <- renderTable(
  {
    IRT_bock_items_coef()
  },
  rownames = FALSE
)
