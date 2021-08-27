uiSetting <-
  tabPanel("",
    value = "settings",
    icon = icon("fas fa-cog"),
    h3("Settings"),
    h4("IRT models setting"),
    p("Set the number of cycles for IRT models in the ", strong("IRT models"), "section."),
    fluidPage(column(2, numericInput(
      inputId = "ncycles",
      label = "Number of cycles",
      value = 2000,
      min = 1,
      max = 999999
    ))),

    h4("Range-restricted reliability settings"),
    p("Set the number of bootstrap samples for the confidence interval calculation in the ", strong("Reliability / Restricted range"), "section."),
    fluidPage(column(
      2,
      numericInput(
        inputId = "reliability_restricted_bootsamples",
        label = "Bootstrap samples",
        value = 10,
        min = 3,
        max = 1000
      )
    )),

    h4("Figure downloads"),
    p("Here you can change setting for download of figures. "),
    fluidPage(
      column(2, numericInput(
        inputId = "setting_figures_text_size",
        label = "Text size [pts]",
        value = 12,
        min = 6,
        max = 20
      )),
      column(2, numericInput(
        inputId = "setting_figures_height",
        label = "Height [in]",
        value = 4,
        min = 1,
        max = 16
      )),
      column(2, numericInput(
        inputId = "setting_figures_width",
        label = "Width [in]",
        value = 8,
        min = 1,
        max = 16
      )),
      column(2, numericInput(
        inputId = "setting_figures_dpi",
        label = "Plot resolution",
        value = 600,
        min = 72,
        max = 600
      ))
    )
  )
