uiSetting <-
  tabPanel("",
    value = "settings",
    icon = icon("gear"),
    h3("Settings"),
    h4("IRT models setting"),
    p("Set the number of iterations (EM cycles) for IRT models in the ", strong("IRT models"), "section."),
    fluidPage(column(2, numericInput(
      inputId = "ncycles",
      label = "Number of iterations",
      value = 500L, # the default for EM estim. in mirt
      min = 10L,
      max = 5000L # tried to set a reasonable upper limit for non-local sessions
    ))),
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
    ),
    h4("Modules"),
    p("You can add newly installed modules without restarting the app."),
    fluidRow(
      column(
        2,
        actionButton("rediscover_mods", "Rediscover modules", icon = icon("rotate"))
      )
    )
  )
