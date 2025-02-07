tab_title <- if (!sm_allow_gui_installation() | ShinyItemAnalysis:::sm_disabled()) {
  "About the modules"
} else {
  "About / Module management"
}

uiModules <- navbarMenu("Modules",
  menuName = "Modules",
  tabPanel(
    title = tab_title,
    value = "how_to",
    h3("About the modules"),
    p(
      "ShinyItemAnalysis modules (SIA modules) are designed to integrate with the
      ShinyItemAnalysis interactive app. They can access and utilize any analysis output or even",
      "the raw data for their own analyses or various interactive demonstrations.",
      "Because SIA modules come in ", code("R"), " packages (or extend the existing ones),",
      "they may come bundled with their own datasets, use compiled code, etc."
    ),
    if (sm_allow_gui_installation() & !ShinyItemAnalysis:::sm_disabled()) {
      tagList(
        h3("Install modules"),
        p(
          "Here you can install the packages equipped with SIA modules that you haven't yet installed.",
          "By default, the app consults the",
          a(
            href = "https://shinyitemanalysis.org/repo", "official SIA repository",
            target = "_blank"
          ),
          "for available modules.",
          strong("If the installation halts, please check your R console for any error messages or prompts."),
          "To update the packages with SIA modules, proceed as usual in your R console.",
          "If you are interested in developing your own SIA module, please refer to the",
          a(
            href = "https://applstat.github.io/SIAtools/", span(code("{SIAtools}"), "R package"),
            target = "_blank", .noWS = "after"
          ),
          "."
        ),
        fluidRow(
          column(
            3,
            selectizeInput("mods_in_repo",
              label = NULL,
              choices = c("Initializing the list..." = ""),
              width = "100%"
            )
          ),
          column(
            2,
            actionButton("install_mod", "Install", width = "100%")
          )
        )
      )
    }
  )
)
