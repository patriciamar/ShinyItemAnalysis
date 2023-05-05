uiModules <- navbarMenu("Modules",
  menuName = "Modules",
  # DESCRIPTION ####
  "Description",
  tabPanel(
    title = "About the modules",
    value = "how_to",
    h3("About the modules"),
    p(
      "ShinyItemAnalysis modules (SIA modules) are designed to integrate with the
      ShinyItemAnalysis interactive app. They can access and utilize any analysis output or even",
      "the raw data for their own analyses or various interactive demonstrations.",
      "Because SIA modules come in ", code("R"), " packages (or extend the existing ones),",
      "they may come bundled with their own datasets, use compiled code, etc."
    ),
    p(
      "Several SIA modules are already provided for you in the, ", code("SIAmodules"),
      "package, which is installed as you run this app for the first time."
    )
  )
)
