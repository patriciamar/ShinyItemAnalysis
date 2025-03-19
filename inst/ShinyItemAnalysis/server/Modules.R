# discover installed modules ----------------------------------------------

if (!ShinyItemAnalysis:::sm_disabled()) {
  # keep track of loaded and appended mods in particular session
  # (so every user gets modules tabs appended in his/her instance)
  # we manipulate this object with add_modules()
  mod_list <- reactiveValues(
    # available mods are picked on every add_modules call by find_modules()
    to_load = character(),
    loaded = character()
  )

  # keep track of the categories with modules
  used_categories <- reactiveVal()

  # list of reactive objects (not evaluated calls!) to expose to modules
  # maybe in the future we should make a curated list beside these raw reactives

  # find every single reactive in shiny's server environment and return it as a list
  mod_server_exports <- rlang::current_env() |>
    as.list() |>
    purrr::keep(inherits, c("reactive", "reactiveVal", "reactivevalues"))
  # note that input object inherits "reactivevalues" so it is readily available for
  # the modules as imports$input

  # mark this object so we can have some trace in the modules
  attr(mod_server_exports, "imported_by_sia") <- TRUE

  # on mod_list change (and on btn click),
  # do the module discovery and load procedure
  observe({
    add_modules(
      mod_list,
      server_dots = mod_server_exports,
      ui_dots = list() # no use for now, maybe completely useless?
    )
  }) |>
    bindEvent(mod_list, input$rediscover_mods)


  if (sm_allow_gui_installation()) {
  # obtain available module packages from SIA repo
  # available.packages are cached per R session
  observe({
    available <- ShinyItemAnalysis:::sm_not_installed()

    if (length(available) == 0) {
      available <- c("All available modules were installed" = "")
    }
    updateSelectizeInput(
      inputId = "mods_in_repo",
      choices = available
    )
  })

  observe({
    sel_mod <- input$mods_in_repo
    req(sel_mod)

    # validate input to be safe
    pkgs_on_repo <- available.packages(
      repos = ShinyItemAnalysis:::sm_repo(),
      fields = "Config/ShinyItemAnalysis/module"
    )

    is_sm <- !is.na(pkgs_on_repo[, "Config/ShinyItemAnalysis/module"])

    mods_on_repo <- pkgs_on_repo[is_sm, "Package"]

    if (!(sel_mod %in% mods_on_repo)) {
      showModal(
        modalDialog(
          title = "Module installation failed",
          p(
            "The module '", span(sel_mod, .noWS = c("before", "after")),
            "' is not available on the repository at '", span(sm_repo(), .noWS = c("before", "after")),
            "'. Please report the issue to the authors."
          ),
          easyClose = TRUE,
          size = "m"
        )
      )
      return()
    }

    # if (sel_mod %in% ShinyItemAnalysis:::pkgs_attached()) {
    #   showModal(
    #     modalDialog(
    #       title = "Selected SIA module is in use",
    #       "Please close the app and restart your R session.",
    #       easyClose = TRUE,
    #       size = "s"
    #     )
    #   )
    # }

    install_cond_handler <- function(cnd) {
      showModal(
        modalDialog(
          title = "Module installation failed",
          p(
            "The module installation failed with the following message.",
            "Please install the module manually in R console and report the error to the authors."
          ),
          p(cnd$message, style = "color:red;"),
          easyClose = TRUE,
          size = "m"
        )
      )
    }

    rlang::try_fetch(
      {
        ShinyItemAnalysis:::sm_install_pkg(sel_mod)
      },
      warning = install_cond_handler,
      error = install_cond_handler
    )

    add_modules(
      mod_list,
      server_dots = mod_server_exports,
      ui_dots = list() # no use for now, maybe completely useless?
    )

    available <- ShinyItemAnalysis:::sm_not_installed()

    if (length(available) == 0L) {
      available <- c("All available modules were installed" = "")
    }
    updateSelectizeInput(
      inputId = "mods_in_repo",
      choices = available
    )
  }) |>
    bindEvent(input$install_mod)
  }
}


# functions ---------------------------------------------------------------


find_modules <- function(...,
                         desc_field = "Config/ShinyItemAnalysis/module",
                         field_value = "true") {
  # this approach may be somewhat slow when the library is big,
  # but gets cached for the field for the R session

  if (are_modules_debugged()) {
    message("Looking for installed packages claiming they contain SIAmodule(s)...")
  }

  mod_pkgs <- names(
    which(
      utils::installed.packages(fields = desc_field, ...)[, desc_field] == field_value
    )
  )


  # list of modules we want to allow
  modallow_path <- file.path(getwd(), ".modallow")

  # by default, allow all modules
  to_allow <- mod_pkgs

  # only if .modallow exists, read it and intersect with mod_pkgs
  if (file.exists(modallow_path)) {
    to_allow <- readLines(modallow_path)
  }

  intersect(mod_pkgs, to_allow)
}

# library call usually raises an R CMD Check warning if the package is given as a symbol,
# with character.only, it seems just OK
# this implementation solves the issues stemming from using different/nonstandard libraries
load_and_attach <- function(pkg) {
  loc <- if (pkg %in% loadedNamespaces()) dirname(getNamespaceInfo(pkg, "path"))
  suppressPackageStartupMessages(
    library(pkg, lib.loc = loc, character.only = TRUE, warn.conflicts = FALSE)
  )
  asNamespace(pkg)
}

#' Load and append modules
#' @importFrom purrr walk
#'
add_modules <- function(mod_list, server_dots = NULL, ui_dots = NULL) {
  # list and store all available modules in a permanent environment
  available <- find_modules()
  mod_debug_msg("available", available)

  # what modules to load (those available that are not already loaded)
  mod_list$to_load <- setdiff(available, mod_list$loaded)
  mod_debug_msg("to_load", mod_list$to_load)

  # if there is nothing to load, exit
  if (length(mod_list$to_load) == 0L) {
    return(NULL)
  }

  # load modules to be loaded
  mod_list$to_load |>
    walk(
      load_and_append_mod,
      server_dots = server_dots, ui_dots = ui_dots
    )

  # track loaded modules - for local use, this has to be session-based,
  # but the deployed app would have benefit from some caching
  # however appendTab() and possibly modules functions calls are not shared between the sessions??
  # maybe we have to "pay" for this functionality with repeated add_module() calls for each user
  mod_list$loaded <- append(mod_list$loaded, mod_list$to_load)
  mod_debug_msg("loaded", mod_list$loaded)
}


#' Load and append module in Shiny app
#'
load_and_append_mod <- function(mod_pkg, server_dots = NULL, ui_dots = NULL) {
  # library() the pkg
  ns <- load_and_attach(mod_pkg)

  # read YAML with module(s) info, function bindings etc.
  mod_yaml <- system.file("sia/modules.yml", package = mod_pkg) |>
    yaml::read_yaml()

  apply_mods <- function(mod_desc, mod_name) {
    # check if category is applicable
    # if unknown category, put it in Modules
    if (!mod_desc$category %in% .sia_mod_categories) {
      mod_desc$category <- "Modules"
    }

    # unique module identifier
    mod_id <- paste0(mod_pkg, "_", mod_name)

    # call server function of the module
    mod_debug_msg("call_server", mod_name)

    do.call(ns[[mod_desc$binding$server]], list(id = mod_id, server_dots)) # positional arg matching, so the module's arg name does not matter


    # create module section in the very end of the tab's dropdown list
    # but only for the first time per category!!
    # trivial approach would be to create hidden delimiters and unhide them with every
    # module addition, but only tabPanels are allowed for navbarMenu, not mere tags...
    # we have to insert the delimiter on-the-go and test for its presence on subsequent module additions
    # checking for the delimiter presence is near to impossible with JS as it requires
    # immediate two-way communication through the websocket (I have tried...)
    if (!mod_desc$category %in% used_categories()) {
      mod_debug_msg("insert_delimiter", mod_desc$category)

      insertUI(
        # taken from shiny.js -- `shiny-insert-tab`
        selector = paste0(
          "a.dropdown-toggle[data-value='", mod_desc$category, "'] + ul.dropdown-menu"
        ),
        # after the last child of the selector element, so we don't have to li:last
        where = "beforeEnd",
        # class names are from shiny source
        tagList(
          tags$li(class = "divider"),
          tags$li(class = "dropdown-header", "Modules")
        ),
        # do not let the module's tab take precedence (but that did not happen in testing)
        immediate = TRUE
      )
    }

    # update used categories to prevent another delimiter insertion
    used_categories(append(used_categories(), mod_desc$category))

    # append UI part as a tab
    mod_debug_msg("call_ui", mod_name, mod_desc$category)
    appendTab(
      inputId = "navbar",
      menuName = mod_desc$category,
      tab = tabPanel(
        title = mod_desc$title, value = mod_id,
        # UI is constructed with the function call, ui_dots are not used ATM
        do.call(ns[[mod_desc$binding$ui]], list(id = mod_id, ui_dots))
      )
    )
  }

  # call each module's function and append their tab
  # enclose in try() so to handle corrupted YAML and other exceptions
  mod_yaml |> iwalk(~ try(apply_mods(.x, .y)))
}

# available categories for modules must match those of SIAtools::list_categories()
# except Modules
.sia_mod_categories <- c(
  "Scores", "Validity", "Reliability", "Item analysis",
  "Regression", "IRT models", "DIF/Fairness"
)

#' Check if to print module debugging messages
#'
are_modules_debugged <- function() {
  as.logical(Sys.getenv("SIA_MODULES_DEBUG", unset = "FALSE"))
}

#' Issue a module debugging message
#'
mod_debug_msg <- function(operation, mods, menuName = NULL) {
  if (!are_modules_debugged()) {
    return(invisible())
  }
  switch(operation,
    available = message("Available: ", paste(mods, collapse = ", ")),
    to_load = {
      if (length(mods) != 0) {
        message("To load: ", paste(mods, collapse = ", "))
      } else {
        message("All available modules are already loaded, exiting...")
      }
    },
    loaded = message("Successfully loaded: ", paste(mods, collapse = ", ")),
    call_server = message("Calling ", mods, "'s server function..."),
    call_ui =
      message(
        "Calling ", mods, "'s UI function and appending its tab to ",
        menuName, "..."
      ),
    load = {
      if (!isNamespaceLoaded(mods)) {
        message("Loading ", mods, "'s namespace")
      } else {
        message("Using ", mods, "'s already loaded namespace")
      }
    },
    attach = message("Attaching ", mods, "'s namespace to search() path"),
    insert_delimiter = message("Inserting delimiter to ", mods, " tab.")
  )
}
