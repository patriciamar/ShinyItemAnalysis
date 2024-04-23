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
  mod_server_exports <- rlang::current_env() %>%
    as.list() %>%
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
  }) %>%
    bindEvent(mod_list, input$rediscover_mods)
}


# functions ---------------------------------------------------------------


#' Find SIA Modules in a Library
#'
#' Discover packages that declares themselves as a SIA addin module with
#' `SIAmodule: true` entry in their `DESCRIPTION`. By default, the function
#' looks for all known library trees (see [.libPaths()]). If the function do not
#' discover anything (and it should), please refer to the `Note` below.
#'
#' @param desc_field *character*, name of the field to look for. Defaults to
#'   "`Config/ShinyItemAnalysis/module`".
#' @param field_value *character*, value to detect. Defaults to "`true`".
#' @inheritDotParams utils::installed.packages -fields
#'
#' @return *character vector* with package names.
#'
#' @note When `{ShinyItemAnalysis}` was installed inside `{renv}` project, provide
#'   `lib.loc = renv::paths$library()` as an argument.
#'
#' @seealso [installed.packages()]
#' @keywords internal
#'
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

 # read list of modules we want to ignore
 modignore_path <- file.path(getwd(), ".modignore")

 to_ignore <- character()

 if (file.exists(modignore_path)) {
   to_ignore <- readLines(modignore_path)
 }

 # return only those not on blacklist
 setdiff(mod_pkgs, to_ignore)

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
#'
#' @param mod_list  mutable reactiveValues?
#' @param server_dots  todo
#' @param ui_dots  todo
#'
#' @return called for side effects
#'
#' @importFrom purrr walk
#' @keywords internal
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
  mod_list$to_load %>%
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
#' @param mod todo
#' @param server_dots todo
#' @param ui_dots todo
#'
#' @return todo
#' @keywords internal
load_and_append_mod <- function(mod_pkg, server_dots = NULL, ui_dots = NULL) {
  # library() the pkg
  ns <- load_and_attach(mod_pkg)

  # read YAML with module(s) info, function bindings etc.
  mod_yaml <- system.file("sia/modules.yml", package = mod_pkg) %>%
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
  mod_yaml %>% iwalk(~ try(apply_mods(.x, .y)))
}

# available categories for modules must match those of SIAtools::list_categories()
# except Modules
.sia_mod_categories <- c(
  "Scores", "Validity", "Reliability", "Item analysis",
  "Regression", "IRT models", "DIF/Fairness"
)

#' Check if to print module debugging messages
#'
#' @param default todo
#'
#' @return todo
#' @keywords internal
are_modules_debugged <- function() {
  as.logical(Sys.getenv("SIA_MODULES_DEBUG", unset = "FALSE"))
}

#' Issue a module debugging message
#'
#' @param operation todo
#' @param mods todo
#' @param menuName todo
#'
#' @return todo
#' @keywords internal
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
