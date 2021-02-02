
# parse R code & insert it in defined block in app ------------------------

insert_block <- function(value, path, block_start, block_end, replace = TRUE) {
  if (!is.null(path) && file.exists(path)) {
    lines <- readLines(path)
    start <- which(grepl(block_start, lines))
    end <- which(grepl(block_end, lines))

    indent <- "" #regmatches(lines[[start]], regexpr("^\\s*", lines[[start]]))

    if (length(start) == 0 && length(end) == 0) {
      stop("Block start and/or end not found in file.")
    }

    if (!(length(start) == 1 && length(end) == 1 && start < end)) {
      stop("Invalid block specification.")
    }

    start <- start + 1L
    end <- end - 1L
  } else {
    stop("File not found or not stated.")
  }

  if (replace) {
    block <- NULL
  } else {
    block <- lines[rlang::seq2(start, end)]
  }

  value <- gsub("\"", replacement = "\\\\\"", value) # escape all quotes
  value[[1]] <- paste0("\"", value[[1]])
  value[[length(value)]] <- paste0(value[[length(value)]], "\"")

  # mimic indent of bock_start
  value <- paste0(indent, value)

  lines <- c(
    lines[rlang::seq2(1, start - 1L)],
    block, value, # the code itself
    lines[rlang::seq2(end + 1L, length(lines))]
  )

  con <- file(path, open = "wb", encoding = "utf-8")
  on.exit(close(con))

  # warning, only windows line-endings supported
  base::writeLines(enc2utf8(lines), con, sep = "\n", useBytes = TRUE)
}


# usage -------------------------------------------------------------------

# get lines
txt <- readLines("sampleRcode/code_reliability-restricted-range.R")

# replace the content of block in specified file
insert_block(txt,
  path = "inst/shiny-examples/ShinyItemAnalysis/ui/uiReliability.R",
  block_start = "## code_reliability_restricted-range: start ##",
  block_end = "## code_reliability_restricted-range: end ##"
)
