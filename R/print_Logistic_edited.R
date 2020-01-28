#' @export
print.Logistic <- function(x, ...) {
  res <- x
  cat("\n")
  mess1 <- switch(res$type, both = " both types of ",
    nudif = " nonuniform ", udif = " uniform "
  )
  cat("Detection of", mess1, "Differential Item Functioning",
    "\n", "using Logistic regression method, ",
    sep = ""
  )
  if (res$purification & is.null(res$anchor.names) & res$match %in%
    c("score", "zscore")) {
    pur <- "with "
  } else {
    pur <- "without "
  }
  cat(pur, "item purification", "\n", sep = "")
  cat("and with ", res$criterion, " DIF statistic",
    "\n", "\n",
    sep = ""
  )
  if (res$purification & is.null(res$anchor.names) & res$match %in%
    c("score", "zscore")) {
    if (res$nrPur <= 1) {
      word <- " iteration"
    } else {
      word <- " iterations"
    }
    if (!res$convergence) {
      cat("WARNING: no item purification convergence after ",
        res$nrPur, word, "\n",
        sep = ""
      )
      loop <- NULL
      for (i in 1:res$nrPur) loop[i] <- sum(res$difPur[1, ] == res$difPur[i + 1, ])
      if (max(loop) != length(res$Logistik)) {
        cat("(Note: no loop detected in less than ",
          res$nrPur, word, ")", "\n",
          sep = ""
        )
      } else {
        cat("(Note: loop of length ", min((1:res$nrPur)[loop ==
          length(res$Logistik)]), " in the item purification process)",
        "\n",
        sep = ""
        )
      }
      cat(
        "WARNING: following results based on the last iteration of the purification",
        "\n", "\n"
      )
    }
    else {
      cat("Convergence reached after ", res$nrPur,
        word, "\n", "\n",
        sep = ""
      )
    }
  }
  if (res$match[1] == "score") {
    cat(
      "Matching variable: test score", "\n",
      "\n"
    )
  } else if (res$match[1] == "zscore") {
    cat(
      "Matching variable: standardized test score", "\n",
      "\n"
    )
  } else {
    cat(
      "Matching variable: specified matching variable",
      "\n", "\n"
    )
  }
  if (is.null(res$anchor.names) | !(res$match %in% c("score", "zscore"))) {
    itk <- 1:length(res$Logistik)
    cat(
      "No set of anchor items was provided", "\n",
      "\n"
    )
  } else {
    itk <- (1:length(res$Logistik))[!is.na(res$Logistik)]
    cat("Anchor items (provided by the user):", "\n")
    if (is.numeric(res$anchor.names)) {
      mm <- res$names[res$anchor.names]
    } else {
      mm <- res$anchor.names
    }
    mm <- cbind(mm)
    rownames(mm) <- rep("", nrow(mm))
    colnames(mm) <- ""
    print(mm, quote = FALSE)
    cat("\n", "\n")
  }
  if (is.null(res$p.adjust.method)) {
    cat(
      "No p-value adjustment for multiple comparisons",
      "\n", "\n"
    )
  } else {
    pAdjMeth <- switch(res$p.adjust.method, bonferroni = "Bonferroni",
      holm = "Holm", hochberg = "Hochberg",
      hommel = "Hommel", BH = "Benjamini-Hochberg",
      BY = "Benjamini-Yekutieli"
    )
    cat(
      "Multiple comparisons made with", pAdjMeth,
      "adjustement of p-values", "\n", "\n"
    )
  }
  cat(
    "Logistic regression DIF statistic:", "\n",
    "\n"
  )
  df <- switch(res$type, both = 2, udif = 1, nudif = 1)
  pval <- round(1 - pchisq(res$Logistik, df), 4)
  if (is.null(res$p.adjust.method)) {
    symb <- symnum(pval, c(0, 0.001, 0.01, 0.05, 0.1, 1),
      symbols = c(
        "***", "**", "*", ".",
        ""
      )
    )
  } else {
    symb <- symnum(res$adjusted.p, c(
      0, 0.001, 0.01, 0.05,
      0.1, 1
    ), symbols = c(
      "***", "**", "*",
      ".", ""
    ))
  }
  m1 <- cbind(round(res$Logistik[itk], 4), pval[itk])
  if (!is.null(res$p.adjust.method)) {
    m1 <- cbind(m1, round(res$adjusted.p[itk], 4))
  }
  m1 <- noquote(cbind(
    format(m1, justify = "right"),
    symb[itk]
  ))
  if (!is.null(res$names)) {
    rownames(m1) <- res$names[itk]
  } else {
    rn <- NULL
    for (i in 1:nrow(m1)) {
      rn[i] <- paste("Item", i,
        sep = ""
      )
    }
    rownames(m1) <- rn[itk]
  }
  con <- c("Stat.", "P-value")
  if (!is.null(res$p.adjust.method)) {
    con <- c(con, "Adj. P")
  }
  con <- c(con, "")
  colnames(m1) <- con
  print(m1)
  cat("\n")
  cat(
    "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 ",
    "\n"
  )
  cat("\n", "Detection threshold: ", round(
    res$thr,
    4
  ), " (significance level: ", res$alpha, ")",
  "\n", "\n",
  sep = ""
  )
  if (is.character(res$DIFitems)) {
    cat(
      "Items detected as DIF items:", res$DIFitems,
      "\n", "\n"
    )
  } else {
    mess2 <- switch(res$type, both = " ", nudif = " nonuniform ",
      udif = " uniform "
    )
    cat("Items detected as", mess2, "DIF items:",
      "\n",
      sep = ""
    )
    if (!is.null(res$names)) {
      m2 <- res$names
    } else {
      rn <- NULL
      for (i in 1:length(res$Logistik)) {
        rn[i] <- paste("Item",
          i,
          sep = ""
        )
      }
      m2 <- rn
    }
    m2 <- cbind(m2[res$DIFitems])
    rownames(m2) <- rep("", nrow(m2))
    colnames(m2) <- ""
    print(m2, quote = FALSE)
    cat("\n", "\n")
  }
  cat(
    "Effect size (Nagelkerke's R^2):", "\n",
    "\n"
  )
  cat("Effect size code:", "\n")
  cat(" 'A': negligible effect", "\n")
  cat(" 'B': moderate effect", "\n")
  cat(" 'C': large effect", "\n", "\n")
  r2 <- round(res$deltaR2, 4)
  symb1 <- symnum(r2, c(0, 0.13, 0.26, 1),
    symbols = c("A", "B", "C")
  )
  symb2 <- symnum(r2, c(0, 0.035, 0.07, 1),
    symbols = c("A", "B", "C")
  )
  matR2 <- noquote(cbind(
    format(r2[itk], justify = "right"),
    symb1[itk], symb2[itk]
  ))
  if (!is.null(res$names)) {
    rownames(matR2) <- res$names[itk]
  } else {
    rn <- NULL
    for (i in 1:length(r2)) {
      rn[i] <- paste("Item",
        i,
        sep = ""
      )
    }
    rownames(matR2) <- rn[itk]
  }
  colnames(matR2) <- c("R^2", "ZT", "JG")
  print(matR2)
  cat("\n")
  cat("Effect size codes:", "\n")
  cat(
    " Zumbo & Thomas (ZT): 0 'A' 0.13 'B' 0.26 'C' 1",
    "\n"
  )
  cat(
    " Jodoin & Gierl (JG): 0 'A' 0.035 'B' 0.07 'C' 1",
    "\n"
  )
  if (!x$save.output) {
    cat("\n", "Output was not captured!", "\n")
  } else {
    if (x$output[2] == "default") {
      wd <- paste(getwd(), "/", sep = "")
    } else {
      wd <- x$output[2]
    }
    fileName <- paste(wd, x$output[1], ".txt", sep = "")
    cat("\n", "Output was captured and saved into file",
      "\n", " '", fileName, "'", "\n",
      "\n",
      sep = ""
    )
  }
}
