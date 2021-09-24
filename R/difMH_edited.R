#' @importFrom difR mantelHaenszel
#' @export
.difMH_edited <- function(Data, group, focal.name, anchor = NULL, match = "score",
                          MHstat = "MHChisq", correct = TRUE, exact = FALSE,
                          alpha = 0.05, purify = FALSE, nrIter = 10, p.adjust.method = NULL,
                          puriadjType = "simple",
                          save.output = FALSE, output = c("out", "default")) {
  if (purify & match[1] != "score") {
    stop("purification not allowed when matching variable is not 'score'",
      call. = FALSE
    )
  }

  internalMH <- function() {
    if (length(group) == 1) {
      if (is.numeric(group)) {
        gr <- Data[, group]
        DATA <- Data[, (1:ncol(Data)) != group]
        colnames(DATA) <- colnames(Data)[(1:ncol(Data)) != group]
      } else {
        gr <- Data[, colnames(Data) == group]
        DATA <- Data[, colnames(Data) != group]
        colnames(DATA) <- colnames(Data)[colnames(Data) != group]
      }
    } else {
      gr <- group
      DATA <- Data
    }

    Group <- as.numeric(gr == focal.name)

    if (length(match) == dim(DATA)[1]) {
      df <- data.frame(DATA, Group, match, check.names = F)
    } else {
      df <- data.frame(DATA, Group, check.names = F)
    }
    if (any(is.na(DATA))) {
      warning("'Data' contains missing values. Observations with missing values are discarded.",
        call. = FALSE
      )
    }
    if (any(is.na(Group))) {
      warning("'group' contains missing values. Observations with missing values are discarded.",
        call. = FALSE
      )
    }
    df <- df[complete.cases(df), ]
    Group <- df[, "Group"]
    DATA <- as.data.frame(df[, !(colnames(df) %in% c("Group", "match"))])
    colnames(DATA) <- colnames(df)[!(colnames(df) %in% c("Group", "match"))]
    if (length(match) > 1) {
      if (any(is.na(match))) {
        warning("'match' contains missing values. Observations with missing values are discarded.",
          call. = FALSE
        )
      }
      match <- df[, "match"]
    }
    Q <- switch(MHstat,
      MHChisq = qchisq(1 - alpha, 1),
      logOR = qnorm(1 - alpha / 2)
    )

    if (is.null(Q)) {
      stop("'MHstat' argument not valid", call. = FALSE)
    }
    if (!is.null(anchor)) {
      dif.anchor <- anchor
      if (is.numeric(anchor)) {
        ANCHOR <- anchor
      } else {
        ANCHOR <- which(colnames(DATA) %in% anchor)
      }
    } else {
      ANCHOR <- 1:ncol(DATA)
      dif.anchor <- NULL
    }

    if (purify) {
      if (is.null(p.adjust.method)) {
        puri.adj.method <- "none"
        adj.method <- "none"
      } else {
        if (puriadjType == "simple") {
          puri.adj.method <- "none"
          adj.method <- p.adjust.method
        } else {
          puri.adj.method <- p.adjust.method
          adj.method <- p.adjust.method
        }
      }
    } else {
      adj.method <- ifelse(is.null(p.adjust.method), "none", p.adjust.method)
    }

    if (exact) {
      if (!purify | match[1] != "score" | !is.null(anchor)) {
        PROV <- mantelHaenszel(DATA, Group,
          match = match,
          correct = correct, exact = exact, anchor = ANCHOR
        )
        STATS <- PROV$resMH
        PVAL <- PROV$Pval
        P.ADJUST <- p.adjust(PVAL, method = adj.method)

        if (min(P.ADJUST, na.rm = T) >= alpha) {
          DIFitems <- "No DIF item detected"
        } else {
          DIFitems <- which(P.ADJUST < alpha)
        }

        if (is.null(p.adjust.method)) {
          adjusted.p <- NULL
        } else {
          adjusted.p <- P.ADJUST
        }

        RES <- list(
          MH = STATS, p.value = PROV$Pval,
          alpha = alpha, DIFitems = DIFitems, correct = correct,
          exact = exact, match = PROV$match, p.adjust.method = p.adjust.method,
          adjusted.p = adjusted.p, purification = purify, names = colnames(DATA),
          anchor.names = dif.anchor, save.output = save.output,
          output = output
        )

        if (!is.null(anchor)) {
          RES$MH[ANCHOR] <- NA
          RES$Pval[ANCHOR] <- NA
          RES$DIFitems <- RES$DIFitems[!RES$DIFitems %in% ANCHOR]
        }
      } else {
        nrPur <- 0
        difPur <- NULL
        noLoop <- FALSE
        prov1 <- mantelHaenszel(DATA, Group,
          match = match,
          correct = correct, exact = exact
        )
        stats1 <- prov1$resMH
        pval1 <- prov1$Pval
        p.adjust1 <- p.adjust(pval1, method = puri.adj.method)
        if (min(p.adjust1, na.rm = T) >= alpha) {
          DIFitems <- "No DIF item detected"
          noLoop <- TRUE
        } else {
          dif <- which(pval1 < alpha)
          difPur <- rep(0, length(stats1))
          difPur[dif] <- 1
          repeat {
            if (nrPur >= nrIter) {
              break
            } else {
              nrPur <- nrPur + 1
              nodif <- NULL
              if (is.null(dif)) {
                nodif <- 1:ncol(DATA)
              } else {
                nodif <- which(!1:ncol(DATA) %in% dif)
              }
              prov2 <- mantelHaenszel(DATA, Group,
                correct = correct,
                match = match, anchor = nodif, exact = exact
              )
              stats2 <- prov2$resMH
              pval2 <- prov2$Pval
              p.adjust2 <- p.adjust(pval2, method = puri.adj.method)
              if (min(p.adjust2, na.rm = T) >= alpha) {
                dif2 <- NULL
              } else {
                dif2 <- which(pval2 < alpha)
              }
              difPur <- rbind(difPur, rep(0, ncol(DATA)))
              difPur[nrPur + 1, dif2] <- 1
              if (length(dif) != length(dif2)) {
                dif <- dif2
              } else {
                dif <- sort(dif)
                dif2 <- sort(dif2)
                if (sum(dif == dif2) == length(dif)) {
                  noLoop <- TRUE
                  break
                }
                else {
                  dif <- dif2
                }
              }
            }
          }
          stats1 <- stats2
          prov1 <- prov2
          pval1 <- pval2
          p.adjust1 <- p.adjust(pval1, method = adj.method)
          if (min(p.adjust1, na.rm = T) >= alpha) {
            DIFitems <- "No DIF item detected"
          } else {
            DIFitems <- which(!is.na(stats1) & p.adjust1 < alpha)
          }
        }
        if (!is.null(difPur)) {
          rownames(difPur) <- paste0("Step", 1:nrow(difPur) - 1)
          colnames(difPur) <- colnames(DATA)
        }

        if (is.null(p.adjust.method)) {
          adjusted.p <- NULL
        } else {
          adjusted.p <- p.adjust1
        }

        RES <- list(
          MH = stats1, p.value = prov1$Pval,
          alpha = alpha, DIFitems = DIFitems, correct = correct,
          exact = exact, match = prov1$match, p.adjust.method = p.adjust.method,
          adjusted.p = adjusted.p,
          puriadjType = puriadjType,
          purification = purify, nrPur = nrPur,
          difPur = difPur, puri.adj.method = puri.adj.method, puriadjType = puriadjType,
          convergence = noLoop, names = colnames(DATA),
          anchor.names = NULL, save.output = save.output,
          output = output
        )
      }
    } else {
      if (!purify | match[1] != "score" | !is.null(anchor)) {
        PROV <- mantelHaenszel(DATA, Group,
          match = match,
          correct = correct, exact = exact, anchor = ANCHOR
        )
        if (MHstat == "MHChisq") {
          STATS <- PROV$resMH
          PVAL <- 1 - pchisq(STATS, 1)
        } else {
          STATS <- log(PROV$resAlpha) / sqrt(PROV$varLambda)
          PVAL <- 2 * (1 - pnorm(abs(STATS)))
        }
        P.ADJUST <- p.adjust(PVAL, method = adj.method)
        if (min(P.ADJUST, na.rm = T) >= alpha) {
          DIFitems <- "No DIF item detected"
        } else {
          DIFitems <- which(!is.na(STATS) & P.ADJUST < alpha)
        }

        if (is.null(p.adjust.method)) {
          adjusted.p <- NULL
        } else {
          adjusted.p <- P.ADJUST
        }

        RES <- list(
          MH = STATS, p.value = PVAL, alphaMH = PROV$resAlpha,
          varLambda = PROV$varLambda, MHstat = MHstat,
          alpha = alpha, thr = Q, DIFitems = DIFitems,
          correct = correct, exact = exact, match = PROV$match,
          p.adjust.method = p.adjust.method, adjusted.p = adjusted.p,
          purification = purify, names = colnames(DATA),
          anchor.names = dif.anchor, save.output = save.output,
          output = output
        )

        if (!is.null(anchor)) {
          RES$MH[ANCHOR] <- NA
          RES$alphaMH[ANCHOR] <- NA
          RES$varLambda[ANCHOR] <- NA
          RES$DIFitems <- RES$DIFitems[!RES$DIFitems %in% ANCHOR]
        }
      } else {
        nrPur <- 0
        difPur <- NULL
        noLoop <- FALSE
        prov1 <- mantelHaenszel(DATA, Group,
          match = match,
          correct = correct, exact = exact
        )

        if (MHstat == "MHChisq") {
          stats1 <- prov1$resMH
          pval1 <- 1 - pchisq(stats1, 1)
        } else {
          stats1 <- log(prov1$resAlpha) / sqrt(prov1$varLambda)
          pval1 <- 2 * (1 - pnorm(abs(stats1)))
        }
        p.adjust1 <- p.adjust(pval1, method = puri.adj.method)

        if (min(p.adjust1, na.rm = T) >= alpha) {
          DIFitems <- "No DIF item detected"
          noLoop <- TRUE
        } else {
          dif <- which(!is.na(stats1) & p.adjust1 < alpha)
          difPur <- rep(0, length(stats1))
          difPur[dif] <- 1
          repeat {
            if (nrPur >= nrIter) {
              break
            } else {
              nrPur <- nrPur + 1
              nodif <- NULL
              if (is.null(dif)) {
                nodif <- 1:ncol(DATA)
              } else {
                nodif <- which(!1:ncol(DATA) %in% dif)
              }

              prov2 <- mantelHaenszel(DATA, Group,
                match = match,
                correct = correct, anchor = nodif, exact = exact
              )
              if (MHstat == "MHChisq") {
                stats2 <- prov2$resMH
                pval2 <- 1 - pchisq(stats2, 1)
              } else {
                stats2 <- log(prov2$resAlpha) / sqrt(prov2$varLambda)
                pval2 <- 2 * (1 - pnorm(abs(stats2)))
              }
              p.adjust2 <- p.adjust(pval2, method = puri.adj.method)

              if (min(p.adjust2, na.rm = T) >= alpha) {
                dif2 <- NULL
              } else {
                dif2 <- which(!is.na(stats2) & p.adjust2 < alpha)
              }

              difPur <- rbind(difPur, rep(0, ncol(DATA)))
              difPur[nrPur + 1, dif2] <- 1
              if (length(dif) != length(dif2)) {
                dif <- dif2
              } else {
                dif <- sort(dif)
                dif2 <- sort(dif2)
                if (sum(dif == dif2) == length(dif)) {
                  noLoop <- TRUE
                  break
                }
                else {
                  dif <- dif2
                }
              }
            }
          }
          stats1 <- stats2
          prov1 <- prov2
          pval1 <- pval2
          p.adjust1 <- p.adjust(pval1, method = adj.method)
          if (min(p.adjust1, na.rm = T) >= alpha) {
            DIFitems <- "No DIF item detected"
          } else {
            DIFitems <- which(!is.na(stats1) & p.adjust1 < alpha)
          }
        }
        if (!is.null(difPur)) {
          rownames(difPur) <- paste0("Step", 1:nrow(difPur) - 1)
          colnames(difPur) <- colnames(DATA)
        }

        if (is.null(p.adjust.method)) {
          adjusted.p <- NULL
        } else {
          adjusted.p <- p.adjust1
        }


        RES <- list(
          MH = stats1, p.value = pval1, alphaMH = prov1$resAlpha,
          varLambda = prov1$varLambda, MHstat = MHstat,
          alpha = alpha, thr = Q, DIFitems = DIFitems,
          correct = correct, exact = exact, match = prov1$match,
          p.adjust.method = p.adjust.method,
          adjusted.p = adjusted.p, puriadjType = puriadjType,
          purification = purify, nrPur = nrPur,
          difPur = difPur,
          convergence = noLoop, names = colnames(DATA),
          anchor.names = NULL, save.output = save.output,
          output = output
        )
      }
    }
    class(RES) <- "MH"
    return(RES)
  }

  resToReturn <- internalMH()
  if (save.output == TRUE) {
    if (output[2] == "default") {
      wd <- paste(getwd(), "/", sep = "")
    } else {
      wd <- output[2]
    }
    fileName <- paste(wd, output[1], ".txt", sep = "")
    capture.output(resToReturn, file = fileName)
  }
  return(resToReturn)
}
