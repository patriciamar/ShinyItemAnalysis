#' @importFrom difR sibTest
#' @export
.difSIBTEST_edited <- function(Data, group, focal.name, type = "udif", anchor = NULL,
                               alpha = 0.05, purify = FALSE, nrIter = 10, p.adjust.method = NULL,
                               puriadjType = "simple",
                               save.output = FALSE, output = c("out", "default")) {
  internalSIBTEST <- function() {
    if (length(group) == 1) {
      if (is.numeric(group)) {
        gr <- Data[, group]
        DATA <- Data[, (1:ncol(Data)) != group]
        colnames(DATA) <- colnames(Data)[(1:ncol(Data)) != group]
      }
      else {
        gr <- Data[, colnames(Data) == group]
        DATA <- Data[, colnames(Data) != group]
        colnames(DATA) <- colnames(Data)[colnames(Data) != group]
      }
    }
    else {
      gr <- group
      DATA <- Data
    }
    Group <- as.numeric(gr == focal.name)

    df <- data.frame(DATA, Group, check.names = F)
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
    DATA <- as.data.frame(df[, !(colnames(df) %in% c("Group"))])
    colnames(DATA) <- colnames(df)[!(colnames(df) %in% c("Group"))]

    if (is.null(anchor)) {
      ANCHOR <- 1:ncol(DATA)
      anchor.names <- NULL
    }
    else {
      if (is.numeric(anchor)) {
        ANCHOR <- anchor
        anchor.names <- anchor
      }
      else {
        ANCHOR <- which(colnames(DATA) %in% anchor)
        anchor.names <- anchor
      }
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

    if (!purify | !is.null(anchor)) {
      PROV <- sibTest(DATA, Group, type = type, anchor = ANCHOR)
      PVAL <- PROV$p.value
      P.ADJUST <- p.adjust(PVAL, method = adj.method)
      if (min(P.ADJUST, na.rm = TRUE) >= alpha) {
        DIFitems <- "No DIF item detected"
      } else {
        DIFitems <- which(!is.na(PVAL) & P.ADJUST < alpha)
      }

      if (is.null(p.adjust.method)) {
        adjusted.p <- NULL
      } else {
        adjusted.p <- P.ADJUST
      }

      RES <- list(
        Beta = PROV$Beta, SE = PROV$SE, X2 = PROV$X2,
        df = PROV$df, p.value = PROV$p.value, type = type,
        alpha = alpha, DIFitems = DIFitems, p.adjust.method = p.adjust.method,
        adjusted.p = adjusted.p, purification = purify, names = colnames(DATA),
        anchor.names = anchor.names, save.output = save.output,
        output = output
      )

      if (!is.null(anchor)) {
        RES$Beta[ANCHOR] <- NA
        RES$SE[ANCHOR] <- NA
        RES$X2[ANCHOR] <- NA
        RES$df[ANCHOR] <- NA
        RES$p.value[ANCHOR] <- NA
        RES$DIFitems <- RES$DIFitems[!RES$DIFitems %in% ANCHOR]
      }
    } else {
      nrPur <- 0
      difPur <- NULL
      noLoop <- FALSE
      prov1 <- sibTest(DATA, Group, type = type)
      pval1 <- prov1$p.value
      p.adjust1 <- p.adjust(pval1, method = puri.adj.method)
      if (min(p.adjust1, na.rm = TRUE) >= alpha) {
        DIFitems <- "No DIF item detected"
        noLoop <- TRUE
      } else {
        dif <- which(!is.na(pval1) & p.adjust1 < alpha)
        difPur <- rep(0, length(pval1))
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
            prov2 <- sibTest(DATA, Group,
              type = type,
              anchor = nodif
            )
            pval2 <- prov2$p.value
            p.adjust2 <- p.adjust(pval2, method = puri.adj.method)

            if (min(p.adjust2, na.rm = TRUE) >= alpha) {
              dif2 <- NULL
            } else {
              dif2 <- which(!is.na(pval2) & p.adjust2 < alpha)
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
        pval1 <- pval2
        p.adjust1 <- p.adjust(pval1, method = adj.method)
        prov1 <- prov2
        if (min(p.adjust1, na.rm = TRUE) >= alpha) {
          DIFitems <- "No DIF item detected"
        } else {
          DIFitems <- which(!is.na(pval1) & p.adjust1 < alpha)
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
        Beta = prov1$Beta, SE = prov1$SE, X2 = prov1$X2,
        df = prov1$df, p.value = pval1, type = type,
        alpha = alpha, DIFitems = DIFitems, p.adjust.method = p.adjust.method,
        adjusted.p = adjusted.p, puriadjType = puriadjType, purification = purify, nrPur = nrPur,
        difPur = difPur, convergence = noLoop, names = colnames(DATA),
        anchor.names = NULL, save.output = save.output,
        output = output
      )
    }
    class(RES) <- "SIBTEST"
    return(RES)
  }
  resToReturn <- internalSIBTEST()
  if (save.output) {
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
