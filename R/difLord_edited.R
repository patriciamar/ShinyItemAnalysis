#' @importFrom difR itemParEst itemRescale LordChi2
#'
#' @export
.difLord_edited <- function(Data, group, focal.name, model, c = NULL, engine = "ltm",
                            discr = 1, irtParam = NULL, same.scale = TRUE, anchor = NULL,
                            alpha = 0.05, purify = FALSE, nrIter = 10,
                            p.adjust.method = NULL, puriadjType = "simple",
                            save.output = FALSE, output = c("out", "default")) {
  internalLord <- function() {
    if (!is.null(irtParam)) {
      nrItems <- nrow(irtParam) / 2
      m0 <- irtParam[1:nrItems, ]
      m1 <- irtParam[(nrItems + 1):(2 * nrItems), ]
      dataName <- rownames(irtParam[1:nrItems, ])
      if (!is.null(anchor) & !same.scale) {
        dif.anchor <- anchor
        if (is.numeric(anchor)) {
          ANCHOR <- anchor
        } else {
          ANCHOR <- NULL
          for (i in 1:length(anchor)) {
            ANCHOR[i] <- (1:length(dataName))[dataName == anchor[i]]
          }
        }
      } else {
        ANCHOR <- 1:nrItems
        dif.anchor <- NULL
      }
      if (same.scale) {
        m1p <- m1
      } else {
        m1p <- itemRescale(m0, m1, items = ANCHOR)
      }
      mod <- as.character(ncol(irtParam))
      model <- switch(mod,
        `2` = "1PL", `5` = "2PL",
        `6` = "3PL", `9` = "3PL"
      )
      DF <- switch(mod,
        `2` = 1, `5` = 2,
        `6` = 2, `9` = 3
      )
      if (ncol(irtParam) != 6) {
        Guess <- NULL
      } else {
        Guess <- irtParam[1:nrItems, 6]
        if (length(unique(round(Guess, 4))) == 1) {
          Guess <- unique(round(Guess, 4))
        }
      }
      if (is.null(Guess)) {
        Q <- switch(model,
          `1PL` = qchisq(1 - alpha, 1),
          `2PL` = qchisq(1 - alpha, 2),
          `3PL` = qchisq(1 - alpha, 3)
        )
      } else {
        Q <- qchisq(1 - alpha, 2)
      }
      itemParInit <- irtParam
      estPar <- FALSE
    } else {
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
      if (any(is.na(Group))) {
        warning("'group' contains missing values. Observations with missing values are discarded.",
          call. = FALSE
        )
      }
      DATA <- DATA[!is.na(Group), ]
      Group <- Group[!is.na(Group)]

      d0 <- sapply(DATA[Group == 0, ], function(x) as.numeric(paste(x)))
      d1 <- sapply(DATA[Group == 1, ], function(x) as.numeric(paste(x)))

      Guess <- c
      if (is.null(Guess)) {
        Q <- switch(model,
          `1PL` = qchisq(1 - alpha, 1),
          `2PL` = qchisq(1 - alpha, 2),
          `3PL` = qchisq(1 - alpha, 3)
        )
        DF <- switch(model, `1PL` = 1,
          `2PL` = 2, `3PL` = 3
        )
        m0 <- switch(model,
          `1PL` = itemParEst(d0, model = "1PL", engine = engine, discr = discr),
          `2PL` = itemParEst(d0, model = "2PL"),
          `3PL` = itemParEst(d0, model = "3PL")
        )
        m1 <- switch(model,
          `1PL` = itemParEst(d1, model = "1PL", engine = engine, discr = discr),
          `2PL` = itemParEst(d1, model = "2PL"),
          `3PL` = itemParEst(d1, model = "3PL")
        )
      } else {
        Q <- qchisq(1 - alpha, 2)
        DF <- 2
        m0 <- itemParEst(d0, model = "3PL", c = Guess)
        m1 <- itemParEst(d1, model = "3PL", c = Guess)
      }
      nrItems <- ncol(DATA)
      if (!is.null(anchor)) {
        dif.anchor <- anchor
        if (is.numeric(anchor)) {
          ANCHOR <- anchor
        } else {
          ANCHOR <- NULL
          for (i in 1:length(anchor)) {
            ANCHOR[i] <- (1:ncol(DATA))[colnames(DATA) == anchor[i]]
          }
        }
      } else {
        ANCHOR <- 1:nrItems
        dif.anchor <- NULL
      }
      m1p <- itemRescale(m0, m1, items = ANCHOR)
      irtParam <- rbind(m0, m1p)
      same.scale <- TRUE
      dataName <- colnames(DATA)
      itemParInit <- rbind(m0, m1)
      estPar <- TRUE
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
      STATS <- LordChi2(m0, m1p)
      PVAL <- 1 - pchisq(STATS, DF)
      P.ADJUST <- p.adjust(PVAL, method = adj.method)

      if (is.null(Guess)) {
        m_null <- switch(model,
          `1PL` = itemParEst(DATA, model = "1PL", engine = engine, discr = discr),
          `2PL` = itemParEst(DATA, model = "2PL"),
          `3PL` = itemParEst(DATA, model = "3PL")
        )
      } else {
        m_null <- itemParEst(DATA, model = "3PL", c = Guess)
      }

      if (min(P.ADJUST, na.rm = TRUE) >= alpha) {
        DIFitems <- "No DIF item detected"
        # itemParBest <- rbind(m_null, m_null)
      } else {
        DIFitems <- which(P.ADJUST < alpha)
        nodif <- which(!1:nrItems %in% DIFitems)
        # if (estPar) {
        #   itemParBest <- itemParInit
        #   itemParBest[nodif, ] <- m_null[nodif, ]
        #   itemParBest[nodif + nrItems, ] <- m_null[nodif, ]
        # }
      }
      if (is.null(p.adjust.method)) {
        adjusted.p <- NULL
      } else {
        adjusted.p <- P.ADJUST
      }

      RES <- list(
        LordChi = STATS, p.value = PVAL, alpha = alpha,
        thr = Q, DIFitems = DIFitems, purification = purify,
        model = model, c = Guess, engine = engine, discr = discr,
        p.adjust.method = p.adjust.method, adjusted.p = adjusted.p,
        itemParInit = itemParInit, # itemParBest = itemParBest,
        estPar = estPar,
        names = dataName, anchor.names = dif.anchor, save.output = save.output,
        output = output
      )
      if (!is.null(anchor) & (RES$estPar | (!RES$estPar &
        !same.scale))) {
        RES$LordChi[ANCHOR] <- NA
        for (i in 1:length(RES$DIFitems)) {
          if (sum(RES$DIFitems[i] == ANCHOR) == 1) {
            RES$DIFitems[i] <- NA
          }
        }
        RES$DIFitems <- RES$DIFitems[!is.na(RES$DIFitems)]
      }
    } else {
      nrPur <- 0
      difPur <- NULL
      noLoop <- FALSE
      stats1 <- LordChi2(m0, m1p)
      pval1 <- 1 - pchisq(stats1, DF)
      p.adjust1 <- p.adjust(pval1, method = puri.adj.method)

      if (min(p.adjust1, na.rm = T) >= alpha) {
        DIFitems <- "No DIF item detected"
        noLoop <- TRUE
        # if (is.null(Guess)) {
        #   m_null <- switch(model,
        #     `1PL` = itemParEst(DATA, model = "1PL", engine = engine, discr = discr),
        #     `2PL` = itemParEst(DATA, model = "2PL"),
        #     `3PL` = itemParEst(DATA, model = "3PL")
        #   )
        # } else {
        #   m_null <- itemParEst(DATA, model = "3PL", c = Guess)
        # }
        # itemParBest <- rbind(m_null, m_null)
        itemParFinal <- rbind(m0, m1p)
        if (is.null(p.adjust.method)) {
          adjusted.p <- NULL
        } else {
          adjusted.p <- p.adjust1
        }

        RES <- list(
          LordChi = stats1, p.value = pval1,
          alpha = alpha, thr = Q, DIFitems = DIFitems,
          purification = purify, nrPur = nrPur, difPur = difPur,
          convergence = noLoop, model = model, c = Guess,
          engine = engine, discr = discr, p.adjust.method = p.adjust.method,
          adjusted.p = adjusted.p, itemParInit = itemParInit,
          itemParFinal = itemParFinal, # itemParBest = itemParBest,
          estPar = estPar,
          names = dataName, anchor.names = NULL, save.output = save.output,
          output = output
        )
      } else {
        dif <- which(p.adjust1 < alpha)
        difPur <- rep(0, length(stats1))
        difPur[dif] <- 1
        repeat {
          if (nrPur >= nrIter) {
            itemParFinal <- rbind(
              m0,
              itemRescale(m0, m1, items = nodif)
            )
            break
          } else {
            nrPur <- nrPur + 1
            nodif <- NULL
            if (is.null(dif)) {
              nodif <- 1:nrItems
            } else {
              nodif <- which(!1:nrItems %in% dif)
            }
            stats2 <- LordChi2(
              m0,
              itemRescale(m0, m1, items = nodif)
            )
            pval2 <- 1 - pchisq(stats2, DF)
            p.adjust2 <- p.adjust(pval2, method = puri.adj.method)
            if (min(p.adjust2, na.rm = TRUE) >= alpha) {
              dif2 <- NULL
            } else {
              dif2 <- which(p.adjust2 < alpha)
            }
            difPur <- rbind(difPur, rep(0, nrItems))
            difPur[nrPur + 1, dif2] <- 1
            dif <- sort(dif)
            dif2 <- sort(dif2)

            if (length(dif) != length(dif2)) {
              dif <- dif2
            } else {
              if (all(dif == dif2)) {
                noLoop <- TRUE
                itemParFinal <- rbind(
                  m0,
                  itemRescale(m0, m1, items = nodif)
                )
                break
              } else {
                dif <- dif2
              }
            }
          }
        }
        if (!is.null(difPur)) {
          rownames(difPur) <- paste0("Step", 1:nrow(difPur) - 1)
          colnames(difPur) <- colnames(DATA)
        }
        stats1 <- stats2
        pval1 <- 1 - pchisq(stats1, DF)
        p.adjust1 <- p.adjust(pval1, method = adj.method)

        # if (is.null(Guess)) {
        #   m_null <- switch(model,
        #     `1PL` = itemParEst(DATA, model = "1PL", engine = engine, discr = discr),
        #     `2PL` = itemParEst(DATA, model = "2PL"),
        #     `3PL` = itemParEst(DATA, model = "3PL")
        #   )
        # } else {
        #   m_null <- itemParEst(DATA, model = "3PL", c = Guess)
        # }

        if (min(p.adjust1, na.rm = T) >= alpha) {
          DIFitems <- "No DIF item detected"
          # itemParBest <- rbind(m_null, m_null)
        } else {
          DIFitems <- which(p.adjust1 < alpha)
          nodif <- which(!1:nrItems %in% DIFitems)
          # if (estPar) {
          #   itemParBest <- itemParInit
          #   itemParBest[nodif, ] <- m_null[nodif, ]
          #   itemParBest[nodif + nrItems, ] <- m_null[nodif, ]
          # }
        }
        if (is.null(p.adjust.method)) {
          adjusted.p <- NULL
        } else {
          adjusted.p <- p.adjust1
        }
        RES <- list(
          LordChi = stats1, p.value = pval1,
          alpha = alpha, thr = Q, DIFitems = DIFitems, purification = purify,
          nrPur = nrPur, difPur = difPur, convergence = noLoop,
          model = model, c = Guess, engine = engine,
          discr = discr, p.adjust.method = p.adjust.method,
          adjusted.p = adjusted.p, itemParInit = itemParInit,
          itemParFinal = itemParFinal, # itemParBest = itemParBest,
          estPar = estPar,
          names = dataName, anchor.names = NULL, save.output = save.output,
          output = output
        )
      }
    }
    class(RES) <- "Lord"
    return(RES)
  }
  resToReturn <- internalLord()
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
