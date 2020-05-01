#' @export
.difLogistic_edited <- function(Data, group, focal.name, anchor = NULL, member.type = "group",
                                match = "score", type = "both", criterion = "LRT",
                                alpha = 0.05, all.cov = FALSE, purify = FALSE, nrIter = 10,
                                p.adjust.method = NULL, puriadjType = "simple",
                                save.output = FALSE, output = c("out", "default")) {
  if (member.type != "group" & member.type != "cont") {
    stop("'member.type' must be either 'group' or 'cont'",
      call. = FALSE
    )
  }
  if (purify & !(match[1] %in% c("score", "zscore"))) {
    stop("purification not allowed when matching variable is not 'score' or 'zscore'.",
      call. = FALSE
    )
  }

  internalLog <- function() {
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
    if (member.type == "group") {
      Group <- as.numeric(gr == focal.name)
    } else {
      Group <- gr
    }
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

    Q <- switch(type,
      both = qchisq(1 - alpha, 2),
      udif = qchisq(1 - alpha, 1),
      nudif = qchisq(1 - alpha, 1)
    )

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

    DDF <- ifelse(type == "both", 2, 1)

    if (!purify | !match[1] %in% c("score", "zscore") | !is.null(anchor)) {
      PROV <- .Logistik_edited(DATA, Group,
        member.type = member.type,
        match = match, type = type, criterion = criterion, # there goes "zscore" string
        anchor = ANCHOR, all.cov = all.cov
      )
      STATS <- PROV$stat
      PVAL <- 1 - pchisq(STATS, DDF)
      P.ADJUST <- p.adjust(PVAL, method = adj.method)
      deltaR2 <- PROV$deltaR2

      logitPar <- PROV$parM1
      logitSe <- PROV$seM1

      if (min(P.ADJUST, na.rm = T) >= alpha) {
        DIFitems <- "No DIF item detected"
      } else {
        DIFitems <- which(P.ADJUST < alpha)
        logitPar[DIFitems, ] <- PROV$parM0[DIFitems, ]
        logitSe[DIFitems, ] <- PROV$seM0[DIFitems, ]
      }

      if (is.null(p.adjust.method)) {
        adjusted.p <- NULL
      } else {
        adjusted.p <- P.ADJUST
      }
      RES <- list(
        Logistik = STATS, p.value = PVAL, logitPar = logitPar,
        logitSe = logitSe, parM0 = PROV$parM0, seM0 = PROV$seM0,
        cov.M0 = PROV$cov.M0, cov.M1 = PROV$cov.M1, deltaR2 = deltaR2,
        alpha = alpha, thr = Q, DIFitems = DIFitems,
        member.type = member.type, match = PROV$match, # from PROV, string
        type = type, p.adjust.method = p.adjust.method,
        adjusted.p = adjusted.p,
        purification = purify, names = colnames(DATA),
        anchor.names = dif.anchor, criterion = criterion,
        save.output = save.output, output = output,
        Data = DATA, group = Group
      )
      if (!is.null(anchor) & match[1] == "score") { # match is "score"
        RES$Logistik[ANCHOR] <- NA
        RES$logitPar[ANCHOR, ] <- NA
        RES$parM0[ANCHOR, ] <- NA
        RES$deltaR2[ANCHOR] <- NA
        RES$DIFitems <- RES$DIFitems[!RES$DIFitems %in% ANCHOR]
      }
    } else { # match is zscore or specified
      nrPur <- 0
      difPur <- NULL
      noLoop <- FALSE
      prov1 <- .Logistik_edited(DATA, Group, # the same as PROV
        member.type = member.type,
        match = match, type = type, criterion = criterion, # match as string "score"
        all.cov = all.cov
      )
      stats1 <- prov1$stat
      pval1 <- 1 - pchisq(stats1, DDF)
      p.adjust1 <- p.adjust(pval1, method = puri.adj.method)
      deltaR2 <- prov1$deltaR2
      if (min(p.adjust1, na.rm = T) >= alpha) {
        DIFitems <- "No DIF item detected"
        logitPar <- prov1$parM1
        logitSe <- prov1$seM1
        noLoop <- TRUE
      } else {
        dif <- which(p.adjust1 < alpha)
        difPur <- rep(0, length(stats1))
        difPur[dif] <- 1 # mark DIF items as 1
        repeat {
          if (nrPur >= nrIter) {
            break
          } else {
            nrPur <- nrPur + 1
            if (is.null(dif)) {
              nodif <- 1:ncol(DATA)
            } else {
              nodif <- which(!1:ncol(DATA) %in% dif)
            }
            prov2 <- .Logistik_edited(DATA, Group,
              anchor = nodif,
              member.type = member.type, match = match, # pur iter match - wants vector?
              type = type, criterion = criterion, all.cov = all.cov
            )
            stats2 <- prov2$stat
            pval2 <- 1 - pchisq(stats2, DDF)
            p.adjust2 <- p.adjust(pval2, method = puri.adj.method)
            deltaR2 <- prov2$deltaR2
            if (min(p.adjust2, na.rm = T) >= alpha) {
              dif2 <- NULL
            } else {
              dif2 <- which(p.adjust2 < alpha)
            }
            difPur <- rbind(difPur, rep(0, ncol(DATA)))
            difPur[nrPur + 1, dif2] <- 1
            dif <- sort(dif)
            dif2 <- sort(dif2)

            if (length(dif) != length(dif2)) {
              dif <- dif2
            } else {
              if (all(dif == dif2)) {
                noLoop <- TRUE
                break
              } else {
                dif <- dif2
              }
            }
          }
        }
        prov1 <- prov2
        stats1 <- stats2
        pval1 <- 1 - pchisq(stats1, DDF)
        p.adjust1 <- p.adjust(pval1, method = adj.method)
        deltaR2 <- deltaR2
        logitPar <- prov1$parM1
        logitSe <- prov1$seM1

        if (min(p.adjust1, na.rm = T) >= alpha) {
          DIFitems <- "No DIF item detected"
        } else {
          DIFitems <- which(!is.na(stats1) & p.adjust1 < alpha)
          logitPar[DIFitems, ] <- prov1$parM0[DIFitems, ]
          logitSe[DIFitems, ] <- prov1$seM0[DIFitems, ]
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
        Logistik = stats1, p.value = pval1, logitPar = logitPar,
        logitSe = logitSe, parM0 = prov1$parM0, seM0 = prov1$seM0,
        cov.M0 = prov1$cov.M0, cov.M1 = prov1$cov.M1,
        deltaR2 = deltaR2, alpha = alpha, thr = Q, DIFitems = DIFitems,
        member.type = member.type, match = prov1$match,
        type = type, p.adjust.method = p.adjust.method,
        adjusted.p = adjusted.p,
        puriadjType = puriadjType,
        purification = purify, nrPur = nrPur,
        difPur = difPur, convergence = noLoop, names = colnames(DATA),
        anchor.names = NULL, criterion = criterion, save.output = save.output,
        output = output,
        Data = DATA, group = Group
      )
    }
    class(RES) <- "Logistic"
    return(RES)
  }
  resToReturn <- internalLog()
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
