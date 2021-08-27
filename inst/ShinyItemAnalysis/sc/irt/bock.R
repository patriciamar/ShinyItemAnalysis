library(mirt)

# loading data
data(HCItest, HCI, package = "ShinyItemAnalysis")
HCInumeric <- HCItest[, 1:20]
HCInumeric[] <- sapply(HCInumeric, as.numeric)

# model
fit <- mirt(HCInumeric, model = 1, itemtype = "nominal", SE = TRUE)

# item response curves
plot(fit, type = "trace")
# item information curves
plot(fit, type = "infotrace", facet_items = FALSE)
# test information curve
plot(fit, type = "infoSE")

# estimated parameters
coef(fit, simplify = TRUE) # mirt default parametrization
coef(fit, printSE = TRUE) # SE printed only w/ simplify = FALSE
coef(fit, IRTpars = TRUE, simplify = TRUE) # Bock's original parametrization
#  i.e. intercept-slope a*theta + d, sums of a and of d restricted to 0
coef(fit, IRTpars = TRUE, printSE = TRUE) # SE not printed

# factor scores vs standardized total scores
fs <- as.vector(fscores(fit))
sts <- as.vector(scale(rowSums(HCI[, 1:20])))
plot(fs ~ sts, xlab = "Standardized total score", ylab = "Factor score")
cor(fs, sts)

# The following settings are used in ShinyItemAnalysis:
# 1. RELEVELING DATA to account for the correct answer
data <- HCItest[, 1:20]
key <- unlist(HCIkey)
m <- ncol(data)

levels_data_original <- lapply(1:m, function(i) levels(factor(unlist(data[, i]))))
lev <- c(unlist(levels_data_original), levels(key)) # all levels in data and key
lev <- unique(lev) # all unique levels
lev_num <- as.numeric(as.factor(lev)) - 1 # change them to numbers

# new numeric levels for key
levels_key_num <- sapply(
  1:length(levels(key)),
  function(i) lev_num[levels(key)[i] == lev]
)
# new numeric levels for dataset
levels_data_num <- lapply(1:m, function(i) {
  sapply(
    1:length(levels(factor(unlist(data[, i])))),
    function(j) lev_num[levels(factor(unlist(data[, i])))[j] == lev]
  )
})

# creating new numeric key
key_num <- key
levels(key_num) <- levels_key_num
key_num <- as.numeric(paste(key_num))

# creating new numeric dataset
data_num <- data.frame(data)
for (i in 1:m) {
  levels(data_num[, i]) <- levels_data_num[[i]]
  data_num[, i] <- as.numeric(paste(data_num[, i]))
}

# 2. SETTING THE STARTING VALUES AND CONSTRAINTS
# starting values
sv <- mirt(data_num, 1, "nominal", pars = "values", verbose = FALSE, SE = TRUE)

# starting values of discrimination for distractors need to be lower than
# for the correct answer (fixed at 0, see below)
sv$value[grepl("ak", sv$name)] <- -0.5
sv$est[grepl("ak", sv$name)] <- TRUE

# ak and d parameters for the correct answer are fixed to 0
for (i in 1:m) {
  item_name <- colnames(data_num)[i]
  tmp <- sv[sv$item == item_name, ]
  tmp$est <- TRUE
  tmp[tmp$name == paste0("ak", key_num[i]), "value"] <- 0
  tmp[tmp$name == paste0("ak", key_num[i]), "est"] <- FALSE
  tmp[tmp$name == paste0("d", key_num[i]), "value"] <- 0
  tmp[tmp$name == paste0("d", key_num[i]), "est"] <- FALSE
  sv[sv$item == item_name, ] <- tmp
}

# a1 parameter set not to be estimated and fixed to 1 (to obtain original Bock model)
sv[sv$name == "a1", "value"] <- 1
sv[sv$name == "a1", "est"] <- FALSE
sv

# FITTING THE MODEL - Original Bock model (i.e., a1 = 1), but with different constraints
# (instead of zero sums of ak and d, the parameters of correct answers are fixed to 0)
fit <- mirt(data_num, model = 1, itemtype = "nominal", pars = sv, SE = TRUE)
fit
coef(fit, simplify = TRUE)
# $items
#         a1    ak0    ak1    ak2    ak3     d0     d1     d2     d3    ak4     d4
# Item.1   1 -1.374 -0.407 -0.997  0.000 -3.315 -2.029 -1.632  0.000     NA     NA
# Item.2   1 -0.984  0.000 -0.445     NA -1.897  0.000 -2.039     NA     NA     NA
# Item.3   1  0.000 -2.090 -1.363     NA  0.000 -3.716 -2.805     NA     NA     NA
# ...

