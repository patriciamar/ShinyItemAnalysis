deltamethod <- function (g, mean, cov, ses = TRUE, envir = parent.frame()) 
{
  cov <- as.matrix(cov)
  n <- length(mean)
  if (!is.list(g)) 
    g <- list(g)
  if ((dim(cov)[1] != n) || (dim(cov)[2] != n)) 
    stop(paste("Covariances should be a ", n, " by ", n, 
               " matrix"))
  syms <- paste("x", 1:n, sep = "")
  for (i in 1:n) assign(syms[i], mean[i])
  gdashmu <- t(sapply(g, function(form) {
    as.numeric(attr(eval(deriv(form, syms), envir = envir), "gradient"))
  }))
  new.covar <- gdashmu %*% cov %*% t(gdashmu)
  if (ses) {
    new.se <- sqrt(diag(new.covar))
    new.se
  }
  else new.covar
}


# 
# function (g, mean, cov, ses = TRUE) 
# {
#   cov <- as.matrix(cov)
#   n <- length(mean)
#   if (!is.list(g)) 
#     g <- list(g)
#   if ((dim(cov)[1] != n) || (dim(cov)[2] != n)) 
#     stop(paste("Covariances should be a ", n, " by ", n, 
#                " matrix"))
#   syms <- paste("x", 1:n, sep = "")
#   for (i in 1:n) assign(syms[i], mean[i])
#   gdashmu <- t(sapply(g, function(form) {
#     as.numeric(attr(eval(deriv(form, syms)), "gradient"))
#   }))
#   new.covar <- gdashmu %*% cov %*% t(gdashmu)
#   if (ses) {
#     new.se <- sqrt(diag(new.covar))
#     new.se
#   }
#   else new.covar
# }
# <environment: namespace:msm>