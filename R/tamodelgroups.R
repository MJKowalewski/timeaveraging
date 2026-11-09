#' Fit theoretical distribution and assess the temporal incompleteness of multiple sample
#'
#' Fit theoretical distributions and assess temporal incompleteness for
#' multiple samples of dated specimens.
#'
#' @details This wrapper function uses function \code{\link{tamodels}}
#' to evaluate multiple samples (multiple age distributions of dated specimens)
#' in terms of their fit to theoretical distributions and their temporal incompleteness.
#'
#' @param x A list of vectors of numerical ages of dated specimens.
#' Note: if negative ages are included, the constant (-min(age) + 1) is added
#' to all age values with a warning
#'
#' @param times A numerical value (number of iterations to be
#' used in Monte Carlo models)
#'
#' @param t A numerical value defining bin width in years
#'
#' @return A list with five items:
#'   \item{AIC}{a data frame with AIC values for each sample}
#'   \item{BIC}{a data frame with BIC values for each sample}
#'   \item{estimate}{observed completeness of each sample at a given time bin  \emph{t}}
#'   \item{monte.carlo}{a data frame with Monte Carlo estimates of
#'   the significance of observed incompleteness (\emph{p}), the theoretical
#'   distribution selected using AIC (\emph{model}), incompleteness
#'   predicted by each model (\emph{predicted}), including the lower (\emph{Ci.lower})
#'   and upper (\emph{CI.upper}) 95\% confidence intervals, the observed
#'   completeness (\emph{observed})}
#'   \item{iterations}{number of iterations in Monte Carlo models}
#'   \item{time.bin}{the time bin (\emph{t})}
#'
#' @examples
#'
#' out1 <- postdist(age=florida$post$cal.BP, prob=florida$post$probability,
#' id=florida$post$UniqueID, size=10000)
#'
#' out2 <- postdist(age=bahamas$post$Age, prob=bahamas$post$Probability,
#' id=bahamas$post$Specimen, size=10000)
#'
#' samples <- list(florida=apply(out1, 1, median), bahamas=apply(out2, 1, median))
#'
#' results <- tamodelgroups(samples)
#'
#' @importFrom stats quantile runif rexp rweibull rnorm AIC BIC
#' @importFrom MASS fitdistr
#'
#' @export

tamodelgroups <- function(x, times=1000, t=100) {
  out <- lapply(x, tamodels, times=times, t=t)
  sam.est <- sapply(out, `[[`, 'estimate')
  models <- do.call(rbind, lapply(out, `[[`, 'models'))
  models.AIC <- data.frame(samples=names(x),
                      models[rownames(models)=='AIC',],
                      row.names=NULL)
  n.models <- ncol(models.AIC) - 1
  sel.model.AIC <- apply(models.AIC, 1, function(x) which(x == min(x)))
  best.AIC <- colnames(models.AIC)[sel.model.AIC]
  names(best.AIC) <- names(x)
  models.BIC <- data.frame(samples=names(out),
                         models[rownames(models)=='BIC',],
                         row.names=NULL)
  sel.model.BIC <- apply(models.BIC, 1, function(x) which(x == min(x)))
  best.BIC <- colnames(models.BIC)[sel.model.BIC]
  names(best.BIC) <- names(x)
  mc.out <- do.call(rbind, lapply(out, `[[`, 'monte.carlo'))
  mc.estimates <- data.frame(sample = rep(names(out), each=n.models),
                             model = rownames(out[[1]]$monte.carlo),
                             mc.out,
                             BICn=rep(best.BIC, each=n.models),
                             AICn=rep(best.AIC, each=n.models),
                             row.names=NULL)
  mc.estimates$AIC <- as.numeric(mc.estimates$model == mc.estimates$AICn)
  mc.estimates$BIC <- as.numeric(mc.estimates$model == mc.estimates$BICn)
  mc.prefinal <- mc.estimates[ , !(colnames(mc.estimates) %in% c('AICn', 'BICn'))]
  mc.final <- mc.prefinal[which(mc.prefinal$AIC == 1), ]
  return(list(AIC = models.AIC,
              BIC = models.BIC,
              estimate=sam.est,
              monte.carlo = mc.final,
              iterations = out[[1]]$iterations,
              time.bin = out[[1]]$time.bin))
}
