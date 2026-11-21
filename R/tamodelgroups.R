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
#'   \item{completeness}{observed completeness of each sample at a given time bin  \emph{t}}
#'   \item{model.param}{fitted parameters for all models for each sample}
#'   \item{model.predict}{a list of matrices with each matrix reporting
#'   predicted values for a given sample}
#'   \item{best.predict}{a list of vectors with each vector reporting
#'   predicted values for the best (AIC-selected) model for a given sample}
#'   \item{monte.carlo.all}{a data frame with Monte Carlo estimates of
#'   the one-tailed significance of observed incompleteness (\emph{p}),
#'   relative to model predictions(\emph{predicted}), incompleteness
#'   predicted by each model (\emph{predicted}), including
#'   the lower (\emph{Ci.lower}) and upper (\emph{CI.upper}) 95\% confidence
#'   intervals, the observed completeness (\emph{observed}),
#'   and \emph{AIC} and \emph{BIC} selected best models}
#'   \item{monte.carlo}{a data frame with estimates limited to the best
#'   model AIC-selected model}
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
  sam.est <- sapply(out, `[[`, 'completeness')
  models <- do.call(rbind, lapply(out, `[[`, 'models'))
  model.param <- do.call(rbind, lapply(out, function(x) unlist(x[['model.param']])))
  model.predict <- simplify2array(lapply(out, function(el) as.matrix(el[['predicted']])))
#  best.predict <- lapply(model.predict,
#                         function(m) m[,grep("*", colnames(m), fixed = TRUE)])
  models.AIC <- data.frame(samples=names(x),
                      models[grep('AIC', rownames(models)),],
                      row.names=NULL)
  #find.model <- apply(models.AIC[,-1], 1, function(x) which(x == min(x)))
  #best.predict2 <- colnames(models.AIC)[-1][find.model]
  n.models <- ncol(models.AIC) - 1
  #sel.model.AIC <- apply(models.AIC, 1, function(x) which(x == min(x)))
  #best.AIC <- colnames(models.AIC)[sel.model.AIC]
  #names(best.AIC) <- names(x)
  models.BIC <- data.frame(samples=names(out),
                         models[grep('BIC', rownames(models)),],
                         row.names=NULL)
  #sel.model.BIC <- apply(models.BIC, 1, function(x) which(x == min(x)))
  #best.BIC <- colnames(models.BIC)[sel.model.BIC]
  #names(best.BIC) <- names(x)
  extract.best <- lapply(out, '[[', 'best')
  best.AIC <- sapply(extract.best, function(x) x[rownames(x) == 'AIC',])
  best.BIC <- sapply(extract.best, function(x) x[rownames(x) == 'BIC',])
  mc.out <- do.call(rbind, lapply(out, `[[`, 'monte.carlo'))
  mc.estimates <- data.frame(sample = rep(names(out), each=n.models),
                             model = rep(colnames(models.AIC)[-1], length(names(out))),
                             mc.out,
                             BICn=rep(best.BIC, each=n.models),
                             AICn=rep(best.AIC, each=n.models),
                             row.names=NULL)
  mc.estimates$AIC <- as.numeric(mc.estimates$model == mc.estimates$AICn)
  mc.estimates$BIC <- as.numeric(mc.estimates$model == mc.estimates$BICn)
  mc.all <- mc.estimates[ , !(colnames(mc.estimates) %in% c('AICn', 'BICn'))]
#  mc.final <- mc.all[which(mc.all$AIC == 1), ]
  return(list(AIC = models.AIC,
              BIC = models.BIC,
              AICbest = best.AIC,
              BICbest = best.BIC,
              completeness=sam.est,
              model.param = model.param,
              model.predict = model.predict,
#              best.predict = best.predict,
#              best.predict2 = best.predict2,
              monte.carlo.all=mc.all,
#             monte.carlo = mc.final,
              iterations = out[[1]]$iterations,
              time.bin = out[[1]]$time.bin))
}
