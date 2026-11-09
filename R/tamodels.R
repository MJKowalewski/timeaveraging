#' Fit theoretical distribution and assess the temporal incompleteness of a sample
#'
#' Fit theoretical distributions (uniform, normal, exponential, and Weibull)
#' to a sample of dated specimens. Assess incompleteness of a sample
#' for a given time bin and significance of incompleteness using Monte Carlo
#' models for uniform, normal, exponential and Weibull distributions.
#'
#' @details An age distribution of a sample of dated specimens is evaluated
#' in terms of their fit to the four theoretical distributions using maximum likelihood.
#' The incompleteness of a sample is estimated as the proportion of bins
#' (time intervals) containing specimens. The incompleteness estimates
#' are compared against predictions of the Monte Carlo models for
#' four theoretical distributions (uniform, normal, exponential, and Weibull).
#'
#' @param x A vector with numerical ages of dated specimens.
#' Note: if negative ages are included, the constant (-min(age) + 1) is added
#' to all age values with a warning
#'
#' @param times A numerical value (number of iterations to be
#' used in Monte Carlo models)
#'
#' @param t A numerical value defining bin width in years
#'
#' @return A list with five items:
#'   \item{models}{a data frame with AIC and BIC
#'   information criteria for each model}
#'   \item{estimate}{observed completeness of a sample for a given \emph{t}}
#'   \item{monte.carlo}{a data frame with Monte Carlo sample-level estimates of
#'   the significance of observed incompleteness (\emph{p}), incompleteness
#'   predicted by each model (\emph{predicted}), including the lower (\emph{Ci.lower})
#'   and upper (\emph{CI.upper}) 95\% confidence intervals, and the observed
#'   completeness (\emph{observed})}
#'   \item{iterations}{number of iterations in Monte Carlo models}
#'   \item{time.bin}{the time bin (\emph{t})}
#'
#' @examples
#'
#' out1 <- postdist(age=florida$post$cal.BP, prob=florida$post$probability,
#' id=florida$post$UniqueID, size=10000)
#'
#' sample1 <- apply(out1, 1, median)
#'
#' results <- tamodels(sample1)
#'
#' results$models
#'
#' results$monte.carlo
#'
#' @importFrom stats quantile runif rexp rweibull rnorm AIC BIC
#' @importFrom MASS fitdistr
#'
#' @export

tamodels <- function(x, times=1000, t = 100) {
  if (min(x) < 0) warning(paste('negative ages (BP?) converted by adding', paste(-min(x)+1), 'years'))
  if (min(x) < 0) x <- x - min(x) + 1
  comp.F <- function(y, t = t) {
    bins <- floor(y/t)
    range <- max(bins) - min(bins) + 1
    k <- length(unique(bins))
    return(k/range)
  }
  obs.comp <- comp.F(x, t = t)
  n.obs <- length(x)
  fit.exp <- MASS::fitdistr(x, "exponential")
  fit.nor <- MASS::fitdistr(x, "normal")
  fit.wei <- MASS::fitdistr(x, densfun="weibull",
                      start=list(shape = 1, scale = mean(x)),
                      lower=c(0.001, 0.001))
  aic.exp <- stats::AIC(fit.exp)
  bic.exp <- stats::BIC(fit.exp)
  aic.nor <- stats::AIC(fit.nor)
  bic.nor <- stats::BIC(fit.nor)
  aic.wei <- stats::AIC(fit.wei)
  bic.wei <- stats::BIC(fit.wei)
  aic.unif <- 2 * 2 - 2 * (-n.obs * log(max(x) - min(x)))
  ll.unif <- -n.obs * log(max(x) - min(x))
  bic.unif <- 2 * log(n.obs) - 2 * ll.unif
  models <- rbind(AIC=c(uniform = aic.unif, normal = aic.nor,
                        exponential = aic.exp, weibull = aic.wei),
                  BIC=c(uniform = bic.unif, normal = bic.nor,
                        exponential = bic.exp, weibull = bic.wei))
  rcomp.unif <- vector(mode='numeric', length=times)
  for (i in 1:times)
    rcomp.unif[i] <- comp.F(stats::runif(n.obs, min(x), max(x)), t=t)
  rcomp.nor <- vector(mode='numeric', length=times)
  for (i in 1:times)
    rcomp.nor[i] <- comp.F(stats::rnorm(n.obs, fit.nor$estimate["mean"],
                                        fit.nor$estimate["sd"]), t = t)
  rcomp.exp <- vector(mode='numeric', length=times)
  for (i in 1:times)
    rcomp.exp[i] <- comp.F(stats::rexp(n.obs, rate=fit.exp$estimate["rate"]), t=t)
  rcomp.wei <- vector(mode='numeric', length=times)
  for (i in 1:times)
    rcomp.wei[i] <- comp.F(stats::rweibull(n.obs, shape=fit.wei$estimate["shape"],
                                               scale=fit.wei$estimate["scale"]), t=t)
  monte.carlo <- rbind(uniform=data.frame(p = (sum(rcomp.unif[-1] <= obs.comp) + 1)/times,
                                   predicted = mean(rcomp.unif),
                                   CI.lower = stats::quantile(rcomp.unif, 0.025),
                                   CI.upper = stats::quantile(rcomp.unif, 0.975),
                                   observed = obs.comp),
                       normal=data.frame(p = (sum(rcomp.nor[-1] <= obs.comp) + 1)/times,
                                              predicted = mean(rcomp.nor),
                                              CI.lower = stats::quantile(rcomp.nor, 0.025),
                                              CI.upper = stats::quantile(rcomp.nor, 0.975),
                                              observed = obs.comp),
                       exponential=data.frame(p = (sum(rcomp.exp[-1] <= obs.comp) + 1)/times,
                                       predicted = mean(rcomp.exp),
                                       CI.lower = stats::quantile(rcomp.exp, 0.025),
                                       CI.upper = stats::quantile(rcomp.exp, 0.975),
                                       observed = obs.comp),
                       weibull=data.frame(p = (sum(rcomp.wei[-1] <= obs.comp) + 1)/times,
                                          predicted = mean(rcomp.wei),
                                          CI.lower = stats::quantile(rcomp.wei, 0.025),
                                          CI.upper = stats::quantile(rcomp.wei, 0.975),
                                          observed = obs.comp))
  return(list(models = models, estimate=obs.comp, monte.carlo = monte.carlo, iterations = times,
              time.bin = t))
}

