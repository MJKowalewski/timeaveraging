#' Fit theoretical distribution and assess the temporal incompleteness of a sample
#'
#' Fit theoretical distributions to a sample of dated specimens and
#' estimate incompleteness of a sample for a given time bin.
#'
#' @details An age distribution of a sample of dated specimens is evaluated
#' against theoretical probability density functions using maximum likelihood.
#' The incompleteness of a sample is estimated as the proportion of bins
#' (time intervals) containing specimens. The incompleteness estimates
#' are compared against predictions of the Monte Carlo models derived for
#' theoretical distributions. The following models are included: uniform,
#' normal, lognormal, exponential, gamma, Weibull (uncensored), Weibull (censored),
#' and two-phase exponential.
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
#'   \item{model.param}{a list with fitted parameters for each model}
#'   \item{predicted}{a data frame with predicted values for each model}
#'   \item{best}{a data frame listing the best model based on AIC and BIC}
#'   \item{completeness}{observed completeness of a sample for a given \emph{t}}
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
#' @importFrom stats runif rexp rweibull rlnorm rnorm rbeta rbinom rgamma
#' @importFrom stats AIC BIC optim approx quantile
#' @importFrom stats dunif dnorm dlnorm dexp dweibull dgamma
#' @importFrom MASS fitdistr
#'
#' @export

tamodels <- function(x, times=1000, t = 100) {
  x <- as.numeric(unlist(x))
  if (min(x) <= 0) warning(paste('negative ages (BP?) converted by adding', paste(-min(x)+1), 'years'))
  if (min(x) <= 0) x <- x - min(x) + 1
  # observed values
  n.obs <- length(x)
  obs.comp <- comp.F(x, t = t)
  # fit models
  # uniform
  aic.unif <- 2 * 2 - 2 * (-n.obs * log(max(x) - min(x)))
  ll.unif <- -n.obs * log(max(x) - min(x))
  bic.unif <- 2 * log(n.obs) - 2 * ll.unif
  #normal
  fit.nor <- MASS::fitdistr(x, "normal")
  aic.nor <- stats::AIC(fit.nor)
  bic.nor <- stats::BIC(fit.nor)
  #lognormal
  fit.lnor <- MASS::fitdistr(x, "lognormal")
  aic.lnor <- stats::AIC(fit.lnor)
  bic.lnor <- stats::BIC(fit.lnor)
  # exponential
  fit.exp <- MASS::fitdistr(x, "exponential")
  aic.exp <- stats::AIC(fit.exp)
  bic.exp <- stats::BIC(fit.exp)
  # gamma
  m.g <- mean(x)
  v.g <- var(x)
  if (length(x) >= 2 && stats::var(x) > 0) {
    m.g <- mean(x)
    v.g <- stats::var(x)
    start_gam <- list(shape = m.g^2 / v.g, rate = m.g / v.g)
    fit.gam <- tryCatch(
      MASS::fitdistr(
        x,
        densfun = "gamma",
        start   = start_gam,
        lower   = c(1e-6, 1e-8),
        control = list(ndeps = c(1e-6, 1e-8))
      ),
      error = function(e) NULL
    )
  } else {
    fit.gam <- NULL
  }
  if (!is.null(fit.gam)) {
    aic.gam  <- stats::AIC(fit.gam)
    bic.gam  <- stats::BIC(fit.gam)
    pred.gam <- stats::dgamma(x,
                              shape = fit.gam$estimate["shape"],
                              rate  = fit.gam$estimate["rate"])
  } else {
    aic.gam  <- NA_real_
    bic.gam  <- NA_real_
    pred.gam <- rep(NA_real_, length(pred.x))
  }
  fit.wei <- MASS::fitdistr(x, densfun="weibull",
                            start=list(shape = 1, scale = mean(x)),
                            lower=c(0.001, 0.001))
  aic.wei <- stats::AIC(fit.wei)
  bic.wei <- stats::BIC(fit.wei)
  # weibull censored
  start1 <- c(log(0.0005), log(0.1))
  fit1 <- stats::optim(par = start1, fn = weibull.censored, x=x)
  cens.weibullrate <- exp(fit1$par[1])
  cens.weibullshape <- exp(fit1$par[2])
  likweibull <- fit1$value
  aic.wei.c <- 2 * 2 - (-2 * likweibull)
  bic.wei.c <- aic.wei.c+((2*2*(2+1))/(length(x)-2-1))
  # two-phase exponential
  out <- two.phase.exponential(x)
  lik <- -(out$lik)
  k <- 3
  aic.two.phase <- 2 * k - (-2 * lik)
  bic.two.phase <- aic.two.phase + ((2 * k * (k + 1)) / (length(x) - k -1))
  beta <- out$beta
  r1 <- out$r1
  r2 <- out$r2
  tau <- out$alpha*(out$r2-out$r1) #disintegration in the TAZ
  lambda1 <- out$r1*out$alpha+out$r2*(1-out$alpha)
  lambda2 <- out$r1 #disintegration in the SZ and burial below it
# AIC and BIC comparison. All models.
  models <- rbind(AIC=c(uniform = aic.unif,
                        normal = aic.nor,
                        lognormal=aic.lnor,
                        exponential = aic.exp,
                        gamma = aic.gam,
                        weibull = aic.wei,
                        weibull.cen = aic.wei.c,
                        two.phase.exp = aic.two.phase),
                  BIC=c(uniform = bic.unif,
                        normal = bic.nor,
                        lognormal=bic.lnor,
                        exponential = bic.exp,
                        gamma = bic.gam,
                        weibull = bic.wei,
                        weibull.cen = bic.wei.c,
                        two.phase.exp = bic.two.phase))
  best.models <- c(AIC=colnames(models)[which(models[1,] == min(models[1,]))],
                   BIC=colnames(models)[which(models[2,] == min(models[2,]))])
# model parameters
  model.param <- list(uniform=c(min=min(x), max=max(x)),
                      normal=c(fit.nor$estimate["mean"],
                               fit.nor$estimate["sd"]),
                      lognormal=c(fit.lnor$estimate["meanlog"],
                                  fit.lnor$estimate["sdlog"]),
                      exponential=fit.exp$estimate["rate"],
                      gamma=c(fit.exp$estimate["shape"],
                              fit.exp$estimate["rate"]),
                      weibull=c(shape=as.numeric(fit.wei$estimate["shape"]),
                                rate=as.numeric(1/fit.wei$estimate["scale"])),
                      weibull.cen=c(shape=exp(fit1$par[2]),
                                    rate=exp(fit1$par[1])),
                      two.phase.exp=c(rate.TAZ=lambda1, rate.SZ=lambda2, tau=tau))
# predicted values
  pred.x <- seq(0, max(x), t) + t/2
  pred.uni <- stats::dunif(pred.x, min=min(x), max=max(x)+t/2)
  pred.exp <- stats::dexp(pred.x, rate = fit.exp$estimate["rate"])
  pred.gam <- stats::dgamma(pred.x, shape=fit.gam$estimate["shape"],
                            rate = fit.gam$estimate["rate"])
  pred.nor <- stats::dnorm(pred.x, mean = fit.nor$estimate["mean"],
                           sd = fit.nor$estimate["sd"])
  pred.lnor <- stats::dlnorm(pred.x, meanlog = fit.lnor$estimate["meanlog"],
                           sdlog = fit.lnor$estimate["sdlog"])
  wr <- 1/fit.wei$estimate[2]
  ws <- fit.wei$estimate[1]
  pred.weib <- ws * wr * (pred.x * wr)^(ws-1) * exp(-(wr * pred.x)^ws)
  cwr <- exp(fit1$par[1])
  cws <- exp(fit1$par[2])
  pred.cweib <- (1/((1/cwr)*gamma(1+1/cws)))*exp(-(cwr*pred.x)^cws)
  pred.2exp <- r1 * exp(-r1*pred.x) * beta + r2 * exp(-r2*pred.x) * (1 - beta)
  predicted <- data.frame(x=pred.x,
                          uniform=(pred.uni/sum(pred.uni))*length(x),
                          normal=(pred.nor/sum(pred.nor))*length(x),
                          lognormal=(pred.lnor/sum(pred.lnor))*length(x),
                          exponential=(pred.exp/sum(pred.exp))*length(x),
                          gamma=(pred.gam/sum(pred.gam))*length(x),
                          weibull=(pred.weib/sum(pred.weib))*length(x),
                          weibull.cen=(pred.cweib/sum(pred.weib))*length(x),
                          two.phase.exp=(pred.2exp/sum(pred.2exp))*length(x))
  colnames(predicted)[colnames(predicted) == best.models["AIC"]] <- paste0(best.models["AIC"],'*')
# MONTE CARLO COMPLETENESS MODELS
# MC uniform
  rcomp.unif <- vector(mode='numeric', length=times)
  for (i in 1:times)
    rcomp.unif[i] <- comp.F(stats::runif(n.obs, min(x), max(x)), t=t)
# MC normal
  rcomp.nor <- vector(mode='numeric', length=times)
  for (i in 1:times)
    rcomp.nor[i] <- comp.F(stats::rnorm(n.obs, fit.nor$estimate["mean"],
                                        fit.nor$estimate["sd"]), t = t)
# MC lognormal
  rcomp.lnor <- vector(mode='numeric', length=times)
  for (i in 1:times)
    rcomp.lnor[i] <- comp.F(stats::rlnorm(n.obs,
                                          meanlog = fit.lnor$estimate["meanlog"],
                                          sdlog = fit.lnor$estimate["sdlog"]),
                            t = t)
# MC exponential
  rcomp.exp <- vector(mode='numeric', length=times)
  for (i in 1:times)
    rcomp.exp[i] <- comp.F(stats::rexp(n.obs,
                                       rate=fit.exp$estimate["rate"]), t=t)
# MC gamma
  rcomp.gam <- vector(mode='numeric', length=times)
  for (i in 1:times)
    rcomp.gam[i] <- comp.F (stats::rgamma(n.obs,
                                          shape=fit.gam$estimate["shape"],
                                          rate = fit.gam$estimate["rate"]),
                            t=t)
# MC weibull (uncensored) (NEEDS TO BE CHECKED)
  rcomp.wei <- vector(mode='numeric', length=times)
  for (i in 1:times)
    rcomp.wei[i] <- comp.F(stats::rweibull(n.obs, shape=fit.wei$estimate["shape"],
                                           scale=fit.wei$estimate["scale"]), t=t)
# MC weibull (censored)
  rcomp.weic <- vector(mode='numeric', length=times)
  for (i in 1:times)
    rcomp.weic[i] <- comp.F(rcensweibull(n.obs,
                                              cens.weibullrate=exp(fit1$par[1]),
                                              cens.weibullshape=exp(fit1$par[2]),
                                              time=ceiling(max(x))), t=t)
# Two phase exponential
  rcomp.two.phase.exp <- vector(mode='numeric', length=times)
  for (i in 1:times)
    rcomp.two.phase.exp[i] <- comp.F(r2exp(n.obs, r1=r1,
                                              r2=r2, beta=beta,
                                              time=ceiling(max(x))),
                                          t=t)
# MC model summary
  uniformMC <- data.frame(p = (sum(rcomp.unif[-1] <= obs.comp) + 1)/times,
                        predicted = mean(rcomp.unif),
                        CI.lower = stats::quantile(rcomp.unif, 0.025),
                        CI.upper = stats::quantile(rcomp.unif, 0.975),
                        observed = obs.comp)
normalMC <- data.frame(p = (sum(rcomp.nor[-1] <= obs.comp) + 1)/times,
                       predicted = mean(rcomp.nor),
                       CI.lower = stats::quantile(rcomp.nor, 0.025),
                       CI.upper = stats::quantile(rcomp.nor, 0.975),
                       observed = obs.comp)
lognormalMC <- data.frame(p = (sum(rcomp.lnor[-1] <= obs.comp) + 1)/times,
                          predicted = mean(rcomp.lnor),
                          CI.lower = stats::quantile(rcomp.lnor, 0.025),
                          CI.upper = stats::quantile(rcomp.lnor, 0.975),
                          observed = obs.comp)
exponentialMC <- data.frame(p = (sum(rcomp.exp[-1] <= obs.comp) + 1)/times,
                            predicted = mean(rcomp.exp),
                            CI.lower = stats::quantile(rcomp.exp, 0.025),
                            CI.upper = stats::quantile(rcomp.exp, 0.975),
                            observed = obs.comp)
gammaMC <- data.frame(p = (sum(rcomp.gam[-1] <= obs.comp) + 1)/times,
                            predicted = mean(rcomp.gam),
                            CI.lower = stats::quantile(rcomp.gam, 0.025),
                            CI.upper = stats::quantile(rcomp.gam, 0.975),
                            observed = obs.comp)
weibullMC <- data.frame(p = (sum(rcomp.wei[-1] <= obs.comp) + 1)/times,
                        predicted = mean(rcomp.wei),
                        CI.lower = stats::quantile(rcomp.wei, 0.025),
                        CI.upper = stats::quantile(rcomp.wei, 0.975),
                        observed = obs.comp)
weibull.cenMC <- data.frame(p = (sum(rcomp.weic[-1] <= obs.comp) + 1)/times,
                            predicted = mean(rcomp.weic),
                            CI.lower = stats::quantile(rcomp.weic, 0.025),
                            CI.upper = stats::quantile(rcomp.weic, 0.975),
                            observed = obs.comp)
two.phase.exp <- data.frame(p = (sum(rcomp.two.phase.exp[-1] <= obs.comp) + 1)/times,
                            predicted = mean(rcomp.two.phase.exp, na.rm=TRUE),
                            CI.lower = stats::quantile(rcomp.two.phase.exp, 0.025, na.rm=TRUE),
                            CI.upper = stats::quantile(rcomp.two.phase.exp, 0.975, na.rm=TRUE),
                            observed = obs.comp)
monte.carlo <- rbind(uniform=uniformMC, normal=normalMC, lognormal=lognormalMC,
                     exponential=exponentialMC, gamma=gammaMC, weibull=weibullMC,
                     weibull.censored=weibull.cenMC,
                     two.phase.exp=two.phase.exp)
  return(list(models = as.data.frame(models),
              model.param = model.param,
              predicted = predicted,
              best = as.data.frame(best.models),
              completeness = obs.comp,
              monte.carlo = monte.carlo,
              iterations = times,
              time.bin = t))
}
