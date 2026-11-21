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
  r_unif <- max(x) - min(x)
  a_unif <- max(0, min(x) - r_unif / (n.obs - 1))
  b_unif <- max(x) + r_unif / (n.obs - 1)
  ll.unif  <- -n.obs * log(b_unif - a_unif)
  aic.unif <- 2 * 2 - 2 * ll.unif
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
  fit.gamma.F <- function(x) {
    m <- mean(x)
    v <- var(x)
    if (v < .Machine$double.eps) v <- (m * 1e-3)^2
    start_gam <- list(
      shape = max(m^2 / v, 1e-4),
      rate  = max(m / v, 1e-6)
    )
    MASS::fitdistr(
      x,
      densfun = "gamma",
      start   = start_gam,
      lower   = c(shape = 1e-6, rate = 1e-8),
      control = list(
        parscale = c(start_gam$shape, start_gam$rate), # Crucial for disparate scales
        fnscale  = length(x),                          # Keeps log-likelihood O(1)
        ndeps    = c(1e-5, 1e-5),                      # Safe relative step size once parscale is set
        maxit    = 1000
      )
    )
  }
  fit.gam <- fit.gamma.F(x)
  aic.gam <- stats::AIC(fit.gam)
  bic.gam <- stats::BIC(fit.gam)
  fit.wei <- MASS::fitdistr(x, densfun="weibull",
                            start=list(shape = 1, scale = mean(x)),
                            lower=c(0.001, 0.001))
  aic.wei <- stats::AIC(fit.wei)
  bic.wei <- stats::BIC(fit.wei)
  # weibull censored
  start1 <- c(log(1 / mean(x)), log(1))
  fit1 <- stats::optim(par = start1, fn = weibull.censored, x = x,
                       method = "L-BFGS-B",
                       lower  = c(log(1e-8), log(0.05)),
                       upper  = c(log(1e4),  log(10)))
  cens.weibullrate  <- exp(fit1$par[1])
  cens.weibullshape <- exp(fit1$par[2])
  ll.wei.c  <- -fit1$value
  aic.wei.c <- 2 * 2 - 2 * ll.wei.c
  bic.wei.c <- 2 * log(n.obs) - 2 * ll.wei.c

  start1 <- c(log(1 / mean(x)), log(1))
  fit1 <- stats::optim(
    par    = start1,
    fn     = weibull.censored,
    x      = x,
    method = "L-BFGS-B",
    lower  = c(log(1e-8), log(0.05)),
    upper  = c(log(1e4),  log(10))
  )
  # two-phase exponential
  out <- two.phase.exponential(x)
  ll.two.phase  <- out$lik
  #  lik <- -(out$lik)
  k <- 3
  aic.two.phase <- 2 * k - 2 * ll.two.phase
  bic.two.phase <- k * log(n.obs) - 2 * ll.two.phase
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
# model parameters
  model.param <- list(uniform=c(min=a_unif, max=b_unif),
                      normal=c(fit.nor$estimate["mean"],
                               fit.nor$estimate["sd"]),
                      lognormal=c(fit.lnor$estimate["meanlog"],
                                  fit.lnor$estimate["sdlog"]),
                      exponential=fit.exp$estimate["rate"],
                      gamma=c(fit.gam$estimate["shape"],
                              fit.gam$estimate["rate"]),
                      weibull=c(shape=as.numeric(fit.wei$estimate["shape"]),
                                rate=as.numeric(1/fit.wei$estimate["scale"])),
                      weibull.cen=c(shape=exp(fit1$par[2]),
                                    rate=exp(fit1$par[1])),
                      two.phase.exp=c(rate.TAZ=lambda1, rate.SZ=lambda2, tau=tau))
# predicted values
  pred.x <- seq(0, max(x), t) + t/2
  pred.uni <- stats::dunif(pred.x, min = a_unif, max = b_unif)
  pred.exp <- stats::dexp(pred.x, rate = fit.exp$estimate["rate"])
  pred.gam <- stats::dgamma(pred.x, shape = fit.gam$estimate["shape"],
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
                          weibull.cen=(pred.cweib/sum(pred.cweib))*length(x),
                          two.phase.exp=(pred.2exp/sum(pred.2exp))*length(x))
# MONTE CARLO COMPLETENESS MODELS
# MC uniform
  rcomp.unif <- vector(mode='numeric', length=times)
  for (i in 1:times)
    rcomp.unif[i] <- comp.F(stats::runif(n.obs, a_unif, b_unif), t = t)
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
  rcomp.weic <- replicate(times, {
    log_y <- log(stats::rgamma(n.obs, shape = 1 / cens.weibullshape, rate = 1))
    sim   <- exp(log_y / cens.weibullshape) / cens.weibullrate
    comp.F(sim, t = t)
  })
  rcomp.two.phase.exp <- replicate(times, {
    c1 <- stats::rbinom(n.obs, 1, out$beta) == 1
    sim <- numeric(n.obs)
    sim[c1]  <- stats::rexp(sum(c1), rate = out$r1)
    sim[!c1] <- stats::rexp(sum(!c1), rate = out$r2)
    comp.F(sim, t = t)
  })
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
# 1. Compute AICc for all models (crucial when n < 40)
k_vec <- c(
  uniform = 2, normal = 2, lognormal = 2, exponential = 1,
  gamma = 2, weibull = 2, weibull.cen = 2, two.phase.exp = 3
)

aicc_vec <- models["AIC", ] + (2 * k_vec * (k_vec + 1)) / pmax(1, n.obs - k_vec - 1)
models   <- rbind(models, AICc = aicc_vec)

# 2. Align Monte Carlo p-values with column names in `models`
# (Ensure row order of monte.carlo matches colnames(models))
mc_p <- monte.carlo[, "p"]
names(mc_p) <- colnames(models)

# 3. Two-step selection helper
select_best <- function(ic_values, p_values, alpha = 0.05) {
  valid <- which(p_values >= alpha & !is.na(ic_values))

  if (length(valid) > 0) {
    # Step 2: Lowest IC among models that pass Monte Carlo GoF
    best_idx <- valid[which.min(ic_values[valid])]
    return(names(ic_values)[best_idx])
  } else {
    # Fallback: Highest Monte Carlo p-value if all models fail GoF
    warning("No model passed Monte Carlo GoF (p >= ", alpha, "); selecting closest MC match.")
    return(names(p_values)[which.max(p_values)])
  }
}

best.models <- c(
  AIC  = select_best(models["AIC", ],  mc_p),
  AICc = select_best(models["AICc", ], mc_p),
  BIC  = select_best(models["BIC", ],  mc_p)
)
win_col <- best.models["AICc"]
colnames(predicted)[colnames(predicted) == win_col] <- paste0(win_col, "*")
  return(list(models = as.data.frame(models),
              model.param = model.param,
              predicted = predicted,
              best = as.data.frame(best.models),
              completeness = obs.comp,
              monte.carlo = monte.carlo,
              iterations = times,
              time.bin = t))
}
