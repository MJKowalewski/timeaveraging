# model functions

# COMPLETENESS FUNCTION
comp.F <- function(y, t = t) {
  bins <- floor(y/t)
  range <- max(bins) - min(bins) + 1
  k <- length(unique(bins))
  return(k/range)
}

# DENSITY FOR CENSORED WEIBULL
#weibull.censored <- function(parameter, x) {
#  parameter[1] <- exp(parameter[1])
#  parameter[2] <- exp(parameter[2])
#  C <- (1/parameter[1])*gamma(1 + 1/parameter[2])
#  lik <- (1/C) * exp(-(parameter[1] * x)^parameter[2])
#  lik <- ifelse(lik<0.00000001, 0.00000001, lik)
#  lik <- log(lik)
#  return(-sum(lik))
#}
weibull.censored <- function(parameter, x) {
  rate  <- exp(parameter[1])
  shape <- exp(parameter[2])
  log_C <- -log(rate) + lgamma(1 + 1 / shape)
  log_lik <- -log_C - (rate * x)^shape
  return(-sum(log_lik))
}
#RANDOM GENERATION FOR THE CENSORED WEIBULL DISTRIBUTION
rcensweibull=function(n.obs, cens.weibullrate, cens.weibullshape, time) {
  time.steps=1:time
  pdf.cens.weibull<-(1/((1/cens.weibullrate)*gamma(1+1/cens.weibullshape)))*exp(-(cens.weibullrate*time.steps)^cens.weibullshape)
  app=approx(x=time.steps, y = pdf.cens.weibull, method = "linear", n=10000)
  out=sample(app$x,n.obs,prob=app$y, replace=T)
  return(out)
}

#RANDOM GENERATION FOR THE TWO-PHASE EXPONENTIAL DISTRIBUTION (ADAM)
r2exp=function(n.obs, r1, r2, beta, time) {
  time.steps=1:time
  pdf.2.exp<-r1*exp(-r1*time.steps)*beta+r2*exp(-r2*time.steps)*(1-beta)
  app=stats::approx(x=time.steps, y = pdf.2.exp, method = "linear", n=10000)
  out=sample(app$x, n.obs, prob=app$y, replace=T)
  return(out)
}

two.phase.exponential <- function(x, niters = 10000, a = 2, b = 0.001, rmean = mean(x)) {
  x <- as.numeric(unlist(x))
  N <- length(x)

  # Log-density helper using log-sum-exp to prevent -Inf
  log_dens <- function(y, beta, r1, r2) {
    l1 <- log(beta) + log(r1) - r1 * y
    l2 <- log(1 - beta) + log(r2) - r2 * y
    m  <- pmax(l1, l2)
    m + log(exp(l1 - m) + exp(l2 - m))
  }

  beta  <- 0.5
  r1    <- 1 / rmean
  r2    <- 1 / rmean
  best  <- c(beta, r1, r2)
  dbest <- sum(log_dens(x, beta, r1, r2))

  is_c1 <- stats::rbinom(N, 1, beta) == 1

  for (iter in 1:niters) {
    n1 <- sum(is_c1)
    n2 <- N - n1

    beta <- stats::rbeta(1, a + n1, a + n2)
    r1   <- stats::rgamma(1, b + n1, rate = rmean * b + sum(x[is_c1]))
    r2   <- stats::rgamma(1, b + n2, rate = rmean * b + sum(x[!is_c1]))

    log_L1 <- log(beta) + log(r1) - r1 * x
    log_L2 <- log(1 - beta) + log(r2) - r2 * x
    prob1  <- stats::plogis(log_L1 - log_L2)

    is_c1 <- stats::rbinom(N, 1, prob1) == 1

    dnew <- sum(log_dens(x, beta, r1, r2))
    if (dnew > dbest) {
      dbest <- dnew
      best  <- c(beta, r1, r2)
    }
  }

  beta <- best[1]
  r1   <- best[2]
  r2   <- best[3]

  if (r1 > r2) {
    beta   <- 1 - beta
    r1temp <- r1
    r1     <- r2
    r2     <- r1temp
  }

  out <- list()
  out$beta    <- beta
  out$r1      <- r1
  out$r2      <- r2
  out$alpha   <- (beta * r1) / (beta * r1 + (1 - beta) * r2)
  out$tau     <- out$alpha * (r2 - r1)
  out$lambda1 <- r2 - out$tau
  out$lambda2 <- r1
  out$lik     <- dbest
  out
}
