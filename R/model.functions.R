# model functions

# COMPLETENESS FUNCTION
comp.F <- function(y, t = t) {
  bins <- floor(y/t)
  range <- max(bins) - min(bins) + 1
  k <- length(unique(bins))
  return(k/range)
}

# DENSITY FOR CENSORED WEIBULL
weibull.censored <- function(parameter, x) {
  rate  <- exp(parameter[1])
  shape <- exp(parameter[2])
  log_C <- -log(rate) + lgamma(1 + 1 / shape)
  log_lik <- -log_C - (rate * x)^shape
  return(-sum(log_lik))
}

# TWO-PHASE EXPONENTIAL FUNCTION
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
