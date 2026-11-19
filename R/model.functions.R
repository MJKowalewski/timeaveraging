# model functions

# DENSITY FOR CENSORED WEIBULL
weibull.censored <- function(parameter, x) {
  parameter[1] <- exp(parameter[1])
  parameter[2] <- exp(parameter[2])
  C <- (1/parameter[1])*gamma(1 + 1/parameter[2])
  lik <- (1/C) * exp(-(parameter[1] * x)^parameter[2])
  lik <- ifelse(lik<0.00000001, 0.00000001, lik)
  lik <- log(lik)
  return(-sum(lik))
}

# DENSITY FOR RANDOM-TIME 2-PHASE EXPONENTIAL MODEL (CENSORED)
two.phase.exponential <- function(x, niters=10000, a=2, b=0.001, rmean=mean(x)) {
  N <- length(x)
  d <- function(y){
    r1 * exp(-r1 * y) * beta + r2 * exp(-r2 * y) * (1 - beta)
  }
  beta <- 0.5
  r1 <- 1
  r2 <- 1
  best <- c(beta, r1, r2)
  dbest <- sum(log(d(x)))
  cluster <- stats::rbinom(N, 1, beta)
  c <- which(cluster == 1)
  for (iter in 1:niters) {
    beta <- stats::rbeta(1, a + length(c), a + N - length(c))
    r1 <- stats::rgamma(1, b + length(c), rate=rmean * b + sum(x[c]))
    r2 <- stats::rgamma(1, b + N - length(c), rate=rmean * b + sum(x[-c]))
    L1 <- beta * r1 * exp(-r1 * x)
    L2 <- (1 - beta) * r2 * exp(-r2 * x)
    cluster <- stats::rbinom(N, 1, L1/(L1 + L2))
    c <- which(cluster==1)
    dnew <- sum(log(d(x)))
    if (dnew > dbest) {
      dbest <- dnew
      best <- c(beta, r1, r2)
    }
  }
  beta <- best[1]
  r1 <- best[2]
  r2 <- best[3]
  if (r1 > r2) {
    beta <- 1 - beta
    r1temp <- r1
    r1 <- r2
    r2 <- r1temp
    }
  out=list()
  out$beta <- beta
  out$r1 <- r1
  out$r2 <- r2
  out$alpha <- (beta * r1) / (beta * r1 + (1 - beta) * r2)
  out$tau <- out$alpha * (r2 - r1)
  out$lambda1 <- r2 - out$tau
  out$lambda2 <- r1
  out$lik <- dbest
  out
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
