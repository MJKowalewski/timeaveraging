#' tastats.boot
#'
#' Returns sample-level bootstrap estimates of statistics describing
#' an age distribution of age dates. Accepts object of the class 'postidst' generated
#' by function \code{\link{postdist}}
#'
#' @details If 'fixed' = TRUE (default), the age estimate
#' of each specimen (typically a median of a Bayesian posterior distribution)
#' is resampled. If 'fixed' = FALSE, the age estimate is a single age value
#' sampled from the Bayesian posterior distribution. Typically, the results are
#' comparable between the two resampling protocols and the function is slightly
#' faster when 'fixed' = TRUE.
#'
#' The following summary statistics are returned:
#'
#' boot.ETA - bootstrap estimate of Estimated Time-Averaging  corrected for
#' dating uncertainty calculated as 2 * square root of TAV-AEV
#' (see Ritter et al. 2023)
#'
#' boot.IQR - bootstrap estimate IQR corrected for dating uncertainty computed
#' as ETA/2 multiplied by iq.age/std.age (see Tomasovych et al. 2023)
#'
#' boot.g1 - bootstrap estimate of skewness, the third moment about the mean calculated from mean or
#' median ages of posterior distributions
#'
#' boot.l3 - bootstrap estimate of L-skeweness calculated using
#' function \code{\link[lmom]{samlmu}} from package "lmom".
#'
#' @param x A list of class 'postdist' returned by \code{\link{postdist}} function
#'
#' @param iter A numerical value (default = 1000) defining number of replicate
#' bootstrap values that will be resampled.
#'
#' @param fixed A logical argument (default=TRUE) to determine whether the median
#' age estimate (TRUE) or a random age value (FALSE) should be drawn from
#' the Bayesian posterior distribution.
#'
#' @param bootdata A logical argument (default=FALSE) to determine if bootstrap
#' distributions of the statistics should be returned.
#'
#' @return A dataframe with sample-level statistics (if simple=T) or
#' a list with two items (if simple=F):
#'   \item{outsummary}{returns a dataframe with bootstrap estimates
#'   of mean, 2.5, 25, 75, and 97.5 quantiles.}
#'   \item{boot.distributions}{a matrix with distributions of age estimates
#'   for the evaluated statistics. Returned, if 'bootdata' = TRUE.}
#'
#' @examples
#'
#' out1 <- postdist(age=florida$post$cal.BP, prob=florida$post$probability,
#' id=florida$post$UniqueID, size=1000)
#'
#' out2 <- tastats.boot(out1, iter=100, bootdata=TRUE)
#' out2$outsummary
#' str(out2$boot.distributions)
#'
#' @importFrom lmom samlmu
#' @importFrom stats var sd median IQR
#'
#' @export
#'
#' @references Hosking, JRM, 2023, L-Moments. R package, version 3.0.
#' https://CRAN.R-project.org/package=lmom
#'
#' Ritter, M, Erthal F, Kosnik, MA, Kowalewski, M, Coimbra, CJ,
#' Caron, F, Kaufman, DS, 2023, Onshore-offshore trends in the temporal
#' resolution of molluscan death assemblages: how age-frequency
#' distributions reveal Quaternary sea-level history.
#' Palaios, 38: 148-157. http://dx.doi.org/10.2110/palo.2021.041
#'
#' Tomašových, A., Kidwell, S.M. and Dai, R., (2023), A downcore increase
#' in time averaging is the null expectation from the transit of death assemblages
#' through a mixed layer. Paleobiology, 49(3), pp.527-562.
#' https://doi.org/10.1017/pab.2022.42
#'
tastats.boot <- function(x, fixed=TRUE, iter=100, bootdata=FALSE) {
  if (!('postdist' %in% class(x))) stop('object of the class "postdist" is required')
  g1F <- function(x) (sum((x-mean(x))^3)/length(x)) / stats::var(x)^(3/2)
  l3F <- function(x) as.numeric(lmom::samlmu(x)[3])
  outboot <- NULL
  for (i in 1:iter) {
    boot.sam <- x[sample(1:nrow(x), replace=T),]
    boot.AEV <- mean(apply(boot.sam, 1, var))
    boot.TAV <- var(as.vector(boot.sam))
    boot.ETA <- 2*sqrt(boot.TAV - boot.AEV)
    if (fixed) boot.ages <- apply(boot.sam, 1, sample, size=1)
    if (!fixed) boot.ages <- apply(boot.sam, 1, stats::median)
    boot.IQR <- 0.5 * boot.ETA * (stats::IQR(boot.ages) / stats::sd(boot.ages))
    boot.g1 <- g1F(boot.ages)
    boot.l3 <- l3F(boot.ages)
    boot.stats <- data.frame(boot.ETA, boot.IQR, boot.g1, boot.l3, no.iter=iter, n=nrow(x))
    outboot <- rbind(outboot, boot.stats)
  }
  mean.and.cf <- function(x) c(mean=mean(x),
                               stats::quantile(x, prob=c(0.025, 0.25, 0.75, 0.975)))
  outsummary <- apply(outboot[,1:4], 2, mean.and.cf)
    if (!bootdata) return(outsummary)
    if (bootdata) return(list(summary=outsummary, boot.distributions=outboot))
}
