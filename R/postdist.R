#' postdist
#'
#' Generates specimen/date-level Monte Carlo distributions of age uncertainties
#' based on Bayesian posterior probabilities of ages
#'
#' @details Radiocarbon calibration provides posterior probabilities of age values
#' given the observed uncalibrated radiocarbon ages. The function preserves the
#' order of id values in the input data. This function generates a Monte Carlo
#' approximation of posterior probability distribution converting probabilities
#' into a predefined number of age values representing age uncertainty around
#' a given date and calculates specimen level and sample level statistics and
#' time-averaging measures.
#'
#' If outdata=TRUE, the function returns (item: "outdata") a matrix with separate
#' Monte Carlo distributions for all dates. The number of distributions
#' (columns in the matrix) equals the number of dated specimens.
#'
#' The function returns (item: "specs") statistics for individual dates including
#' means, variances, and medians of posterior distributions calculated from
#' Monte Carlo distributions and "exact" means, variances, and medians estimated
#' directly from the posterior probabilities.
#'
#' The function returns (item: "sample") sample-level statistics include:
#'
#' TAV - Total Assemblage Variance (variance of all Monte-Carlo ages)
#'
#' TAVexact - "exact" TAV based on posterior probabilities
#'
#' AEV - Age Estimation Variance calculated as average variance of posterior
#' Monte-Carlo distributions and estimating dating uncertainty
#'
#' AEVexact (AEV estimated directly from posterior probabilities)
#'
#' ETA (ES) - Estimated Time-Averaging corrected for dating uncertainty
#' calculated as 2 * square root of TAV-AEV (2 standard deviations)
#'
#' ETAexact - Same as ETA but calculated using TAXexact and AEVexact
#' OTA (observed time-averaging that does not correct for age uncertainty,
#' calculated as 2*standard deviation of ages (means or medians of posterior
#' distributions) in a sample
#'
#' iqr - interquartile range calculated from mean or median ages of posterior
#' distributions
#'
#' g1 - skewness, the third moment about the mean calculated from mean or
#' median ages of posterior distributions
#'
#' t3 - L-skeweness calculated using function \code{\link[lmom]{samlmu}}
#' from package "lmom".
#'
#' See Ritter et al. 2023 for TAV, AEV, and ETA equations.
#'
#' @param age A numerical vector containing age values to which probabilities are assigned
#'
#' @param prob A numerical vector containing probabilities for each age value
#'
#' @param id A vector containing unique specimen ids
#'
#' @param idname A character string proving a label for 'id' variable (optional)
#'
#' @param size a single numerical value (default = 10000)  defining how many age values
#' should be sampled to approximate the posterior probability distribution
#'
#' @param outdata logical (default=FALSE) to indicate whether generated samples
#' should be returned. If FALSE, a single data.frame with statistical descriptors
#' is returned. If TRUE, a list of two date.frames is returned.
#'
#' @return A list with the following items:
#'   \item{spec}{returns specimen-level estimates of mean age, age variance,
#'   and median age based on Monte Carlo sample from posterior age distributions
#'   and "exact" mean ages, age variances, and median ages calculated directly
#'   from posterior probabilities; see details}
#'   \item{sample}{returns a numerical vector with sample-level estimates of
#'   central tendency, dispersion, and skewness; see details}
#'   \item{outdata}{returns a numerical matrix with n='size' rows and n=number of specimens
#'   columns with Monte Carlo ages based on posterior probability distributions.
#'   NOTE: returned when outdata = TRUE}
#'
#' @examples
#'
#' postdist(age=bahamas$post$Age, prob=bahamas$post$Probability,
#' id=bahamas$post$Specimen, idname='Specimen')
#'
#' @importFrom lmom samlmu
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

postdist <- function(age, prob, id, idname=NULL, size=10000, outdata=FALSE) {

  # split posteriors by specimen id and create Monte Carlo distributions
  multiply <- function(x) sample(x[,1], size=size, replace=T, prob=x[,2])
  id <- factor(id, levels=unique(id)) # to prevent sorting by factor
  x <- split(data.frame(age, prob), id)
  y <- unlist(lapply(x, multiply))
  z <- data.frame(id=rep(unique(id), each=size), age=y)

  # specimen level stats (var, mean, median) based on Monte Carlo distributions
  # and posterior probabilities. Due to binning, probabilities don't always
  # sum up to 1. Here, sum=1 is ensured by x[,2]/sum(x[,2])
  specstatsF <- function(x) c(mean=sum(x)/length(x), var=stats::var(x),
                              median=stats::median(x))
  exactmean <- function(x) sum(x[,1] * (x[,2] / sum(x[,2])))
  varF <- function(x) sum((x[,1] - exactmean(x))^2 * (x[,2]/sum(x[,2]))) * ((nrow(x)-1)/nrow(x))
  medianF <- function(x) {
    c <- cumsum(x[,2])/sum(x[,2]) - 0.5
    (x[,1][which(c == min(c[c>=0]))] + x[,1][which(-c == min(abs(c[c<=0])))])/2
  }
  v <- array2DF(tapply(z$age, z$id, specstatsF))
  colnames(v)[1] <- idname
  rownames(v) <- 1:nrow(v)
  ex.mean <- sapply(x, exactmean)
  ex.var <- sapply(x, varF)
  ex.med <- sapply(x, medianF)

  # sample level stats
  tot.var <- stats::var(z$age)
  ex.tot.var <- varF(cbind(age, prob))
  std2 <- 2*stats::sd(v$mean)
  IQ <- stats::IQR(v$mean)
  g1 <- (sum((v$mean-mean(v$mean))^3)/length(v$mean)) / stats::var(v$mean)^(3/2)
  t3 <- as.numeric(lmom::samlmu(v$mean)[3])
  specstats <- data.frame(v, ex.mean, ex.var, ex.med)
  samplestats <- c(mean=mean(v$mean), median=stats::median(v$median), TAV=tot.var, TAVexact=ex.tot.var, AEV=mean(v$var),
                   AEVexact=mean(ex.var), ETA=2*sqrt(tot.var-mean(v$var)),
                   ETAexact=2*sqrt(ex.tot.var-mean(ex.var)),
                   OTA=std2, iqr=IQ, g1=g1, t3=t3)
  # output
  if (outdata) {
    z2 <- matrix(z[,2], size, length(unique(id)))
    colnames(z2) <- unique(id)
    rownames(z2) <- 1:size
    out1 <- list(sample=samplestats, spec=specstats, outdata=z2, samid=idname)
  }
  if (!outdata) out1 <- list(sample=samplestats, spec=specstats, samid=idname)
  class(out1) <- append(class(out1), "postdist")
  return(out1)
}
