#' postdist
#'
#' Generates specimen/date-level Monte Carlo distributions of age uncertainties
#' based on Bayesian posterior probabilities of ages
#'
#' @details Radiocarbon calibration provides posterior probabilities of age values
#' given the observed uncalibrated radiocarbon age. This function generates a Monte Carlo
#' approximation of posterior probability distribution converting probabilities into
#' a predefined number of age values representing age uncertainty around a given date.
#' The number of distributions equals the number of dated specimens. The function preserves
#' the order of id values in the original file.
#'
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
#' @return A list with two data.frames (if outdate=TRUE) and a data.frame (if outdate=False):
#'   \item{stats}{returns means, variances and medians of posterior age distributions
#'    and exact means and medians calculated directly from the probability distribution}
#'   \item{outdata}{archives Monte Carlo approximations of posterior probability distributions,
#'   returned if outdata=TRUE}
#'
#' @examples
#'
#' postdist(age=bahamas$post$Age, prob=bahamas$post$Probability,
#' id=bahamas$post$Specimen, idname='Specimen')
#'
#' @export
#'

postdist <- function(age, prob, id, idname=NULL, size=10000, outdata=FALSE) {
  id <- factor(id, levels=unique(id)) # to prevent sorting by factor
  x <- split(data.frame(age, prob), id)
  y <- unlist(lapply(x, function(x) sample(x[,1], size=size, replace=T, prob=x[,2])))
  z <- data.frame(id=rep(unique(id), each=size), age=y)
  # probabilities do not always sum up to 1 in calibration files: corrected by /sum(x[,2]
  ex.mean <- sapply(x, function(x) sum(x[,1] * (x[,2] / sum(x[,2]))))
  varF <- function(x) sum((x[,1] - mean(x[,1]))^2 * (x[,2]/sum(x[,2]))) * ((nrow(x)-1)/nrow(x))
  ex.var <- sapply(x, varF)
  medianF <- function(x) {
    c <- cumsum(x[,2])/sum(x[,2]) - 0.5
    (x[,1][which(c == min(c[c>=0]))] + x[,1][which(-c == min(abs(c[c<=0])))])/2
  }
  ex.med <- sapply(x, medianF)
  v <- tapply(z$age, z$id, function(x) c(mean=mean(x), var=stats::var(x), median=stats::median(x)))
  v <- array2DF(v)
  colnames(v)[1] <- idname
  rownames(v) <- 1:nrow(v)
  if (outdata) return(list(stats=data.frame(v, ex.mean), outdata=z))
  if (!outdata) return(stats=data.frame(v, ex.mean, ex.var, ex.med))
}

