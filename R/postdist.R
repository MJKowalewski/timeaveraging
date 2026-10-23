#' Monte Carlo Approximation of Age Probability Distribution
#'
#' Generates specimen/date-level Monte Carlo distributions of age uncertainties
#' based on Bayesian posterior probabilities of ages
#'
#' @details This function produces a Monte Carlo sample of ages based
#' on the posterior probability age estimates. It should provide robust
#' approximation of the distribution moments when adequate number of replicate
#' ages are used (10000 is recommended here, although 1000 may be sufficient).
#' The resulting object of the class 'postdist' is used by other functions.
#'
#' @param age A numerical vector containing age values to which probabilities are assigned
#'
#' @param prob A numerical vector containing probabilities for each age value
#'
#' @param id A vector containing unique specimen ids
#'
#' @param size a single numerical value (default = 10000)  defining how many age values
#' should be sampled to approximate the posterior probability distribution
#'
#' @return A single matrix of the class 'postdist':
#'   \item{out}{returns a numerical matrix with rows representing individual
#'   age dates columns representing Monte Carlo ages sampled from posterior
#'   probability distributions}
#'
#' @examples
#'
#' out1 <- postdist(age=bahamas$post$Age, prob=bahamas$post$Probability,
#' id=bahamas$post$Specimen)
#'
#' dim(out1)
#'
#' @export
#'

postdist <- function(age, prob, id, size=10000) {
    multiply <- function(x, y) sample(x, size=size, replace=T, prob=y/sum(y))
    age <- ceiling(age)
    out1 <- vector(mode='numeric', length=0)
    for (i in unique(id))
      out1 <- c(out1, multiply(x=age[which(id==i)], y=prob[which(id==i)]))
    out <- matrix(out1, size, length(unique(id)))
    colnames(out) <- unique(id)
    out <- out[,order(colnames(out))]
    class(out) <- append(class(out), "postdist")
    return(t(out))
}
