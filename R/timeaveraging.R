#' timeaveraging: analysis of age distributions of fossils
#'
#' Analyze and model age distributions of individually dated fossils or
#' other geomaterials. Estimate moments of age distributions, including
#' multiple measures of dispersion corrected for excess variance produced by
#' dating errors.
#'
#'
#' The current beta version is limited to single-sample functions and can be
#' used to estimate moments of distribution and bootstrap confidence intervals
#' around them. For each dated specimen, the probability age distribution (e.g.,
#' Bayesian calibration of an radiocarbon age) must be provided.
#'
#' @author Michał Kowalewski \email{kowalewski@@ufl.edu} (author)
#' @author Adam Tomašových \email{adam.tomasovych@@savba.sk} (author)
#' @author Matthew A. Kosnik \email{mkosnik@@gmail.com} (author)
#' @author Rafał Nawrot \email{rafal.nawrot@@univie.ac.at} (author)
#' @author Daniele Scarponi \email{daniele.scarponi@@unibo.it} (conceptual contributor)
#' @author Martin Zuschin \email{martin.zuschin@@univie.ac.at} (conceptual contributor)
#'
#' @examples
#'
#' # data examples
#' data(florida)
#' head(florida)
#'
#' out1 <- postdist(age=bahamas$post$Age, prob=bahamas$post$Probability,
#' id=bahamas$post$Specimen)
#' out1[1:5, 1:5]
#'
#' out2 <- postdist(age=florida$post$cal.BP, prob=florida$post$probability,
#' id=florida$post$UniqueID)
#' out1[1:5, 1:5]
#'
#' tastats(out1)
#' out2 <- tastats(out1, simple=FALSE)
#' str(out2)
#'
#'
#' @importFrom boot boot boot.ci
#' @importFrom lmom samlmu
#'
"_PACKAGE"
