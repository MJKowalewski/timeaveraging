#' timeaveraging: assessing age distributions of fossil specimens
#'
#' The package 'timeaveraging' provides functions to assess and visualize
#' age distributions while incorporating dating uncertainty estimated via
#' Bayesian posterior calibrations (specimen-level age probability
#' distributions)
#'
#'
#' The current version of the package provides functions for
#' assessing moments of age distributions
#'
#' @author Michal Kowalewski \email{kowalewski@@ufl.edu}
#'
#' @examples
#'
#' # data examples
#' data(florida)
#' str(florida)
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
