#' timeaveraging: assessing age distributions of fossil specimens
#'
#' The package 'timeaveraging' provides functions to assess and visualize
#' age distributions while inorporating dating uncertainty estimated via
#' Bayesian posterior calibrations (specimen-level age probability
#' distributions)
#'
#'
#' The current version of the package provides functions for
#' assessing moments of age distributions
#'
#' @author Michal Kowalewski \email{kowalewski@@ufl.edu}
#' @name timeaveraging
#' @docType package
#'
#' @examples
#'
#' # data examples
#' data(bahamas)
#' str(bahamas)
#'
#' # agemoments
#' agemoments(bahamas$spec$carbonateAge)
#'
#' @importFrom lmom samlmu
#'
NULL
