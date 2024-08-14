#' timeaveraging: assessing age distributions of individually dated fossil specimens
#'
#' The package 'timeaveraging' provides common measures of age distributions
#' while taking in consideration dating uncertainty estimated via
#' Bayesian posterior calibrations (specimen-level age probability
#' distributions)
#'
#' Plot functions to visualize age distributions are also provided.
#'
#' The current version of the package provides functions for
#' assessing moments of age distributions
#'
#' @author Michal Kowalewski \email{kowalewski@@ufl.edu}
#' @name timeaveraging
#' @aliases timeaveraging
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
NULL
