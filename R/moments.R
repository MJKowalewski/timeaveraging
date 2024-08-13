#' moments
#'
#' @param age A variable containing raw ages (e.g., radiocarbon ages)
#'
#'
#' @return A vector containing the following components:
#'   \item{medianage}{median age of specimens in a given sample}
#'
#' @examples
#'
#' data(bahamas)
#' out1 <- moments(bahamas$carbonateAge)
#'
#' @export

moments <- function(age) {
  medianage <- stats::median(age)
  return(medianage)
}
