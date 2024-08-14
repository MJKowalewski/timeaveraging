#' agemoments
#'
#' Computes univariate statistics for a sample of
#' individually dated specimens.
#'
#' @details The function returns basic descriptive univariate statistics,
#' including mean, median, standard deviation, interquartile range,
#' sample skewness, and L-skewness
#'
#' @param age A variable containing ages (e.g., radiocarbon dates)
#'
#' @return A vector containing the following components:
#'   \item{mean}{arithmetic mean age of specimens in a sample}
#'   \item{median}{median age of specimens in a sample}
#'   \item{sdev}{standard deviation of specimen ages in a sample}
#'   \item{iqr}{interquartile age range of specimens in a sample}
#'   \item{g1}{sample skewness estimate g1 of specimens in a sample}
#'   \item{t3}{sample L-skewness estimate t3}
#'
#' @examples
#'
#' agemoments(bahamas$spec$carbonateAge)
#'
#' @importFrom lmom samlmu
#'
#' @export

agemoments <- function(age) {
  if (is.numeric(age) == FALSE) stop('age must be numeric')
  if (is.vector(age) == FALSE) stop('age must be a vector')
  if (sum(is.na(age)) > 0) stop('missing values not allowed')
  meanage <- mean(age)
  medianage <- stats::median(age)
  std <- stats::sd(age)
  iqr <- stats::IQR(age)
  m3 <- sum((age-mean(age))^3)/length(age)
  s3 <- stats::var(age)^(3/2)
  t3 <- lmom::samlmu(age)[3]
  return(c(mean = meanage, median = medianage,
           sdev = std, iqr = iqr, g1 = m3/s3, t3 = t3))
}
