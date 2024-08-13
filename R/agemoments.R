#' agemoments
#'
#' Computes univariate statistics for a sample of
#' individually dated specimens.
#'
#' @details The function returns basic moments,
#' including mean, median, standard deviation, interquartile range,
#' sample skewness, and L-skewness
#'
#' @param age A variable containing ages (e.g., radiocarbon dates)
#'
#'
#' @return A vector containing the following components:
#'   \item{mean}{arithmetic mean age of specimens in a sample}
#'   \item{median}{median age of specimens in a sample}
#'   \item{std.dev}{standard deviation of specimen ages in a sample}
#'   \item{iqr}{interquartile age range of specimens in a sample}
#'   \item{g1}{sample skewness estimate g1 of specimens in a sample}
#'
#' @export

agemoments <- function(age) {
  meanage <- mean(age)
  medianage <- stats::median(age)
  std <- stats::sd(age)
  iqr <- stats::IQR(age)
  m3 <- sum((age-mean(age))^3)/length(age)
  s3 <- stats::var(age)^(3/2)
  return(c(mean = meanage, median = medianage,
           std.dev = std, iqr = iqr, g1 = m3/s3))
}
