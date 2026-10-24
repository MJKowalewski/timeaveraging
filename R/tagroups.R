#' By-group analysis of age distributions
#'
#' Returns Monte Carlo approximations, moments of age distributions,
#' and bootstrap confidence intervals for data grouped into samples.
#' Dispersion estimates are adjusted for excess variances produced by dating
#' uncertainty. Uses functions \code{\link{postdist}}, \code{\link{tastats}},
#' and \code{\link{tastats.boot}}
#'
#' @details This analysis requires two objects: (1) specimen-level data,
#' which must include specimen id and a grouping variable; and (2) posterior
#' calibration file, which must include age (numeric), probability (numeric),
#' and specimen id. The specimen id must match between the two datasets,
#' although they can be subsets of each other listed in any order.
#'
#' @param group A vector with a grouping variable to be used to group
#' specimens into subsets (specimen dataset).
#'
#' @param id A vector with specimen ids (specimen dataset)
#'
#' @param mcid A vector with specimen ids (posterior probability dataset)
#'
#' @param mcprob A vector with probabilities (posterior probability dataset)
#'
#' @param mcage A vector with ages (posterior probability dataset)
#'
#' @param size An integer indicating the number of iterations
#' used in \code{\link{postdist}}
#'
#' @return a list with five items:
#'   \item{mcdist}{returns a list (with length n = number of groups) of
#'   data.frames with Monte Carlo approximations (see \code{\link{postdist}})
#'   for each group.}
#'   \item{tasummary}{returns a list (with length n = number of groups) of
#'   data.frames with statistics (see \code{\link{tastats}}) for each group.}
#'   \item{taboot}{returns a list (with length n = number of groups) of
#'   data.frames with bootstrap confidence intervals for each group
#'   (see \code{\link{tastats.boot}})}
#'   \item{number.groups}{the integer indicating number of groups}
#'   \item{iterations}{the integer indicating number of replications}
#'
#' @examples
#'
#' res1 <- tagroups(group=bahamas$spec$taxon, id=bahamas$spec$sample,
#' mcid=bahamas$post$Specimen, mcprob=bahamas$post$Probability,
#' mcage=bahamas$post$Age)
#'
#' str(res1)
#'
#' @importFrom boot boot boot.ci
#' @importFrom lmom samlmu
#' @importFrom stats var sd median IQR
#'
#' @export
#'
tagroups <- function(group, id, mcid, mcprob, mcage, size=1000) {
  ngp <- length(unique(group))
  group.list <- list(NULL)
  k <- 0
  for (i in unique(group)) {
    k <- k + 1
    gpspec <- id[which(group==i)]
    mcgroup <- data.frame(age=mcage[which(mcid %in% gpspec)],
          prob=mcprob[which(mcid %in% gpspec)],
          id=mcid[which(mcid %in% gpspec)])
    group.list[[k]] <- mcgroup
    }
  out1 <- lapply(group.list, function(x) postdist(age=x$age, prob=x$prob,
                                                id=x$id, size=size))
  names(out1) <- unique(group)
  out2 <- lapply(out1, tastats)
  out3 <- lapply(out1, tastats.boot)
  return(list(mcdist=out1, tasummary=out2, taboot=out3,
              number.groups=ngp, group.names=unique(group), iterations=size))
}

