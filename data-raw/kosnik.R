# # REQUIRES:
# c14probs <- probabilities from carbon-14 calibration # reps <- number of ages to draw # RETURNS:
# matrix of ages drawn from carbon calibration uncertainty distribution #######################################################################################################
c14AgesForShells <- function(c14probs, reps) {
  # specimens
  c14specs <- sort(unique(c14probs$Specimen)) # so that they are integer years
  c14probs$cal.BP <- ceiling(c14probs$Age) # summary table
  sumTable <- matrix(0, nrow=length(c14specs), ncol=reps)
  for (s in 1:length(c14specs)) {
    specData <- c14probs[(c14probs$Specimen == c14specs[s]),] # probabilities sum to 1
    specData$Probability <- specData$Probability / sum(specData$Probability) # cumulative probabilities
    specData$cumProb <- cumsum(specData$Probability) # 'reps' random numbers from a uniform distribution between 0 and 1
    rProbs <- seq(0,1,length.out=(reps+2)) # vector for ages
    rAges <- vector(mode='numeric', length=reps) # convert uniform random to ages for (i in 1:reps)
    rAges[s] <- specData[(specData$cumProb > rProbs[(s+1)]),'cal.BP'][1] # put ages in sumTable
    sumTable[s,] <- rAges
  }
  return(sumTable)
}

c14AgesFromPosterior <- function(c14probs, reps) {
  # specimens
  c14specs <- sort(unique(c14probs$Specimen)) # so that they are integer years
  c14probs$Age <- ceiling(c14probs$Age) # summary table
  sumTable <- matrix(0, nrow=length(c14specs), ncol=reps)
  for (s in 1:length(c14specs))
    { specData <- c14probs[(c14probs$Specimen == c14specs[s]),] # probabilities sum to 1
    specData$Probability <- specData$Probability / sum(specData$Probability)
    specData$mc <- round(specData$Probability*reps)
    specData <- specData[(specData$mc > 0),]
    rAges <- rep(specData$Age,specData$mc) ## if rounding results in not enogh many ages increase the most under represented by one.
  while (length(rAges) < reps) {
    specData$mc1 <- (specData$Probability*reps)
    specData$mcd <- specData$mc - specData$mc1 ## avoids particularlly nasty situation when multiple rows match
    logRows <- which(specData$mcd == min(specData$mcd))
    if (length(logRows) > (reps - length(rAges)))
    logRows <- sample(logRows,(reps - length(rAges)))
    specData[logRows,'mc'] <- specData[logRows,'mc'] + 1
    rAges <- rep(specData$Age,specData$mc)
    }
    ## if rounding results in too many ages decrease the most over represented by one.
    while (length(rAges) > reps) {
      specData$mc1 <- (specData$Probability*reps)
      specData$mcd <- specData$mc - specData$mc1
      logRows <- which(specData$mcd == max(specData$mcd))
      if (length(logRows) > (length(rAges) - reps))
        logRows <- sample(logRows,(length(rAges) - reps))
      specData[logRows,'mc'] <- specData[logRows,'mc'] -1
      rAges <- rep(specData$Age,specData$mc) } # put ages in sumTable
    sumTable[s,] <- rAges
    }
    return(sumTable)
  }


