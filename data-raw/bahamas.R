## code to prepare `bahamas` nad 'florida' datasets
post <- read.csv('posteriors.csv', skip=20)
spec <- read.csv('specimendata.csv', skip=20)
bahamas <- list(spec=spec, post=post)
usethis::use_data(bahamas, overwrite = TRUE)

flpost1 <- read.csv('C:/Users/kowalewski/UFL Dropbox/Michal Kowalewski/R PROJECTS/timeaveraging/data-raw/echMoll_posteriors_05182023.csv', na.strings = c(NA, ""))
flpost2 <- read.csv('C:/Users/kowalewski/UFL Dropbox/Michal Kowalewski/R PROJECTS/timeaveraging/data-raw/echMoll_posteriors_08232023.csv', na.strings = c(NA, ""))
flspec1 <- read.csv('C:/Users/kowalewski/UFL Dropbox/Michal Kowalewski/R PROJECTS/timeaveraging/data-raw/echMoll_spec_07082024.csv', na.strings = c(NA, ""))
flspec <- flspec1[flspec1$include == 1,]
flpost0 <- rbind(flpost1, flpost2)
flpost <- flpost0[which(flpost0$UniqueID %in% flspec$UniqueID),]
florida <- list(spec=flspec, post=flpost)
usethis::use_data(florida, overwrite = TRUE)
