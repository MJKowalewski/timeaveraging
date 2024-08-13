## code to prepare `bahamas` dataset goes here
post <- read.csv('posteriors.csv', skip=20)
spec <- read.csv('specimendata.csv', skip=20)
bahamas <- list(spec=spec, post=post)
usethis::use_data(bahamas, overwrite = TRUE)
