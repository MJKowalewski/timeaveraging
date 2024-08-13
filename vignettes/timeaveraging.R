## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----installing timeaveraging package, eval = FALSE---------------------------
#  install.packages('devtools')
#  library(devtools)
#  devtools::install_github('mjkowalewski/timeaveraging', build_vignettes = TRUE)
#  library(timeaveraging)

## ----data example-------------------------------------------------------------
library(timeaveraging)
str(bahamas) # check the structure of the dataset

## ----moments summary function------------------------------------------------
moments(age=bahamas$spec$carbonateAge)
