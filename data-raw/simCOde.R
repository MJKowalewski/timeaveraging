################################  FILE LICENSE  ################################
#
#	This file is copyright (C) 2023 Matthew Kosnik
#
#	This program is free software; you can redistribute it and/or modify it 
#	under the terms of version 3 the GNU General Public License as published 
#	by the Free Software Foundation.
#
#	This program is distributed in the hope that it will be useful, but WITHOUT
#	ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
#	FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for 
#	more details.
#
#	To view a copy of the license go to:
#	http://www.gnu.org/licenses/licenses.html#GPL
#	
################################################################################

############################  ABOUT THESE ANALYSES  ############################
#
#	This file was used to generate figures and analyses in the
#	following paper:
#
#
#	doi:
#
#	Matthew Kosnik: mkosnik@alumni.uchicago.edu
#
#	This is the version is current as of the time stamp above, but you are
#	encouraged to contact the authors to obtain the most recent version.
#
#	If something in this file is useful to you... please use it, please cite the
#	original work, and feel free to contact me should you have any questions.
#
#	We would like to acknowledge Tom Olszewski's substantive contribution to
#	this work.
#
################################################################################

{my_packages <- c("colorspace",'EnvStats')
not_installed <- my_packages[!(my_packages %in% installed.packages()[ , "Package"])]
if(length(not_installed)) install.packages(not_installed)}

#library(robustbase)
library(colorspace)
library(EnvStats)

#
#	Should return different distributions with the same mean & sd given sufficient sample size
#	DISTRIBUTIONS ARE CONTSTRAINED TO BE POSITIVE NUMBERS
#	MEAN MUST BE GREATER THAN OR EQUAL TO THE SD
#########################################################################
getSampleAges <- function(distribution, Sim.mu, Sim.sd, Sim.n) {
	
	## NORMAL DISTRIBUTION
	## mean = mean
	## mode = mean
	## median = mean
	## variance = 1/ sigma^2
	## skewness = 0	
	## standard error = SD / sqrt(n)

		if (distribution == 'NOR') {

			if (Sim.mu < Sim.sd)
				return(list(sample=NA, dist= distribution, Sim.mu=Sim.mu, Sim.sd=Sim.sd, Sim.n=Sim.n, Tru.iqr=NA, Tru.median=NA, Tru.mad=NA, error ='error, mean must be greater or equal to sd'));

#			x <- rnorm(Sim.n, mean, sd)
			x <- rnormTrunc(Sim.n, Sim.mu, Sim.sd, min=0)
			q25 <- qnormTrunc(0.25, Sim.mu, Sim.sd, min=0)
			q75 <- qnormTrunc(0.75, Sim.mu, Sim.sd, min=0)
			iqr <- round(q75 - q25)
			mad <- Sim.sd * sqrt(2/pi)
			return(list(sample=round(x), dist='tnormal', Sim.mu=Sim.mu, Sim.sd=Sim.sd, Sim.n= Sim.n, min=0, Tru.iqr=iqr, Tru.median=Sim.mu, Tru.mad=mad, error=NA))
		}
		
	## EXPONENTIAL DISTRIBUTION
	## mean = 1/lambda
	## mode = 0
	## median = ln(2)/lambda
	## variance = 1/lambda^2
	## skewness = 2

		if (distribution == 'EXP') {
			offset <- Sim.mu - (1/Sim.sd)
			
			if (offset < 0)
			return(list(sample=NA, dist= distribution, Sim.mu=Sim.mu, Sim.sd=Sim.sd, Sim.n=Sim.n, Tru.iqr=NA, Tru.median=NA, Tru.mad=NA, error ='error, mean must be greater or equal to 1/sd'));

			x <- rexp(Sim.n,1/Sim.sd) + offset
			#x <- x + (Sim.mu - Sim.sd)
			q25 <- qexp(0.25, 1/Sim.sd)
			q75 <- qexp(0.75, 1/Sim.sd)
			iqr <- round(q75 - q25)
			mad <- NA
			return(list(sample=round(x), dist='exponential', Sim.mu=Sim.mu, Sim.sd=Sim.sd, Sim.n= Sim.n, lambda=1/Sim.sd, Tru.iqr=iqr, Tru.median=log(2)*Sim.mu, Tru.mad=mad, error=NA))
		}

	## UNIFORM DISRIBUTION
	## mean = 1/2 (a+b)
	## mode = any value in (a,b)
	## median = 1/2 (a+b)
	## variance = 1/12(b-a)^2
	## skewness = 0	

		if (distribution == 'UNI') {
			spn <- sqrt(12)* Sim.sd					# this works - set SD assuming a zero yng age.
			yng <- round((2*Sim.mu - spn)/2)		# slide the yng & old age to match the required mean
			old <- yng + spn
			mean <- (old - yng)/2
			if (mean < Sim.mu) {
				old <- old + (Sim.mu - mean)	
				yng <- yng + (Sim.mu - mean)	
			}
			if (yng < 0) 
			return(list(sample=NA, dist='uniform', Sim.mu=Sim.mu, Sim.sd=Sim.sd, Sim.n= Sim.n, min=yng, max=old, error = 'min age must be at least zero'))
			x <- runif(Sim.n, yng, old)
			q25 <- qunif(0.25, yng, old)
			q75 <- qunif(0.75, yng, old)
			iqr <- round(q75 - q25)
			mad <- (1/4)*(old - yng)
			return(list(sample=round(x), dist='uniform', Sim.mu=Sim.mu, Sim.sd=Sim.sd, Sim.n= Sim.n, min=yng, max=old, Tru.iqr=iqr, Tru.median=Sim.mu, Tru.mad=mad, error=NA))
		}
		
		## GAMMA DISTRIBUION
		## needs shape (k) & scale (theta).
		## mean = k * theta
		## median = no simple closed form.
		## variance = k * theta^2
		
		

#	return(x)	
}
