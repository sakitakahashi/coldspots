library(foreign)
library(maptools)
library(RColorBrewer)
library(ggplot2)
library(mgcv)
library(splancs)
library(rgeos)
library(grid)
library(sp)
library(rgdal)	
library(geoR)
library(maps)

# Source code for analysis
source("./Code/FunctionsForAnalysis.R")

# Read in the master list of country codes
codes <- as.data.frame(read.csv("./Data/Master_List.csv"))

# Only keep Burundi and Tanzania
codes <- codes[c(1,7),]

################################################################################

# The two countries of interest are Burundi and Tanzania

# Fit the GAM model for each country: leave out the subnationals from the model
for(i in 1:nrow(codes)){

	# Load the manipulated data
	load(paste0("./Data/Manipulated_data_for_GAM/", codes$DHS_code[i], ".RData"))

	# Write out the call for the phenomenological model: sub-national SIAs only
	func_name <- paste0("measles_status ~ s(long,lat)+s(binned_survey_age)")
	
	# Fit the phenomenological model
	fit_phenom <- do.call(gam, list(as.formula(func_name), 
						 family=binomial(link="logit"),
						 data=dataInd_country_subnational))
						
	save(fit_phenom, file=paste0("./Results/GAM_fit_phenom_NoSubnationals/", codes$DHS_code[which(codes$Country==codes$Country[i])], "_NoSubnationals.RData"))
	
}

################################################################################

# Get GAM smoothed age parameters for each country: leave out the subnationals from the model

dat_smooth_NoSubnationals <- NULL
for(i in 1:nrow(codes)){

	# Load the GAM ouput
	load(paste0("./Results/GAM_fit_phenom_NoSubnationals/", codes$DHS_code[which(codes$Country==codes$Country[i])], "_NoSubnationals.RData"))

	# Plot the model at the data locations: toggle this off
	# vis.gam(fit_phenom, view=c("long_jittered", "lat_jittered"), plot.type="contour", too.far=0.02, color="terrain", type="response")

	# Diagnostics
	# summary(fit_phenom)

	# Save the plot of the model
	a <- plot.gam(fit_phenom, seWithMean=TRUE, rug=F, main=codes$Country[i], select=2, shade=T, col=rgb(0,0,1), shade.col=rgb(0,0,1,0.5), cex.main=2, cex.axis=1.5, cex.lab=1.5, font.main=1)

	# Save the data
	dat_smooth_NoSubnationals$x <- c(dat_smooth_NoSubnationals$x, a[[2]]$x)
	dat_smooth_NoSubnationals$y <- c(dat_smooth_NoSubnationals$y, a[[2]]$fit)
	dat_smooth_NoSubnationals$y_lower <- c(dat_smooth_NoSubnationals$y_lower, a[[2]]$fit - a[[2]]$se)
	dat_smooth_NoSubnationals$y_upper <- c(dat_smooth_NoSubnationals$y_upper, a[[2]]$fit + a[[2]]$se)
	dat_smooth_NoSubnationals$z <- c(dat_smooth_NoSubnationals$z, rep(codes$Country[i], times=length(a[[2]]$x)))
	
}

dat_smooth_NoSubnationals <- as.data.frame(dat_smooth_NoSubnationals)
dat_smooth_NoSubnationals$zz <- codes$Country[match(dat_smooth_NoSubnationals$z, rownames(codes))]
save(dat_smooth_NoSubnationals, file="./Results/GAM_plot/Two_countries_NoSubnationals.RData")

################################################################################

# Compare old vs. new points for each country: leave out the subnationals from the model
# At each grid cell

dat_smooth_NoSubnationals <- NULL
for(i in 1:nrow(codes)){

	# Load the Della output	I
	load(paste0("./Results/GAMtoMeanAndSE_Della/GAMtoMeanAndSE_",codes$DHS_code[which(codes$Country==codes$Country[i])],"_6-60m.RData"))

	# Save the data
	dat_smooth_NoSubnationals$x <- c(dat_smooth_NoSubnationals$x, plogis(grid_tmp_lt5y[,"Mean_24m"]))
	
	rm(grid_tmp_lt5y)

	# Load the Della output	II
	load(paste0("./Results/GAMtoMeanAndSE_Della_NoSubnationals/GAMtoMeanAndSE_",codes$DHS_code[which(codes$Country==codes$Country[i])],"_6-60m_NoSubnationals.RData"))
	
	# Save the data
	dat_smooth_NoSubnationals$y <- c(dat_smooth_NoSubnationals$y, plogis(grid_tmp_lt5y[,"Mean_24m"]))
	
	# Which country?
	dat_smooth_NoSubnationals$z <- c(dat_smooth_NoSubnationals$z, rep(codes$Country[i], times=nrow(grid_tmp_lt5y)))
	
}

dat_smooth_NoSubnationals <- as.data.frame(dat_smooth_NoSubnationals)
dat_smooth_NoSubnationals$zz <- codes$Country[match(dat_smooth_NoSubnationals$z, rownames(codes))]
save(dat_smooth_NoSubnationals, file="./Results/GAM_plot/Two_countries_NoSubnationals_Comparison.RData")

################################################################################