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
source("./Code/Functions_For_Analysis.R")

# Read in the master list of country codes
codes <- as.data.frame(read.csv("./Data/Master_List.csv"))

################################################################################

# Fit the GAM model for each country: jitter

for(i in 1:nrow(codes)){

	# Load the manipulated data
	load(paste0("./Data/Manipulated_data_for_GAM_JitterAndWeights/", codes$DHS_code[i], ".RData"))

	# Write out the call for the phenomenological model: sub-national SIAs only
	func_name <- paste0("measles_status ~ s(long_jittered,lat_jittered)+s(binned_survey_age)+", paste(which_SIAs_subnational, collapse="+"))
	
	if(length(which_SIAs_subnational)==0) func_name <- paste0("measles_status ~ s(long_jittered,lat_jittered)+s(binned_survey_age)")
		
	# Fit the phenomenological model
	fit_phenom <- do.call(gam, list(as.formula(func_name), 
						 family=binomial(link="logit"),
						 data=dataInd_country_subnational))
						
	save(fit_phenom, file=paste0("./Results/GAM_fit_phenom_JitterAndWeights/", codes$DHS_code[which(codes$Country==codes$Country[i])], "_Jitter.RData"))
	
}

# Fit the GAM model for each country: weights

for(i in 1:nrow(codes)){

	# Load the manipulated data
	load(paste0("./Data/Manipulated_data_for_GAM_JitterAndWeights/", codes$DHS_code[i], ".RData"))

	# Write out the call for the phenomenological model: sub-national SIAs only
	func_name <- paste0("measles_status ~ s(long,lat)+s(binned_survey_age)+", paste(which_SIAs_subnational, collapse="+"))
	
	if(length(which_SIAs_subnational)==0) func_name <- paste0("measles_status ~ s(long,lat)+s(binned_survey_age)")
	
	# Figure out the weights
	wts <- dataInd_country_subnational$DHS_weight/mean(dataInd_country_subnational$DHS_weight)
	
	# Fit the phenomenological model
	fit_phenom <- do.call(gam, list(as.formula(func_name), 
						 family=binomial(link="logit"),
						 weights=wts, 
						 data=dataInd_country_subnational))
						
	save(fit_phenom, file=paste0("./Results/GAM_fit_phenom_JitterAndWeights/", codes$DHS_code[which(codes$Country==codes$Country[i])], "_Weights.RData"))
	
}	

################################################################################

# Get GAM smoothed age parameters for each country: jitter

dat_smooth <- NULL
for(i in 1:nrow(codes)){

	# Load the GAM ouput
	load(paste0("./Results/GAM_fit_phenom_JitterAndWeights/", codes$DHS_code[which(codes$Country==codes$Country[i])], "_Jitter.RData"))

	# Plot the model at the data locations: toggle this off
	# vis.gam(fit_phenom, view=c("long_jittered", "lat_jittered"), plot.type="contour", too.far=0.02, color="terrain", type="response")

	# Diagnostics
	# summary(fit_phenom)

	# Save the plot of the model
	a <- plot.gam(fit_phenom, seWithMean=TRUE, rug=F, main=codes$Country[i], select=2, shade=T, col=rgb(0,0,1), shade.col=rgb(0,0,1,0.5), cex.main=2, cex.axis=1.5, cex.lab=1.5, font.main=1)

	# Save the data
	dat_smooth$x <- c(dat_smooth$x, a[[2]]$x)
	dat_smooth$y <- c(dat_smooth$y, a[[2]]$fit)
	dat_smooth$y_lower <- c(dat_smooth$y_lower, a[[2]]$fit - a[[2]]$se)
	dat_smooth$y_upper <- c(dat_smooth$y_upper, a[[2]]$fit + a[[2]]$se)
	dat_smooth$z <- c(dat_smooth$z, rep(codes$Country[i], times=length(a[[2]]$x)))
	
}

dat_smooth <- as.data.frame(dat_smooth)
dat_smooth$zz <- codes$Country[match(dat_smooth$z, rownames(codes))]
save(dat_smooth, file="./Results/GAM_plot/All_countries_Jitter.RData")

# Get GAM smoothed age parameters for each country: weights

dat_smooth <- NULL
for(i in 1:nrow(codes)){

	# Load the GAM ouput
	load(paste0("./Results/GAM_fit_phenom_JitterAndWeights/", codes$DHS_code[which(codes$Country==codes$Country[i])], "_Weights.RData"))

	# Plot the model at the data locations: toggle this off
	# vis.gam(fit_phenom, view=c("long", "lat"), plot.type="contour", too.far=0.02, color="terrain", type="response")

	# Diagnostics
	# summary(fit_phenom)

	# Save the plot of the model
	a <- plot.gam(fit_phenom, seWithMean=TRUE, rug=F, main=codes$Country[i], select=2, shade=T, col=rgb(0,0,1), shade.col=rgb(0,0,1,0.5), cex.main=2, cex.axis=1.5, cex.lab=1.5, font.main=1)

	# Save the data
	dat_smooth$x <- c(dat_smooth$x, a[[2]]$x)
	dat_smooth$y <- c(dat_smooth$y, a[[2]]$fit)
	dat_smooth$y_lower <- c(dat_smooth$y_lower, a[[2]]$fit - a[[2]]$se)
	dat_smooth$y_upper <- c(dat_smooth$y_upper, a[[2]]$fit + a[[2]]$se)
	dat_smooth$z <- c(dat_smooth$z, rep(codes$Country[i], times=length(a[[2]]$x)))
	
}

dat_smooth <- as.data.frame(dat_smooth)
dat_smooth$zz <- codes$Country[match(dat_smooth$z, rownames(codes))]
save(dat_smooth, file="./Results/GAM_plot/All_countries_Weights.RData")

################################################################################

# Compare old vs. new points for each country: jitter
# At each grid cell

dat_smooth_Jitter <- NULL
for(i in 1:nrow(codes)){

	# Load the Della output	I
	load(paste0("./Results/GAMtoMeanAndSE_Della/GAMtoMeanAndSE_",codes$DHS_code[which(codes$Country==codes$Country[i])],"_6-60m.RData"))

	# Save the data
	dat_smooth_Jitter$x <- c(dat_smooth_Jitter$x, plogis(grid_tmp_lt5y[,"Mean_24m"]))
	
	rm(grid_tmp_lt5y)

	# Load the Della output	II
	load(paste0("./Results/GAMtoMeanAndSE_Della_JitterAndWeights/GAMtoMeanAndSE_",codes$DHS_code[which(codes$Country==codes$Country[i])],"_6-60m_Jitter.RData"))
	
	# Save the data
	dat_smooth_Jitter$y <- c(dat_smooth_Jitter$y, plogis(grid_tmp_lt5y[,"Mean_24m"]))
	
	# Which country?
	dat_smooth_Jitter$z <- c(dat_smooth_Jitter$z, rep(codes$Country[i], times=nrow(grid_tmp_lt5y)))
	
}

dat_smooth_Jitter <- as.data.frame(dat_smooth_Jitter)
dat_smooth_Jitter$zz <- codes$Country[match(dat_smooth_Jitter$z, rownames(codes))]
save(dat_smooth_Jitter, file="./Results/GAM_plot/All_countries_Jitter_Comparison.RData")

# Compare old vs. new points for each country: weights
# At each DHS cluster

dat_smooth_Weights <- NULL
for(i in 1:nrow(codes)){

	# Load pre-processed data
	load(paste0("./Data/Manipulated_data/", codes$DHS_code[i], ".RData"))

	# Remove places where long/lat are NA
	dataClust <- dataClust[which(!is.na(dataClust$lat)),]

	# [Mozambique] has [2] clusters with latlong=c(0,0), so remove these
	dataClust <- dataClust[which(dataClust$long>1),]
	
	# Load the GAM ouput I
	load(paste0("./Results/GAM_fit_phenom/", codes$DHS_code[which(codes$Country==codes$Country[i])], ".RData"))

	# Predict using these
	newd <- data.frame(long=dataClust$long, lat=dataClust$lat, 
						   binned_survey_age=24,
						   SIA_1=0, SIA_2=0, SIA_3=0, SIA_4=0, SIA_5=0, SIA_6=0,
						   SIA_7=0, SIA_8=0, SIA_9=0, SIA_10=0, SIA_11=0, SIA_12=0,
						   SIA_13=0, SIA_14=0, SIA_15=0, SIA_16=0, SIA_17=0,
						   SIA_18=0, SIA_19=0, SIA_20=0, SIA_21=0, SIA_22=0)	

	# Predict there
	pred_1 <- predict.gam(fit_phenom, newd, se.fit=TRUE)
	
	# Save the data
	dat_smooth_Weights$x <- c(dat_smooth_Weights$x, plogis(pred_1$fit))
	rm(fit_phenom)
	
	# Load the GAM ouput II
	load(paste0("./Results/GAM_fit_phenom_JitterAndWeights/", codes$DHS_code[which(codes$Country==codes$Country[i])], "_Jitter.RData"))

	# Predict using these
	newd <- data.frame(long_jittered=dataClust$long, lat_jittered=dataClust$lat, 
						   binned_survey_age=24,
						   SIA_1=0, SIA_2=0, SIA_3=0, SIA_4=0, SIA_5=0, SIA_6=0,
						   SIA_7=0, SIA_8=0, SIA_9=0, SIA_10=0, SIA_11=0, SIA_12=0,
						   SIA_13=0, SIA_14=0, SIA_15=0, SIA_16=0, SIA_17=0,
						   SIA_18=0, SIA_19=0, SIA_20=0, SIA_21=0, SIA_22=0)	
	
	# Predict there
	pred_2 <- predict.gam(fit_phenom, newd, se.fit=TRUE)
	
	# Save the data
	dat_smooth_Weights$y <- c(dat_smooth_Weights$y, plogis(pred_2$fit))
		
	# Which country?
	dat_smooth_Weights$z <- c(dat_smooth_Weights$z, rep(codes$Country[i], times=length(pred_2$fit)))
	
}

dat_smooth_Weights <- as.data.frame(dat_smooth_Weights)
dat_smooth_Weights$zz <- codes$Country[match(dat_smooth_Weights$z, rownames(codes))]
save(dat_smooth_Weights, file="./Results/GAM_plot/All_countries_Weights_Comparison.RData")

################################################################################