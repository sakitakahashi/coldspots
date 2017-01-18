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

# Fit the GAM model for each country

for(i in 1:nrow(codes)){

	# Load the manipulated data
	load(paste0("./Data/Manipulated_data_for_GAM/", codes$DHS_code[i], ".RData"))

	# Write out the call for the phenomenological model: sub-national SIAs only
	func_name <- paste0("measles_status ~ s(long,lat)+s(binned_survey_age)+", paste(which_SIAs_subnational, collapse="+"))
	
	if(length(which_SIAs_subnational)==0) func_name <- paste0("measles_status ~ s(long,lat)+s(binned_survey_age)")
		
	# Fit the phenomenological model
	fit_phenom <- do.call(gam, list(as.formula(func_name), 
						 family=binomial(link="logit"),
						 data=dataInd_country_subnational))
						
	save(fit_phenom, file=paste0("./Results/GAM_fit_phenom/", codes$DHS_code[which(codes$Country==codes$Country[i])], ".RData"))
	
}	

################################################################################

# Model selection (AICs for 6 comparison models)

aic_table <- NULL

for(i in 1:nrow(codes)){

	# Load the manipulated data
	load(paste0("./Data/Manipulated_data_for_GAM/", codes$DHS_code[i], ".RData"))

	# Full model
	# Write out the call for the phenomenological model: sub-national SIAs only
	func_name <- paste0("measles_status ~ s(long,lat)+s(binned_survey_age)+", paste(which_SIAs_subnational, collapse="+"))
	
	if(length(which_SIAs_subnational)==0) func_name <- paste0("measles_status ~ s(long,lat)+s(binned_survey_age)")

	# Fit the phenomenological model ('full')
	fit_phenom <- do.call(gam, list(as.formula(func_name), 
						 family=binomial(link="logit"),
						 data=dataInd_country_subnational))				
	aic_full <- (fit_phenom$aic)
	
	# Comparison model 1
	func_name1 <- paste0("measles_status ~ s(long,lat)+s(binned_survey_age)")
	fit_phenom1 <- do.call(gam, list(as.formula(func_name1), 
						 family=binomial(link="logit"),
						 data=dataInd_country_subnational))	
	aic_1 <- (fit_phenom1$aic)
	
	# Comparison model 2
	func_name2 <- paste0("measles_status ~ s(long,lat)+", paste(which_SIAs_subnational, collapse="+"))
	if(length(which_SIAs_subnational)==0) func_name2 <- paste0("measles_status ~ s(long,lat)")	
	fit_phenom2 <- do.call(gam, list(as.formula(func_name2), 
						 family=binomial(link="logit"),
						 data=dataInd_country_subnational))	
	aic_2 <- (fit_phenom2$aic)	

	# Comparison model 3
	func_name3 <- paste0("measles_status ~ s(long,lat)")
	fit_phenom3 <- do.call(gam, list(as.formula(func_name3), 
						 family=binomial(link="logit"),
						 data=dataInd_country_subnational))	
	aic_3 <- (fit_phenom3$aic)	
	
	# Comparison model 4
	func_name4 <- paste0("measles_status ~ s(binned_survey_age)+", paste(which_SIAs_subnational, collapse="+"))
	if(length(which_SIAs_subnational)==0) func_name4 <- paste0("measles_status ~ s(binned_survey_age)")	
	fit_phenom4 <- do.call(gam, list(as.formula(func_name4), 
						 family=binomial(link="logit"),
						 data=dataInd_country_subnational))		
	aic_4 <- (fit_phenom4$aic)
	
	# Comparison model 5
	func_name5 <- paste0("measles_status ~ s(binned_survey_age)")
	fit_phenom5 <- do.call(gam, list(as.formula(func_name5), 
						 family=binomial(link="logit"),
						 data=dataInd_country_subnational))		
	aic_5 <- (fit_phenom5$aic)
	
	# Comparison model 6
	func_name6 <- paste0("measles_status ~ ", paste(which_SIAs_subnational, collapse="+"))
	if(length(which_SIAs_subnational)==0) func_name6 <- paste0("measles_status ~ 1")	
	fit_phenom6 <- do.call(gam, list(as.formula(func_name6), 
						 family=binomial(link="logit"),
						 data=dataInd_country_subnational))		
	aic_6 <- (fit_phenom6$aic)	
	
	# Populate the table
	aic_table$country[i] <- as.character(codes$Country[i])
	aic_table$aic_full[i] <- aic_full
	aic_table$aic_1[i] <- aic_1
	aic_table$aic_2[i] <- aic_2
	aic_table$aic_3[i] <- aic_3
	aic_table$aic_4[i] <- aic_4
	aic_table$aic_5[i] <- aic_5
	aic_table$aic_6[i] <- aic_6

}	

aic_table_df <- as.data.frame(aic_table)
aic_table_df[,2:8] <- round(aic_table_df[,2:8], 2)

# Get the row mins
# apply(aic_table_df[,2:8], 1, min)

write.csv(aic_table_df, file="./Results/Tables/AIC.csv")

################################################################################

# Get GAM smoothed age parameters for each country

dat_smooth <- NULL
for(i in 1:nrow(codes)){

	# Load the GAM ouput
	load(paste0("./Results/GAM_fit_phenom/", codes$DHS_code[which(codes$Country==codes$Country[i])], ".RData"))

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
save(dat_smooth, file="./Results/GAM_plot/All_countries.RData")

################################################################################