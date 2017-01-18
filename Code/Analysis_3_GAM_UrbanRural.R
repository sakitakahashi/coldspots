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

################################################################################

# Fit the GAM model for each country with the urban_rural covariate

for(i in 1:nrow(codes)){

	# Load the manipulated data
	load(paste0("./Data/Manipulated_data_for_GAM_urbanrural/", codes$DHS_code[i], ".RData"))

	# Write out the call for the phenomenological model: sub-national SIAs only
	func_name <- paste0("measles_status ~ as.factor(urban_rural)+s(long,lat)+s(binned_survey_age)+", paste(which_SIAs_subnational, collapse="+"))
	
	if(length(which_SIAs_subnational)==0) func_name <- paste0("measles_status ~ as.factor(urban_rural)+s(long,lat)+s(binned_survey_age)")

	# Fit the phenomenological model
	fit_phenom <- do.call(gam, list(as.formula(func_name), 
						 family=binomial(link="logit"),
						 data=dataInd_country_subnational))
						
	save(fit_phenom, file=paste0("./Results/GAM_fit_phenom_UrbanRural/", codes$DHS_code[which(codes$Country==codes$Country[i])], ".RData"))
	
}

################################################################################

# Model selection (AICs for full vs. urban_rural model)

aic_table <- NULL

for(i in 1:nrow(codes)){

	# Load the manipulated data
	load(paste0("./Data/Manipulated_data_for_GAM/", codes$DHS_code[i], ".RData"))

	# Full model
	# Write out the call for the phenomenological model: sub-national SIAs only
	func_name <- paste0("measles_status ~ s(long,lat)+s(binned_survey_age)+", paste(which_SIAs_subnational, collapse="+"))
	
	if(length(which_SIAs_subnational)==0) func_name <- paste0("measles_status ~ s(long,lat)+s(binned_survey_age)")
	
	# Fit the phenomenological model
	fit_phenom <- do.call(gam, list(as.formula(func_name), 
						 family=binomial(link="logit"),
						 data=dataInd_country_subnational))
						
	aic_full <- (fit_phenom$aic)
	rm(dataInd_country_subnational)
	
	# Comparison model: including urban_rural
	# Load the manipulated data
	load(paste0("./Data/Manipulated_data_for_GAM_urbanrural/", codes$DHS_code[i], ".RData"))

	# Write out the call for the phenomenological model: sub-national SIAs only
	func_name <- paste0("measles_status ~ as.factor(urban_rural)+s(long,lat)+s(binned_survey_age)+", paste(which_SIAs_subnational, collapse="+"))
	
	if(length(which_SIAs_subnational)==0) func_name <- paste0("measles_status ~ as.factor(urban_rural)+s(long,lat)+s(binned_survey_age)")
		
	# Fit the phenomenological model
	fit_phenom_UR <- do.call(gam, list(as.formula(func_name), 
						 family=binomial(link="logit"),
						 data=dataInd_country_subnational))
						
	aic_UR <- (fit_phenom_UR$aic)
		
	# Populate the table
	aic_table$country[i] <- as.character(codes$Country[i])
	aic_table$aic_full[i] <- aic_full
	aic_table$aic_UR[i] <- aic_UR
	
}	

aic_table_df <- as.data.frame(aic_table)
aic_table_df[,2:3] <- round(aic_table_df[,2:3], 2)

# Get the row mins
# apply(aic_table_df[,2:3], 1, min)

write.csv(aic_table_df, file="./Results/Tables/AIC_UrbanRural.csv")

################################################################################