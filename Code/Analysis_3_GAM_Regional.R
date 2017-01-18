library(gpclib)
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
library(raster)
library(plyr)
library(xlsx)

# Source code for analysis
source("./Code/FunctionsForAnalysis.R")

# Read in the master list of country codes
codes <- as.data.frame(read.csv("./Data/Master_List.csv"))

# Re-name years, as necessary
levels(codes$Year)[levels(codes$Year)=="2010-2011"] <- 2010
levels(codes$Year)[levels(codes$Year)=="2013-2014"] <- 2013

################################################################################

# Make a clean data frame
dat_all <- NULL

# Loop over the countries
for(i in 1:nrow(codes)) {

	# Load the manipulated data
	load(paste0("./Data/Manipulated_data_for_GAM/", codes$DHS_code[i], ".RData"))
	
	# Append to dat_all
	dat_all$measles_status <- c(dat_all$measles_status, dataInd_country_subnational$measles_status)
	dat_all$long <- c(dat_all$long, dataInd_country_subnational$long)
	dat_all$lat <- c(dat_all$lat, dataInd_country_subnational$lat)
	dat_all$binned_survey_age <- c(dat_all$binned_survey_age, dataInd_country_subnational$binned_survey_age)
	dat_all$year <- c(dat_all$year, rep(as.character(codes$Year[i]), times=length(dataInd_country_subnational$measles_status)))
	dat_all$country <- c(dat_all$country, rep(as.character(codes$Country[i]), times=length(dataInd_country_subnational$measles_status)))
	
}

# Save as data frame
dat_all_df <- as.data.frame(dat_all)
	
# Loop over the countries again to get the sub-national SIAs
which_SIA <- 1

for(i in 1:nrow(codes)) {

	# Load the manipulated data
	load(paste0("./Data/Manipulated_data_for_GAM/", codes$DHS_code[i], ".RData"))
	
	how_many_subnationals <- length(which_SIAs_subnational)
	
	if(how_many_subnationals > 0) {
	
		for(j in 1:how_many_subnationals) {
		
			dat_all_df[, paste0("SIA_", which_SIA)] <- 0
			dat_all_df[which(dat_all_df$country==as.character(codes$Country[i])), paste0("SIA_", which_SIA)] <- dataInd_country_subnational[,(12+j)]
			which_SIA <- which_SIA + 1
		
		}
	
	}

}

# Write out the call for the phenomenological model: sub-national SIAs only
func_name <- "measles_status ~ -1 + as.factor(country) + s(long,lat) + s(binned_survey_age) + SIA_1 + SIA_2 + SIA_3 + SIA_4 + SIA_5 + SIA_6 + SIA_7 + SIA_8 + SIA_9 + SIA_10 + SIA_11 + SIA_12 + SIA_13 + SIA_14 + SIA_15 + SIA_16 + SIA_17 + SIA_18 + SIA_19 + SIA_20 + SIA_21 + SIA_22 + SIA_23 + SIA_24"
	
# Fit the phenomenological model
fit_phenom <- do.call(gam, list(as.formula(func_name), 
					 family=binomial(link="logit"),
					 data=dat_all_df))
						
save(dat_all_df, func_name, fit_phenom, file="./Results/GAM_fit_phenom_Regional/Regional.RData")

################################################################################