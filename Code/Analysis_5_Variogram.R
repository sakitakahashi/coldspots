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
library(dplyr)

# Source code for analysis
source("./Code/Functions_For_Analysis.R")

# Read in the master list of country codes
codes <- as.data.frame(read.csv("./Data/Master_List.csv"))

################################################################################

# GAM residual analysis for each country

for(i in 1:nrow(codes)){

	# Load the manipulated data for GAM
	load(paste0("./Data/Manipulated_data_for_GAM/", codes$DHS_code[i], ".RData"))

	# Load the GAM ouput
	load(paste0("./Results/GAM_fit_phenom/", codes$DHS_code[which(codes$Country==codes$Country[i])], ".RData"))
	
	# Set up spatial data points in long/lat
	coords <- matrix(NA, nrow(dataInd_country_subnational), 2)
	coords[,1] <- dataInd_country_subnational$long
	coords[,2] <- dataInd_country_subnational$lat
	coords <- data.frame(X=coords[,1], Y=coords[,2])
	
	# Convert spatial data points to UTM
	sp::coordinates(coords) <- c("X", "Y")
	sp::proj4string(coords) <- CRS("+proj=longlat +datum=WGS84")
	coords_tr <- as.data.frame(sp::spTransform(coords, CRS(paste("+proj=utm +zone=", codes$UTM[i]," ellps=WGS84", sep=''))))

	# Get the maximum distance between two unique spatial points (in km)
	unique_coords <- unique(coords_tr)
	rownames(unique_coords) <- 1:nrow(unique_coords)
	max_dist_meters <- max(dist(unique_coords))
	
	# Variogram of the residuals, maximum distance is prop_of_max of the max distance between points, spaced at interval kilometers	
	gb <- list(data=residuals(fit_phenom, type="pearson"), coords=coords_tr)
	
	save(gb, coords_tr, unique_coords, max_dist_meters, file=paste0("./Data/Manipulated_data_for_Variogram/", codes$DHS_code[i], ".RData"))
	
}

################################################################################