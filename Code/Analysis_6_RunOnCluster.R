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

## Source code for analysis
source("./Code/FunctionsForAnalysis.R")

## Read in the master list of country codes
codes <- as.data.frame(read.csv("./Data/Master_List.csv"))

## If only Burundi and Tanzania
codes_BT <- codes[c(1,7),]
codes_BT$Country <- factor(codes_BT$Country)
rownames(codes_BT) <- 1:nrow(codes_BT)

## Which country? (for job arrays)
index <- as.integer(commandArgs()[length(commandArgs())])
	
################################################################################

## Note: the code below takes too much computation time to run on a single computer
## Currently organized to run on a cluster ("Della" at Princeton)

## ./Results/GAMtoMeanAndSE_Della
## Mean and SE of GAM output between [lower_age, upper_age] months of age at each grid cell
## Output: one file per country

getGAM_Map_noSubSIA <- function(lower_age, upper_age, i) {

	# Load the GAM ouput
	load(paste0("./Results/GAM_fit_phenom/", codes$DHS_code[i], ".RData"))

	# Get a data.frame of grid cells over the country at a certain grid size
	load(paste0("./Results/Imputed_WorldPop/", codes$DHS_code[i], ".RData"))

	# Get border lines to add
	adm0 <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[i], "_adm/", codes$ADM_code[i], "_adm0.shp"))
	
	# For each age
	for(j in lower_age:upper_age) {

		# Predict for certain values of covariates
		newd <- data.frame(long=grid_tmp_lt5y$long, lat=grid_tmp_lt5y$lat, 
						   binned_survey_age=j,
						   SIA_1=0, SIA_2=0, SIA_3=0, SIA_4=0, SIA_5=0, SIA_6=0,
						   SIA_7=0, SIA_8=0, SIA_9=0, SIA_10=0, SIA_11=0, SIA_12=0,
						   SIA_13=0, SIA_14=0, SIA_15=0, SIA_16=0, SIA_17=0,
						   SIA_18=0, SIA_19=0, SIA_20=0, SIA_21=0, SIA_22=0)	

		# Predict at grid cells
		pred <- predict.gam(fit_phenom, newd, se.fit=TRUE)
		
		# Get the Mean and SE of prediction
		grid_tmp_lt5y[,paste0("Mean_", j, "m")] <- pred$fit
		grid_tmp_lt5y[,paste0("SE_", j, "m")] <- pred$se.fit
		
	}
	
	# Convert the data points to RasterLayer --> SPDF for manipulation
	g <- rasterFromXYZ(grid_tmp_lt5y)
	g <- as(g, "SpatialPolygonsDataFrame")

	# Get the intersection of the SPDF and adm0 border, which produces a SpatialPolygons
	g <- rgeos::gIntersection(g, adm0, byid=TRUE)		

	# Convert back to SPDF
	g <- as(g, "SpatialPolygonsDataFrame")
	g@data$id <- rownames(g@data)
	
	# Add all the data back 	
	data_to_add <- cbind(grid_tmp_lt5y[,paste0("Mean_", lower_age:upper_age, "m")], grid_tmp_lt5y[,paste0("SE_", lower_age:upper_age, "m")])
	rownames(data_to_add) <- g@data$id
	g <- spCbind(g, data_to_add)
	
	# Save the SPDF as a data.frame to plot
	g@data$id <- rownames(g@data)
	g_points <- ggplot2::fortify(g, region="id")	
	g_df <- plyr::join(g_points, g@data, by="id")

	# Save the .RData
	save(grid_tmp_lt5y, g, g_df, file=paste0("./Results/GAMtoMeanAndSE_Della/GAMtoMeanAndSE_", codes$DHS_code[i], "_", lower_age, "-", upper_age, "m", ".RData"))

}

getGAM_Map_noSubSIA(lower_age=6, upper_age=60, i=index)

## ./Results/GAMtoMeanAndSE_Della_JitterAndWeights
## Mean and SE of GAM output between [lower_age, upper_age] months of age at each grid cell, with jittering/weights
## Output: one file per country, for jitter and for weights

getGAM_Map_noSubSIA_Jitter <- function(lower_age, upper_age, i) {

	# Load the GAM ouput
	load(paste0("./Results/GAM_fit_phenom_JitterAndWeights/", codes$DHS_code[i], "_Jitter.RData"))

	# Get a data.frame of grid cells over the country at a certain grid size
	load(paste0("./Results/Imputed_WorldPop/", codes$DHS_code[i], ".RData"))

	# Get border lines to add
	adm0 <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[i], "_adm/", codes$ADM_code[i], "_adm0.shp"))
	
	# For each age
	for(j in lower_age:upper_age) {

		# Predict for certain values of covariates
		newd <- data.frame(long_jittered=grid_tmp_lt5y$long, lat_jittered=grid_tmp_lt5y$lat, 
						   binned_survey_age=j,
						   SIA_1=0, SIA_2=0, SIA_3=0, SIA_4=0, SIA_5=0, SIA_6=0,
						   SIA_7=0, SIA_8=0, SIA_9=0, SIA_10=0, SIA_11=0, SIA_12=0,
						   SIA_13=0, SIA_14=0, SIA_15=0, SIA_16=0, SIA_17=0,
						   SIA_18=0, SIA_19=0, SIA_20=0, SIA_21=0, SIA_22=0)	

		# Predict at grid cells
		pred <- predict.gam(fit_phenom, newd, se.fit=TRUE)
		
		# Get the Mean and SE of prediction
		grid_tmp_lt5y[,paste0("Mean_", j, "m")] <- pred$fit
		grid_tmp_lt5y[,paste0("SE_", j, "m")] <- pred$se.fit
		
	}
	
	# Convert the data points to RasterLayer --> SPDF for manipulation
	g <- rasterFromXYZ(grid_tmp_lt5y)
	g <- as(g, "SpatialPolygonsDataFrame")

	# Get the intersection of the SPDF and adm0 border, which produces a SpatialPolygons
	g <- rgeos::gIntersection(g, adm0, byid=TRUE)		

	# Convert back to SPDF
	g <- as(g, "SpatialPolygonsDataFrame")
	g@data$id <- rownames(g@data)
	
	# Add all the data back 	
	data_to_add <- cbind(grid_tmp_lt5y[,paste0("Mean_", lower_age:upper_age, "m")], grid_tmp_lt5y[,paste0("SE_", lower_age:upper_age, "m")])
	rownames(data_to_add) <- g@data$id
	g <- spCbind(g, data_to_add)
	
	# Save the SPDF as a data.frame to plot
	g@data$id <- rownames(g@data)
	g_points <- ggplot2::fortify(g, region="id")	
	g_df <- plyr::join(g_points, g@data, by="id")

	# Save the .RData
	save(grid_tmp_lt5y, g, g_df, file=paste0("./Results/GAMtoMeanAndSE_Della_JitterAndWeights/GAMtoMeanAndSE_", codes$DHS_code[i], "_", lower_age, "-", upper_age, "m", "_Jitter.RData"))

}

getGAM_Map_noSubSIA_Jitter(lower_age=6, upper_age=60, i=index)

getGAM_Map_noSubSIA_Weights <- function(lower_age, upper_age, i) {

	# Load the GAM ouput
	load(paste0("./Results/GAM_fit_phenom_JitterAndWeights/", codes$DHS_code[i], "_Weights.RData"))

	# Get a data.frame of grid cells over the country at a certain grid size
	load(paste0("./Results/Imputed_WorldPop/", codes$DHS_code[i], ".RData"))

	# Get border lines to add
	adm0 <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[i], "_adm/", codes$ADM_code[i], "_adm0.shp"))
	
	# For each age
	for(j in lower_age:upper_age) {

		# Predict for certain values of covariates
		newd <- data.frame(long=grid_tmp_lt5y$long, lat=grid_tmp_lt5y$lat, 
						   binned_survey_age=j,
						   SIA_1=0, SIA_2=0, SIA_3=0, SIA_4=0, SIA_5=0, SIA_6=0,
						   SIA_7=0, SIA_8=0, SIA_9=0, SIA_10=0, SIA_11=0, SIA_12=0,
						   SIA_13=0, SIA_14=0, SIA_15=0, SIA_16=0, SIA_17=0,
						   SIA_18=0, SIA_19=0, SIA_20=0, SIA_21=0, SIA_22=0)	

		# Predict at grid cells
		pred <- predict.gam(fit_phenom, newd, se.fit=TRUE)
		
		# Get the Mean and SE of prediction
		grid_tmp_lt5y[,paste0("Mean_", j, "m")] <- pred$fit
		grid_tmp_lt5y[,paste0("SE_", j, "m")] <- pred$se.fit
		
	}
	
	# Convert the data points to RasterLayer --> SPDF for manipulation
	g <- rasterFromXYZ(grid_tmp_lt5y)
	g <- as(g, "SpatialPolygonsDataFrame")

	# Get the intersection of the SPDF and adm0 border, which produces a SpatialPolygons
	g <- rgeos::gIntersection(g, adm0, byid=TRUE)		

	# Convert back to SPDF
	g <- as(g, "SpatialPolygonsDataFrame")
	g@data$id <- rownames(g@data)
	
	# Add all the data back 	
	data_to_add <- cbind(grid_tmp_lt5y[,paste0("Mean_", lower_age:upper_age, "m")], grid_tmp_lt5y[,paste0("SE_", lower_age:upper_age, "m")])
	rownames(data_to_add) <- g@data$id
	g <- spCbind(g, data_to_add)
	
	# Save the SPDF as a data.frame to plot
	g@data$id <- rownames(g@data)
	g_points <- ggplot2::fortify(g, region="id")	
	g_df <- plyr::join(g_points, g@data, by="id")

	# Save the .RData
	save(grid_tmp_lt5y, g, g_df, file=paste0("./Results/GAMtoMeanAndSE_Della_JitterAndWeights/GAMtoMeanAndSE_", codes$DHS_code[i], "_", lower_age, "-", upper_age, "m", "_Weights.RData"))

}

getGAM_Map_noSubSIA_Weights(lower_age=6, upper_age=60, i=index)

## ./Results/GAMtoMeanAndSE_Della_NoSubnationals
## Mean and SE of GAM output between [lower_age, upper_age] months of age at each grid cell, with no sub-national SIAs in the model
## Output: one file each for Burundi and Tanzania

getGAM_Map_noSubSIA_InModel <- function(codes_sub, lower_age, upper_age, i) {

	# Which countries?
	codes <- codes_sub
	
	# Load the GAM ouput
	load(paste0("./Results/GAM_fit_phenom_NoSubnationals/", codes$DHS_code[i], "_NoSubnationals.RData"))

	# Get a data.frame of grid cells over the country at a certain grid size
	load(paste0("./Results/Imputed_WorldPop/", codes$DHS_code[i], ".RData"))

	# Get border lines to add
	adm0 <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[i], "_adm/", codes$ADM_code[i], "_adm0.shp"))
	
	# For each age
	for(j in lower_age:upper_age) {

		# Predict for certain values of covariates
		newd <- data.frame(long=grid_tmp_lt5y$long, lat=grid_tmp_lt5y$lat, 
						   binned_survey_age=j)	

		# Predict at grid cells
		pred <- predict.gam(fit_phenom, newd, se.fit=TRUE)
		
		# Get the Mean and SE of prediction
		grid_tmp_lt5y[,paste0("Mean_", j, "m")] <- pred$fit
		grid_tmp_lt5y[,paste0("SE_", j, "m")] <- pred$se.fit
		
	}
	
	# Convert the data points to RasterLayer --> SPDF for manipulation
	g <- rasterFromXYZ(grid_tmp_lt5y)
	g <- as(g, "SpatialPolygonsDataFrame")

	# Get the intersection of the SPDF and adm0 border, which produces a SpatialPolygons
	g <- rgeos::gIntersection(g, adm0, byid=TRUE)		

	# Convert back to SPDF
	g <- as(g, "SpatialPolygonsDataFrame")
	g@data$id <- rownames(g@data)
	
	# Add all the data back 	
	data_to_add <- cbind(grid_tmp_lt5y[,paste0("Mean_", lower_age:upper_age, "m")], grid_tmp_lt5y[,paste0("SE_", lower_age:upper_age, "m")])
	rownames(data_to_add) <- g@data$id
	g <- spCbind(g, data_to_add)

	# Save the SPDF as a data.frame to plot
	g@data$id <- rownames(g@data)
	g_points <- ggplot2::fortify(g, region="id")	
	g_df <- plyr::join(g_points, g@data, by="id")

	# Save the .RData
	save(grid_tmp_lt5y, g, g_df, file=paste0("./Results/GAMtoMeanAndSE_Della_NoSubnationals/GAMtoMeanAndSE_", codes$DHS_code[i], "_", lower_age, "-", upper_age, "m", "_NoSubnationals.RData"))

}

getGAM_Map_noSubSIA_InModel(codes_sub=codes_BT, lower_age=6, upper_age=60, i=index)

## ./Results/GAMtoMeanAndSE_Della_UrbanRural
## Mean and SE of GAM output between [lower_age, upper_age] months of age at each grid cell, for urban and for rural
## Output: one file per country, for urban and for rural

getGAM_Map_noSubSIA_Urban <- function(lower_age, upper_age, i) {

	# Load the GAM ouput
	load(paste0("./Results/GAM_fit_phenom_UrbanRural/", codes$DHS_code[i], ".RData"))

	# Get a data.frame of grid cells over the country at a certain grid size
	load(paste0("./Results/Imputed_WorldPop/", codes$DHS_code[i], ".RData"))

	# Get border lines to add
	adm0 <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[i], "_adm/", codes$ADM_code[i], "_adm0.shp"))
	
	# For each age
	for(j in lower_age:upper_age) {

		# Predict for certain values of covariates
		newd <- data.frame(long=grid_tmp_lt5y$long, lat=grid_tmp_lt5y$lat, 
						   binned_survey_age=j,
						   urban_rural="U",
						   SIA_1=0, SIA_2=0, SIA_3=0, SIA_4=0, SIA_5=0, SIA_6=0,
						   SIA_7=0, SIA_8=0, SIA_9=0, SIA_10=0, SIA_11=0, SIA_12=0,
						   SIA_13=0, SIA_14=0, SIA_15=0, SIA_16=0, SIA_17=0,
						   SIA_18=0, SIA_19=0, SIA_20=0, SIA_21=0, SIA_22=0)	

		# Predict at grid cells
		pred <- predict.gam(fit_phenom, newd, se.fit=TRUE)
		
		# Get the Mean and SE of prediction
		grid_tmp_lt5y[,paste0("Mean_", j, "m")] <- pred$fit
		grid_tmp_lt5y[,paste0("SE_", j, "m")] <- pred$se.fit
		
	}
	
	# Convert the data points to RasterLayer --> SPDF for manipulation
	g <- rasterFromXYZ(grid_tmp_lt5y)
	g <- as(g, "SpatialPolygonsDataFrame")

	# Get the intersection of the SPDF and adm0 border, which produces a SpatialPolygons
	g <- rgeos::gIntersection(g, adm0, byid=TRUE)		

	# Convert back to SPDF
	g <- as(g, "SpatialPolygonsDataFrame")
	g@data$id <- rownames(g@data)
	
	# Add all the data back 	
	data_to_add <- cbind(grid_tmp_lt5y[,paste0("Mean_", lower_age:upper_age, "m")], grid_tmp_lt5y[,paste0("SE_", lower_age:upper_age, "m")])
	rownames(data_to_add) <- g@data$id
	g <- spCbind(g, data_to_add)
	
	# Save the SPDF as a data.frame to plot
	g@data$id <- rownames(g@data)
	g_points <- ggplot2::fortify(g, region="id")	
	g_df <- plyr::join(g_points, g@data, by="id")

	# Save the .RData
	save(grid_tmp_lt5y, g, g_df, file=paste0("./Results/GAMtoMeanAndSE_Della_UrbanRural/GAMtoMeanAndSE_", codes$DHS_code[i], "_", lower_age, "-", upper_age, "m", "_Urban.RData"))

}

getGAM_Map_noSubSIA_Urban(lower_age=6, upper_age=60, i=index)

getGAM_Map_noSubSIA_Rural <- function(lower_age, upper_age, i) {

	# Load the GAM ouput
	load(paste0("./Results/GAM_fit_phenom_UrbanRural/", codes$DHS_code[i], ".RData"))

	# Get a data.frame of grid cells over the country at a certain grid size
	load(paste0("./Results/Imputed_WorldPop/", codes$DHS_code[i], ".RData"))

	# Get border lines to add
	adm0 <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[i], "_adm/", codes$ADM_code[i], "_adm0.shp"))
	
	# For each age
	for(j in lower_age:upper_age) {

		# Predict for certain values of covariates
		newd <- data.frame(long=grid_tmp_lt5y$long, lat=grid_tmp_lt5y$lat, 
						   binned_survey_age=j,
						   urban_rural="R",
						   SIA_1=0, SIA_2=0, SIA_3=0, SIA_4=0, SIA_5=0, SIA_6=0,
						   SIA_7=0, SIA_8=0, SIA_9=0, SIA_10=0, SIA_11=0, SIA_12=0,
						   SIA_13=0, SIA_14=0, SIA_15=0, SIA_16=0, SIA_17=0,
						   SIA_18=0, SIA_19=0, SIA_20=0, SIA_21=0, SIA_22=0)	

		# Predict at grid cells
		pred <- predict.gam(fit_phenom, newd, se.fit=TRUE)
		
		# Get the Mean and SE of prediction
		grid_tmp_lt5y[,paste0("Mean_", j, "m")] <- pred$fit
		grid_tmp_lt5y[,paste0("SE_", j, "m")] <- pred$se.fit
		
	}
	
	# Convert the data points to RasterLayer --> SPDF for manipulation
	g <- rasterFromXYZ(grid_tmp_lt5y)
	g <- as(g, "SpatialPolygonsDataFrame")

	# Get the intersection of the SPDF and adm0 border, which produces a SpatialPolygons
	g <- rgeos::gIntersection(g, adm0, byid=TRUE)		

	# Convert back to SPDF
	g <- as(g, "SpatialPolygonsDataFrame")
	g@data$id <- rownames(g@data)
	
	# Add all the data back 	
	data_to_add <- cbind(grid_tmp_lt5y[,paste0("Mean_", lower_age:upper_age, "m")], grid_tmp_lt5y[,paste0("SE_", lower_age:upper_age, "m")])
	rownames(data_to_add) <- g@data$id
	g <- spCbind(g, data_to_add)
	
	# Save the SPDF as a data.frame to plot
	g@data$id <- rownames(g@data)
	g_points <- ggplot2::fortify(g, region="id")	
	g_df <- plyr::join(g_points, g@data, by="id")

	# Save the .RData
	save(grid_tmp_lt5y, g, g_df, file=paste0("./Results/GAMtoMeanAndSE_Della_UrbanRural/GAMtoMeanAndSE_", codes$DHS_code[i], "_", lower_age, "-", upper_age, "m", "_Rural.RData"))

}

getGAM_Map_noSubSIA_Rural(lower_age=6, upper_age=60, i=index)

## ./Results/GAMtoMeanAndSE_Della_WithSubnationals
## Mean and SE of GAM output between [lower_age, upper_age] months of age at each grid cell, including impact of sub-national SIAs
## Output: one file each for Burundi, DRC, and Tanzania

getGAM_Map_WithSubSIA_Burundi <- function(lower_age, upper_age) {

	# Which country?
	i <- 1
	
	# Load the SIA data 1
	load(paste0("./Data/Manipulated_data/", as.character(codes$DHS_code[i]), ".RData"))

	# Load the SIA data 2
	load(paste0("./Data/Manipulated_data_for_GAM/", as.character(codes$DHS_code[i]), ".RData"))

	# Load the GAM ouput
	load(paste0("./Results/GAM_fit_phenom/", codes$DHS_code[i], ".RData"))

	# Get a data.frame of grid cells over the country at a certain grid size
	load(paste0("./Results/Imputed_WorldPop/", codes$DHS_code[i], ".RData"))

	# Get border lines to add
	adm0 <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[i], "_adm/", codes$ADM_code[i], "_adm0.shp"))
	
	## Double check: what region type is the SIAs on?
	# print(as.numeric(codes$SIA_region_type[i]))
	# print(get(paste0("region", as.numeric(codes$SIA_region_type[i]), "_key")))
	
	## Should be the same as above
	# print(as.numeric(codes$analysis_region_type[i]))
	# print(get(paste0("region", as.numeric(codes$analysis_region_type[i]), "_key")))

	# Get the map data for province plotting
	adm1 <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[i], "_adm/", codes$ADM_code[i], "_adm1.shp"))
	adm1@data$id <- rownames(adm1@data)

	# Centroids of province for ID
	unifiedPolygons <- unionSpatialPolygons(adm1, adm1@data$id)
	centroids <- as.data.frame(coordinates(unifiedPolygons))
	names(centroids) <- c("long", "lat")
	centroids$name <- as.numeric(rownames(centroids))
	
	# Loop over all the sub-national SIAs
	for(x in 1:length(which_SIAs_subnational)) {
		
		# Make an indicator for that location's eligibility for the SIA
		grid_tmp_lt5y[,paste0(which_SIAs_subnational[x], "_elig")] <- 0 
		
		# Which region2's (AKA adm1@data$id) are contained in that region1?
		region2_of_SIA <- c(1,2)
	
		# Loop over all the region2's in that region1
		for(j in 1:length(region2_of_SIA)) {
		
			# Get the DHS region ID
			which_DHS_region <- which(centroids$name==region2_of_SIA[j])
			region <- unifiedPolygons[which_DHS_region]
			
			# Buffer everything, because otherwise we miss some boundary cases
			region <- gBuffer(region, width=0.01)
				
			# Get the points on the country-level grid that are actually inside the polygon
			def <- inout(pts=as.points(grid_tmp_lt5y$long, grid_tmp_lt5y$lat), 
						 poly=region@polygons[[1]]@Polygons[[1]]@coords, 
						 bound=TRUE)
			
			# Plot the "in" points to double check
			plot(adm0, main=paste0(j, " of ", length(region2_of_SIA)))
			plot(unifiedPolygons[which_DHS_region], add=TRUE)
			plot(region, add=TRUE, lty=2)
			points(grid_tmp_lt5y$long[def], grid_tmp_lt5y$lat[def], col=3, cex=0.4)

			# Only keep those points that are inside that polygon
			grid_tmp_lt5y[def==TRUE,paste0(which_SIAs_subnational[x], "_elig")] <- 1

		}
	
	}
	
	# Get the age limits of each sub-national SIA
	load("./Results/eCDF/All_countries.RData")
	relevant_SIAs <- dat_SIA[which(dat_SIA$country==i),]	
	relevant_SIAs <- relevant_SIAs[5,]
	relevant_SIAs$lb <- floor(relevant_SIAs$lb)
	relevant_SIAs$ub <- ceiling(relevant_SIAs$ub)	
	
	# For each age
	for(k in lower_age:upper_age) {
	
		SIA_5 <- grid_tmp_lt5y$SIA_5_elig * ifelse(k >= min(relevant_SIAs[1,1:2]) && k <= max(relevant_SIAs[1,1:2]) && !is.infinite(relevant_SIAs[1,1]) && !is.infinite(relevant_SIAs[1,2]), 1, 0)
		
		# Predict for certain values of covariates
		newd <- data.frame(long=grid_tmp_lt5y$long, lat=grid_tmp_lt5y$lat, binned_survey_age=k, SIA_5=SIA_5)

		# Predict at grid cells
		pred <- predict.gam(fit_phenom, newd, se.fit=TRUE)
		
		# Get the Mean and SE of prediction
		grid_tmp_lt5y[,paste0("Mean_", k, "m")] <- pred$fit
		grid_tmp_lt5y[,paste0("SE_", k, "m")] <- pred$se.fit
		
	}
	
	# Convert the data points to RasterLayer --> SPDF for manipulation
	g <- rasterFromXYZ(grid_tmp_lt5y)
	g <- as(g, "SpatialPolygonsDataFrame")

	# Get the intersection of the SPDF and adm0 border, which produces a SpatialPolygons
	g <- rgeos::gIntersection(g, adm0, byid=TRUE)		

	# Convert back to SPDF
	g <- as(g, "SpatialPolygonsDataFrame")
	g@data$id <- rownames(g@data)
	
	# Add all the data back 	
	data_to_add <- cbind(grid_tmp_lt5y[,paste0("Mean_", lower_age:upper_age, "m")], grid_tmp_lt5y[,paste0("SE_", lower_age:upper_age, "m")])
	rownames(data_to_add) <- g@data$id
	g <- spCbind(g, data_to_add)
	
	# Save the SPDF as a data.frame to plot
	g@data$id <- rownames(g@data)
	g_points <- ggplot2::fortify(g, region="id")	
	g_df <- plyr::join(g_points, g@data, by="id")

	# Save the .RData
	save(grid_tmp_lt5y, g, g_df, file=paste0("./Results/GAMtoMeanAndSE_Della_WithSubnationals/GAMtoMeanAndSE_WithSubnationals_", codes$DHS_code[i], "_", lower_age, "-", upper_age, "m", ".RData"))

}

getGAM_Map_WithSubSIA_Burundi(lower_age=6, upper_age=60)

getGAM_Map_WithSubSIA_DRC <- function(lower_age, upper_age) {

	# Which country?
	i <- 2
	
	# Load the SIA data 1
	load(paste0("./Data/Manipulated_data/", as.character(codes$DHS_code[i]), ".RData"))

	# Load the SIA data 2
	load(paste0("./Data/Manipulated_data_for_GAM/", as.character(codes$DHS_code[i]), ".RData"))

	# Load the GAM ouput
	load(paste0("./Results/GAM_fit_phenom/", codes$DHS_code[i], ".RData"))

	# Get a data.frame of grid cells over the country at a certain grid size
	load(paste0("./Results/Imputed_WorldPop/", codes$DHS_code[i], ".RData"))

	# Get border lines to add
	adm0 <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[i], "_adm/", codes$ADM_code[i], "_adm0.shp"))
	
	## Double check: what region type is the SIAs on?
	# print(as.numeric(codes$SIA_region_type[i]))
	# print(get(paste0("region", as.numeric(codes$SIA_region_type[i]), "_key")))
	
	## For DRC: however, the adm2 data is on...
	# print(as.numeric(codes$analysis_region_type[i]))
	# print(get(paste0("region", as.numeric(codes$analysis_region_type[i]), "_key")))

	# Get the map data for province plotting
	adm2 <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[i], "_adm/", codes$ADM_code[i], "_adm2.shp"))
	adm2@data$DHS_id <- c(6,6,7,8,9,10,3,2,3,4,5,5,5,5,5,12,11,12,14,13,15,13,17,16,18,18,16,19,1,26,20,21,26,22,23,24,25,25) # Order by DHS region name
	adm2@data$id <- rownames(adm2@data)

	# Centroids of province for ID
	unifiedPolygons <- unionSpatialPolygons(adm2, adm2@data$DHS_id)
	centroids <- as.data.frame(coordinates(unifiedPolygons))
	names(centroids) <- c("long", "lat")
	centroids$name <- as.numeric(rownames(centroids))
	
	# Loop over all the sub-national SIAs
	for(x in 1:length(which_SIAs_subnational)) {
		
		# Make an indicator for that location's eligibility for the SIA
		grid_tmp_lt5y[,paste0(which_SIAs_subnational[x], "_elig")] <- 0 
		
		# Which region1 is the sub-national SIA is occurring?
		region1_of_SIA <- dataSIA_country$region1_name[dataSIA_country$Campaign_name==which_SIAs_subnational[x]]
		
		# Which region2's are contained in that region1?
		region2_of_SIA <- region2_key$region2_id[which(region2_key$region1_name==region1_of_SIA)]
	
		# Loop over all the region2's in that region1
		for(j in 1:length(region2_of_SIA)) {
		
			# Get the DHS region ID
			which_DHS_region <- which(centroids$name==region2_of_SIA[j])
			region <- unifiedPolygons[which_DHS_region]
			
			# Buffer everything, because otherwise we miss some boundary cases
			region <- gBuffer(region, width=0.01)
				
			# Get the points on the country-level grid that are actually inside the polygon
			def <- inout(pts=as.points(grid_tmp_lt5y$long, grid_tmp_lt5y$lat), 
						 poly=region@polygons[[1]]@Polygons[[1]]@coords, 
						 bound=TRUE)
			
			# Plot the "in" points to double check
			plot(adm0, main=paste0(j, " of ", length(region2_of_SIA)))
			plot(unifiedPolygons[which_DHS_region], add=TRUE)
			plot(region, add=TRUE, lty=2)
			points(grid_tmp_lt5y$long[def], grid_tmp_lt5y$lat[def], col=3, cex=0.4)

			# Only keep those points that are inside that polygon
			grid_tmp_lt5y[def==TRUE,paste0(which_SIAs_subnational[x], "_elig")] <- 1
		
		}

	}
	
	# Get the age limits of each sub-national SIA
	load("./Results/eCDF/All_countries.RData")
	relevant_SIAs <- dat_SIA[which(dat_SIA$country==i),]
	rownames(relevant_SIAs) <- 1:nrow(relevant_SIAs)
	# relevant_SIAs <- relevant_SIAs[-3,]
	relevant_SIAs$lb <- floor(relevant_SIAs$lb)
	relevant_SIAs$ub <- ceiling(relevant_SIAs$ub)
	
	# For each age
	for(k in lower_age:upper_age) {
	
		SIA_1 <- grid_tmp_lt5y$SIA_1_elig * ifelse(k >= min(relevant_SIAs[1,1:2]) && k <= max(relevant_SIAs[1,1:2]) && !is.infinite(relevant_SIAs[1,1]) && !is.infinite(relevant_SIAs[1,2]), 1, 0)
		SIA_2 <- grid_tmp_lt5y$SIA_2_elig * ifelse(k >= min(relevant_SIAs[2,1:2]) && k <= max(relevant_SIAs[2,1:2]) && !is.infinite(relevant_SIAs[2,1]) && !is.infinite(relevant_SIAs[2,2]), 1, 0)
		SIA_3 <- grid_tmp_lt5y$SIA_3_elig * ifelse(k >= min(relevant_SIAs[3,1:2]) && k <= max(relevant_SIAs[3,1:2]) && !is.infinite(relevant_SIAs[3,1]) && !is.infinite(relevant_SIAs[3,2]), 1, 0)
		SIA_4 <- grid_tmp_lt5y$SIA_4_elig * ifelse(k >= min(relevant_SIAs[4,1:2]) && k <= max(relevant_SIAs[4,1:2]) && !is.infinite(relevant_SIAs[4,1]) && !is.infinite(relevant_SIAs[4,2]), 1, 0)
		SIA_5 <- grid_tmp_lt5y$SIA_5_elig * ifelse(k >= min(relevant_SIAs[5,1:2]) && k <= max(relevant_SIAs[5,1:2]) && !is.infinite(relevant_SIAs[5,1]) && !is.infinite(relevant_SIAs[5,2]), 1, 0)
		SIA_6 <- grid_tmp_lt5y$SIA_6_elig * ifelse(k >= min(relevant_SIAs[6,1:2]) && k <= max(relevant_SIAs[6,1:2]) && !is.infinite(relevant_SIAs[6,1]) && !is.infinite(relevant_SIAs[6,2]), 1, 0)
		SIA_7 <- grid_tmp_lt5y$SIA_7_elig * ifelse(k >= min(relevant_SIAs[7,1:2]) && k <= max(relevant_SIAs[7,1:2]) && !is.infinite(relevant_SIAs[7,1]) && !is.infinite(relevant_SIAs[7,2]), 1, 0)
		SIA_8 <- grid_tmp_lt5y$SIA_8_elig * ifelse(k >= min(relevant_SIAs[8,1:2]) && k <= max(relevant_SIAs[8,1:2]) && !is.infinite(relevant_SIAs[8,1]) && !is.infinite(relevant_SIAs[8,2]), 1, 0)
		SIA_9 <- grid_tmp_lt5y$SIA_9_elig * ifelse(k >= min(relevant_SIAs[9,1:2]) && k <= max(relevant_SIAs[9,1:2]) && !is.infinite(relevant_SIAs[9,1]) && !is.infinite(relevant_SIAs[9,2]), 1, 0)
		SIA_10 <- grid_tmp_lt5y$SIA_10_elig * ifelse(k >= min(relevant_SIAs[10,1:2]) && k <= max(relevant_SIAs[10,1:2]) && !is.infinite(relevant_SIAs[10,1]) && !is.infinite(relevant_SIAs[10,2]), 1, 0)
		SIA_11 <- grid_tmp_lt5y$SIA_11_elig * ifelse(k >= min(relevant_SIAs[11,1:2]) && k <= max(relevant_SIAs[11,1:2]) && !is.infinite(relevant_SIAs[11,1]) && !is.infinite(relevant_SIAs[11,2]), 1, 0)
		SIA_12 <- grid_tmp_lt5y$SIA_12_elig * ifelse(k >= min(relevant_SIAs[12,1:2]) && k <= max(relevant_SIAs[12,1:2]) && !is.infinite(relevant_SIAs[12,1]) && !is.infinite(relevant_SIAs[12,2]), 1, 0)
		SIA_13 <- grid_tmp_lt5y$SIA_13_elig * ifelse(k >= min(relevant_SIAs[13,1:2]) && k <= max(relevant_SIAs[13,1:2]) && !is.infinite(relevant_SIAs[13,1]) && !is.infinite(relevant_SIAs[13,2]), 1, 0)
		SIA_14 <- grid_tmp_lt5y$SIA_14_elig * ifelse(k >= min(relevant_SIAs[14,1:2]) && k <= max(relevant_SIAs[14,1:2]) && !is.infinite(relevant_SIAs[14,1]) && !is.infinite(relevant_SIAs[14,2]), 1, 0)
		SIA_15 <- grid_tmp_lt5y$SIA_15_elig * ifelse(k >= min(relevant_SIAs[15,1:2]) && k <= max(relevant_SIAs[15,1:2]) && !is.infinite(relevant_SIAs[15,1]) && !is.infinite(relevant_SIAs[15,2]), 1, 0)
		SIA_16 <- grid_tmp_lt5y$SIA_16_elig * ifelse(k >= min(relevant_SIAs[16,1:2]) && k <= max(relevant_SIAs[16,1:2]) && !is.infinite(relevant_SIAs[16,1]) && !is.infinite(relevant_SIAs[16,2]), 1, 0)
		SIA_17 <- grid_tmp_lt5y$SIA_17_elig * ifelse(k >= min(relevant_SIAs[17,1:2]) && k <= max(relevant_SIAs[17,1:2]) && !is.infinite(relevant_SIAs[17,1]) && !is.infinite(relevant_SIAs[17,2]), 1, 0)
		SIA_18 <- grid_tmp_lt5y$SIA_18_elig * ifelse(k >= min(relevant_SIAs[18,1:2]) && k <= max(relevant_SIAs[18,1:2]) && !is.infinite(relevant_SIAs[18,1]) && !is.infinite(relevant_SIAs[18,2]), 1, 0)
		SIA_19 <- grid_tmp_lt5y$SIA_19_elig * ifelse(k >= min(relevant_SIAs[19,1:2]) && k <= max(relevant_SIAs[19,1:2]) && !is.infinite(relevant_SIAs[19,1]) && !is.infinite(relevant_SIAs[19,2]), 1, 0)
		SIA_20 <- grid_tmp_lt5y$SIA_20_elig * ifelse(k >= min(relevant_SIAs[20,1:2]) && k <= max(relevant_SIAs[20,1:2]) && !is.infinite(relevant_SIAs[20,1]) && !is.infinite(relevant_SIAs[20,2]), 1, 0)
		SIA_21 <- grid_tmp_lt5y$SIA_21_elig * ifelse(k >= min(relevant_SIAs[21,1:2]) && k <= max(relevant_SIAs[21,1:2]) && !is.infinite(relevant_SIAs[21,1]) && !is.infinite(relevant_SIAs[21,2]), 1, 0)
		SIA_22 <- grid_tmp_lt5y$SIA_22_elig * ifelse(k >= min(relevant_SIAs[22,1:2]) && k <= max(relevant_SIAs[22,1:2]) && !is.infinite(relevant_SIAs[22,1]) && !is.infinite(relevant_SIAs[22,2]), 1, 0)
	
		# Predict for certain values of covariates
		newd <- data.frame(long=grid_tmp_lt5y$long, lat=grid_tmp_lt5y$lat, binned_survey_age=k, 
						   SIA_1=SIA_1, SIA_2=SIA_2, SIA_3=SIA_3, SIA_4=SIA_4, SIA_5=SIA_5, 
						   SIA_6=SIA_6, SIA_7=SIA_7, SIA_8=SIA_8, SIA_9=SIA_9, SIA_10=SIA_10, 
						   SIA_11=SIA_11, SIA_12=SIA_12, SIA_13=SIA_13, SIA_14=SIA_14, SIA_15=SIA_15, 
						   SIA_16=SIA_16, SIA_17=SIA_17, SIA_18=SIA_18, SIA_19=SIA_19, SIA_20=SIA_20,
						   SIA_21=SIA_21, SIA_22=SIA_22)	

		# Predict at grid cells
		pred <- predict.gam(fit_phenom, newd, se.fit=TRUE)
		
		# Get the Mean and SE of prediction
		grid_tmp_lt5y[,paste0("Mean_", k, "m")] <- pred$fit
		grid_tmp_lt5y[,paste0("SE_", k, "m")] <- pred$se.fit
		
	}
	
	# Convert the data points to RasterLayer --> SPDF for manipulation
	g <- rasterFromXYZ(grid_tmp_lt5y)
	g <- as(g, "SpatialPolygonsDataFrame")

	# Get the intersection of the SPDF and adm0 border, which produces a SpatialPolygons
	g <- rgeos::gIntersection(g, adm0, byid=TRUE)		

	# Convert back to SPDF
	g <- as(g, "SpatialPolygonsDataFrame")
	g@data$id <- rownames(g@data)
	
	# Add all the data back 	
	data_to_add <- cbind(grid_tmp_lt5y[,paste0("Mean_", lower_age:upper_age, "m")], grid_tmp_lt5y[,paste0("SE_", lower_age:upper_age, "m")])
	rownames(data_to_add) <- g@data$id
	g <- spCbind(g, data_to_add)
	
	# Save the SPDF as a data.frame to plot
	g@data$id <- rownames(g@data)
	g_points <- ggplot2::fortify(g, region="id")	
	g_df <- plyr::join(g_points, g@data, by="id")

	# Save the .RData
	save(grid_tmp_lt5y, g, g_df, file=paste0("./Results/GAMtoMeanAndSE_Della_WithSubnationals/GAMtoMeanAndSE_WithSubnationals_", codes$DHS_code[i], "_", lower_age, "-", upper_age, "m", ".RData"))

}

getGAM_Map_WithSubSIA_DRC(lower_age=6, upper_age=60)

getGAM_Map_WithSubSIA_Tanzania <- function(lower_age, upper_age) {

	# Which country?
	i <- 7
	
	# Load the SIA data 1
	load(paste0("./Data/Manipulated_data/", as.character(codes$DHS_code[i]), ".RData"))

	# Load the SIA data 2
	load(paste0("./Data/Manipulated_data_for_GAM/", as.character(codes$DHS_code[i]), ".RData"))

	# Load the GAM ouput
	load(paste0("./Results/GAM_fit_phenom/", codes$DHS_code[i], ".RData"))

	# Get a data.frame of grid cells over the country at a certain grid size
	load(paste0("./Results/Imputed_WorldPop/", codes$DHS_code[i], ".RData"))

	# Get border lines to add
	adm0 <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[i], "_adm/", codes$ADM_code[i], "_adm0.shp"))
	
	## Double check: what region type is the SIAs on?
	# print(as.numeric(codes$SIA_region_type[i]))
	# print(get(paste0("region", as.numeric(codes$SIA_region_type[i]), "_key")))
	
	## Should be the same as above
	# print(as.numeric(codes$analysis_region_type[i]))
	# print(get(paste0("region", as.numeric(codes$analysis_region_type[i]), "_key")))

	# Get the map data for province plotting
	adm1 <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[i], "_adm/", codes$ADM_code[i], "_adm1.shp"))
	adm1@data$id <- rownames(adm1@data)

	# Centroids of province for ID
	unifiedPolygons <- unionSpatialPolygons(adm1, adm1@data$id)
	centroids <- as.data.frame(coordinates(unifiedPolygons))
	names(centroids) <- c("long", "lat")
	centroids$name <- as.numeric(rownames(centroids))
	
	# Loop over all the sub-national SIAs
	for(x in 1:length(which_SIAs_subnational)) {
		
		# Make an indicator for that location's eligibility for the SIA
		grid_tmp_lt5y[,paste0(which_SIAs_subnational[x], "_elig")] <- 0 
		
		# Which region2's (AKA adm1@data$id) are contained in that region1?
		region2_of_SIA <- c(0:16, 19:26)
	
		# Loop over all the region2's in that region1
		for(j in 1:length(region2_of_SIA)) {
		
			# Get the DHS region ID
			which_DHS_region <- which(centroids$name==region2_of_SIA[j])
			region <- unifiedPolygons[which_DHS_region]
			
			# Buffer everything, because otherwise we miss some boundary cases
			region <- gBuffer(region, width=0.01)

			## Manual change 1:
			if(j==2) {
			
				# Get the points on the country-level grid that are actually inside the polygon
				def <- inout(pts=as.points(grid_tmp_lt5y$long, grid_tmp_lt5y$lat), 
							 poly=region@polygons[[1]]@Polygons[[2]]@coords, 
							 bound=TRUE)
			
			}
			
			## Manual change 2:
			else if(j==18) {
			
				# Get the points on the country-level grid that are actually inside the polygon
				def <- inout(pts=as.points(grid_tmp_lt5y$long, grid_tmp_lt5y$lat), 
							 poly=region@polygons[[1]]@Polygons[[10]]@coords, 
							 bound=TRUE)				
			
			}
			
			else {
			
				# Get the points on the country-level grid that are actually inside the polygon
				def <- inout(pts=as.points(grid_tmp_lt5y$long, grid_tmp_lt5y$lat), 
							 poly=region@polygons[[1]]@Polygons[[1]]@coords, 
							 bound=TRUE)
						 
			}
			
			# Plot the "in" points to double check
			plot(adm0, main=paste0(j, " of ", length(region2_of_SIA)))
			plot(unifiedPolygons[which_DHS_region], add=TRUE)
			plot(region, add=TRUE, lty=2)
			points(grid_tmp_lt5y$long[def], grid_tmp_lt5y$lat[def], col=3, cex=0.4)

			# Only keep those points that are inside that polygon
			grid_tmp_lt5y[def==TRUE,paste0(which_SIAs_subnational[x], "_elig")] <- 1
		
		}
	
	}
	
	# Get the age limits of each sub-national SIA
	load("./Results/eCDF/All_countries.RData")
	relevant_SIAs <- dat_SIA[which(dat_SIA$country==i),]
	relevant_SIAs$lb <- floor(relevant_SIAs$lb)
	relevant_SIAs$ub <- ceiling(relevant_SIAs$ub)	
		
	# For each age
	for(k in lower_age:upper_age) {
	
		SIA_1 <- grid_tmp_lt5y$SIA_1_elig * ifelse(k >= min(relevant_SIAs[1,1:2]) && k <= max(relevant_SIAs[1,1:2]) && !is.infinite(relevant_SIAs[1,1]) && !is.infinite(relevant_SIAs[1,2]), 1, 0)
		
		# Predict for certain values of covariates
		newd <- data.frame(long=grid_tmp_lt5y$long, lat=grid_tmp_lt5y$lat, binned_survey_age=k, SIA_1=SIA_1)

		# Predict at grid cells
		pred <- predict.gam(fit_phenom, newd, se.fit=TRUE)
		
		# Get the Mean and SE of prediction
		grid_tmp_lt5y[,paste0("Mean_", k, "m")] <- pred$fit
		grid_tmp_lt5y[,paste0("SE_", k, "m")] <- pred$se.fit
		
	}

	# Convert the data points to RasterLayer --> SPDF for manipulation
	g <- rasterFromXYZ(grid_tmp_lt5y)
	g <- as(g, "SpatialPolygonsDataFrame")

	# Get the intersection of the SPDF and adm0 border, which produces a SpatialPolygons
	g <- rgeos::gIntersection(g, adm0, byid=TRUE)		

	# Convert back to SPDF
	g <- as(g, "SpatialPolygonsDataFrame")
	g@data$id <- rownames(g@data)
	
	# Add all the data back 	
	data_to_add <- cbind(grid_tmp_lt5y[,paste0("Mean_", lower_age:upper_age, "m")], grid_tmp_lt5y[,paste0("SE_", lower_age:upper_age, "m")])
	rownames(data_to_add) <- g@data$id
	g <- spCbind(g, data_to_add)

	# Save the SPDF as a data.frame to plot
	g@data$id <- rownames(g@data)
	g_points <- ggplot2::fortify(g, region="id")	
	g_df <- plyr::join(g_points, g@data, by="id")

	# Save the .RData
	save(grid_tmp_lt5y, g, g_df, file=paste0("./Results/GAMtoMeanAndSE_Della_WithSubnationals/GAMtoMeanAndSE_WithSubnationals_", codes$DHS_code[i], "_", lower_age, "-", upper_age, "m", ".RData"))

}

getGAM_Map_WithSubSIA_Tanzania(lower_age=6, upper_age=60)

################################################################################

## ./Results/GAMtoPeople_Della
## Number of unvaccinated people between [lower_age, upper_age] months of age at each grid cell
## Output: one file per country

getPeopleMap_noSubSIA <- function(lower_age, upper_age, i) {

	# Load the GAM ouput
	load(paste0("./Results/GAM_fit_phenom/", codes$DHS_code[i], ".RData"))

	# Get a data.frame of grid cells over the country at a certain grid size
	load(paste0("./Results/Imputed_WorldPop/", codes$DHS_code[i], ".RData"))
	grid_tmp_lt5y$z <- 0

	# Get border lines to add
	adm0 <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[i], "_adm/", codes$ADM_code[i], "_adm0.shp"))
	
	# For each age
	for(j in lower_age:upper_age) {

		# Predict for certain values of covariates
		newd <- data.frame(long=grid_tmp_lt5y$long, lat=grid_tmp_lt5y$lat, 
						   binned_survey_age=j,
						   SIA_1=0, SIA_2=0, SIA_3=0, SIA_4=0, SIA_5=0, SIA_6=0,
						   SIA_7=0, SIA_8=0, SIA_9=0, SIA_10=0, SIA_11=0, SIA_12=0,
						   SIA_13=0, SIA_14=0, SIA_15=0, SIA_16=0, SIA_17=0,
						   SIA_18=0, SIA_19=0, SIA_20=0, SIA_21=0, SIA_22=0)	

		# Predict at grid cells
		pred <- predict.gam(fit_phenom, newd)
		
		# Get the cumulative number of unvaccinateds
		grid_tmp_lt5y$z <- grid_tmp_lt5y$z + ((1-plogis(pred)) * grid_tmp_lt5y$lt5y/60)
		
		# Get the proportion unvaccinated in that monthly age group
		grid_tmp_lt5y[,paste0("p_unvacc_", j, "m")] <- 1-plogis(pred)
		
	}
	
	# Convert the data points to RasterLayer --> SPDF for manipulation
	g <- rasterFromXYZ(grid_tmp_lt5y[, c("long", "lat", "z")])
	g <- as(g, "SpatialPolygonsDataFrame")	

	# Get the intersection of the SPDF and adm0 border, which produces a SpatialPolygons
	g <- rgeos::gIntersection(g, adm0, byid=TRUE)		

	# Get the area of each grid cell in the SP, so that we can reduce the number of unvaccinated people between [lower_age, upper_age] months of age, by the % of that cell that is actually in that country
	area <- NULL
	for(k in 1:length(g)) area[k] <- g@polygons[[k]]@Polygons[[1]]@area
	area <- area/max(area)
	
	# Convert back to SPDF, and add a new column for the ADJUSTED number of unvaccinated people between [lower_age, upper_age] months of age, which we call 'dummy' 	
	g <- as(g, "SpatialPolygonsDataFrame")	
	g$dummy <- grid_tmp_lt5y$z * area
	print(summary(g$dummy))
	
	# Save the SPDF as a data.frame to plot
	g@data$id <- rownames(g@data)
	g_points <- ggplot2::fortify(g, region="id")	
	g_df <- plyr::join(g_points, g@data, by="id")

	# Save the .RData
	save(grid_tmp_lt5y, g, g_df, area, file=paste0("./Results/GAMtoPeople_Della/GAMtoPeople_", codes$DHS_code[i], "_", lower_age, "-", upper_age, "m", ".RData"))

}

getPeopleMap_noSubSIA(lower_age=6, upper_age=60, i=index)

################################################################################

## ./Results/Variogram_Della
## GAM residual analysis: variogram envelope simulations
## Output: one file per country

prop_of_max <- 1  		# maximum distance is [prop_of_max] of the max distance between points
max_dist_km <- 200		# maximum distance that we're assessing for all countries, in km
interval <- 5	  		# spaced at [interval] kilometers	
nsim <- 5000	  		# how many simulations do you want to run for the "envelope"
nugget_km <- 0			# below what distance do we assume points are co-located?

dat_variogram <- NULL

getResidualVariogram <- function(nugget_km, prop_of_max, max_dist_km, interval, nsim, i) {

	# Load the manipulated data
	load(paste0("./Data/Manipulated_data_for_Variogram/", codes$DHS_code[i], ".RData"))

	# Variogram of the residuals, maximum distance is [max_dist_km], spaced at [interval] kilometers	
	variog <- geoR::variog(gb, 
						   nugget.tolerance=nugget_km * 1000,
						   max.dist=max_dist_km * 1000, 
						   breaks=seq(0, max_dist_km * 1000, by=interval*1000), # Unit: meters
						   estimator.type="classical")

    # If you want to create an 'envelope'
	variog.env <- geoR::variog.mc.env(gb, obj.var=variog, nsim=nsim)

	# Save the data
	dat_variogram$x <- variog$u
	dat_variogram$y <- variog$v
	dat_variogram$y_lower <- variog.env$v.lower
	dat_variogram$y_upper <- variog.env$v.upper
	dat_variogram$z <- rep(codes$Country[i], times=length(variog$u))
	dat_variogram$zz <- rep(i, times=length(variog$u))
	dat_variogram <- as.data.frame(dat_variogram)
	
	save(nugget_km, prop_of_max, max_dist_km, interval, nsim, dat_variogram, file=paste0("./Results/Variogram_Della/Variogram_resid_", codes$DHS_code[i], ".RData"))

}

getResidualVariogram(nugget_km=nugget_km, prop_of_max=prop_of_max, max_dist_km=max_dist_km, interval=interval, nsim=nsim, i=index)

################################################################################