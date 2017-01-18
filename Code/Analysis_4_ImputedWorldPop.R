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

# Source code for analysis
source("./Code/FunctionsForAnalysis.R")

# Read in the master list of country codes
codes <- as.data.frame(read.csv("./Data/Master_List.csv"))

################################################################################

# Get a country's grid cells from WorldPop, with numbers of lt5y in 2010 as well
# Resolution of WorldPop is 0.00833333 decimal degrees (approx 1km at the equator) 
# We aggregate this up by a factor of 10, meaning 0.0833333 decimal degrees (approx 10 km)

for(i in 1:nrow(codes)) {

	# Which country?
	ADM_code <- as.character(codes$ADM_code[i])
	adm0 <- readShapeSpatial(paste0("./Data/ADM/", ADM_code, "_adm/", ADM_code, "_adm0.shp"))
	
	# Males, 2010, 0-59 months
	worldpop_tiff_m <- "./Data/WorldPop/ap10v4_A0005_M_adj.tif"
	continent_m <- raster(worldpop_tiff_m)
	continent_m_country <- crop(continent_m, extent(adm0))
	continent_m_country <- mask(continent_m_country, adm0)

	# Females, 2010, 0-59 months
	worldpop_tiff_f <- "./Data/WorldPop/ap10v4_A0005_F_adj.tif"
	continent_f <- raster(worldpop_tiff_f)
	continent_f_country <- crop(continent_f, extent(adm0))
	continent_f_country <- mask(continent_f_country, adm0)

	# Combine M + F
	continent_country_mf_lt5y <- overlay(continent_m_country, continent_f_country, fun=sum)
	continent_country_mf_lt5y_df <- as.data.frame(continent_country_mf_lt5y, xy=TRUE)

	# Aggregate up
	continent_country_mf_lt5y_agg <- raster::aggregate(continent_country_mf_lt5y, fun=sum, fact=10)
	continent_country_mf_lt5y_agg_df <- as.data.frame(continent_country_mf_lt5y_agg, xy=TRUE)

	# Get rid of NA cells
	continent_country_mf_lt5y_agg_df_complete <- continent_country_mf_lt5y_agg_df[complete.cases(continent_country_mf_lt5y_agg_df),]

	grid_tmp_lt5y <- data.frame(long=continent_country_mf_lt5y_agg_df_complete$x, 
								lat=continent_country_mf_lt5y_agg_df_complete$y,
								lt5y=continent_country_mf_lt5y_agg_df_complete$layer)

	# Save data
	save(grid_tmp_lt5y, file=paste0("./Results/Imputed_WorldPop/", codes$DHS_code[i], ".RData"))

}

################################################################################