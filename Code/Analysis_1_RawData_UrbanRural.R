library(foreign)
library(maptools)
library(RColorBrewer)
library(ggplot2)

# Source code for analysis
source("./Code/Functions_For_Analysis.R")

# Read in the master list of country codes
codes <- as.data.frame(read.csv("./Data/Master_List.csv"))
	
################################################################################

# Add indicator of rural/urban to each DHS cluster
for(i in 1:nrow(codes)){

	# Load the data I
	load(paste0("./Data/Manipulated_data/", codes$DHS_code[i], ".RData"))
	
	# Load the manipulated data for GAMs
	load(paste0("./Data/Manipulated_data_for_GAM/", codes$DHS_code[i], ".RData"))
	
	# Add column for urban or rural
	dataInd_country$urban_rural <- dataClust$urban_rural[match(dataInd_country$cluster_id,dataClust$cluster_id)]
	dataInd_country_subnational$urban_rural <- dataClust$urban_rural[match(dataInd_country_subnational$cluster_id,dataClust$cluster_id)]
	
	# Save
	save(all_SIAs, which_SIAs_subnational, dataInd_country_subnational, dataInd_country, dataSIA_country, file=paste0("./Data/Manipulated_data_for_GAM_UrbanRural/", codes$DHS_code[i], ".RData"))
	
}

################################################################################