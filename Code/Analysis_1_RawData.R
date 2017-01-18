library(foreign)
library(maptools)
library(RColorBrewer)
library(ggplot2)

# Source code for analysis
source("./Code/FunctionsForAnalysis.R")

# Read in the master list of country codes
codes <- as.data.frame(read.csv("./Data/Master_List.csv"))
	
################################################################################

# Read in the raw SIA data
dataSIA <- read.csv("./Data/SIA/Measles_SIAs_2000_2015.csv", na.strings=c("","NA"), stringsAsFactors=FALSE)

# Maniema is saved as "Maniema\n"
dataSIA$Regions[61] <- dataSIA$Regions[49] <- "Maniema"

# Remove all the outbreak response immunizations (ORI)
dataSIA <- subset(dataSIA, Activity!="ORI")

# Get the pre-processed data for all countries
for(i in 1:length(codes$Country)) getDataByInd_Cluster(codes=codes, input_country=codes$Country[i])

# Get the individual-level SIA data in addition to this, for GAMs
for(i in 1:length(codes$Country)) {

	# Load pre-processed data
	load(paste0("./Data/Manipulated_data/", codes$DHS_code[which(codes$Country==codes$Country[i])], ".RData"))

	# Clean up the SIA for the country of interest
	dataSIA_country <- getCleanSIAs(codes=codes, 
									dataInd=dataInd, 
									dataSIA=dataSIA, 
									input_country=codes$Country[i])

	# Get the individual level data with SIA eligibility
	dataInd_country <- getSIAelibilityInd(dataInd=dataInd, 
										  dataSIA_country=dataSIA_country, 
										  input_country=codes$Country[i], 
										  SIA_region_type=codes$SIA_region_type[codes$Country==codes$Country[i]])

	# If survey_age_ub_mos < 1, add 0.0001 (this is so that the ages range from 1-60)
	dataInd_country$survey_age_ub_mos[which(dataInd_country$survey_age_ub_mos<1)] <- dataInd_country$survey_age_ub_mos[which(dataInd_country$survey_age_ub_mos<1)] + 0.0001
		
	# Round up survey age
	dataInd_country$binned_survey_age <- ceiling(dataInd_country$survey_age_ub_mos)

	# Link up long/lat from dataClust to dataInd_country
	dataInd_country$long <- dataClust$long[match(dataInd_country$cluster_id,dataClust$cluster_id)]
	dataInd_country$lat <- dataClust$lat[match(dataInd_country$cluster_id,dataClust$cluster_id)]

	# Remove places where long/lat are NA
	dataInd_country <- dataInd_country[which(!is.na(dataInd_country$lat)),]

	# [Mozambique] has [2] clusters with latlong=c(0,0), so remove these
	dataInd_country <- dataInd_country[which(dataInd_country$long>1),]
	
	# [Kenya] has some people whose measles_age is negative, so remove these
	dataInd_country <- dataInd_country[which(dataInd_country$measles_age_ub_mos>=0 | is.na(dataInd_country$measles_age_ub_mos)),]
		
	# [Tanzania]'s 21 sub-national SIAs are all 1 campaign (all parts of mainland), so combine and call it SIA_1
	if(codes$Country[i]=="Tanzania"){
	
		dataInd_country$SIA_1 <- rowSums(dataInd_country[,14:34])
		dataInd_country$SIA_1 <- ifelse(dataInd_country$SIA_1>1, 1, dataInd_country$SIA_1)
		dataInd_country <- dataInd_country[,c(1:13,35:37)]
	
		dataSIA_country <- dataSIA_country[1,]
		dataSIA_country$Regions <- dataSIA_country$region1_name <- dataSIA_country$region2_name <- "All_Mainland"
		
		# Manually: get the SIA dates
		dataSIA_country$Start_Date <- "2008-08-30"
		dataSIA_country$End_Date <- "2008-09-01"
		dataSIA_country$age_lb_mos <- 6
		dataSIA_country$age_ub_mos <- 131
		dataSIA_country$dob_oldest <- "1997-08-31"
		dataSIA_country$dob_youngest <- "2008-03-01"
	
	}
	
	# Remove children less than 6 months of age at survey
	dataInd_country <- dataInd_country[which(dataInd_country$binned_survey_age>=6),]
	
	# Separate out national from sub-national SIAs
	all_SIAs <- dataSIA_country$Campaign_name
	which_SIAs_national <- dataSIA_country$Campaign_name[which(dataSIA_country$region1_name==0 | dataSIA_country$region2_name==0)]
	which_SIAs_subnational <- dataSIA_country$Campaign_name[(dataSIA_country$Campaign_name %in% which_SIAs_national)==FALSE]

	# If there are no national SIAs, as in [Tanzania], then the data sets should match (i.e., dataInd_country and dataInd_country_subnational)
	if(length(which_SIAs_national)==0) {
		dataInd_country_subnational <- dataInd_country
	}
	else {
		dataInd_country_subnational <- dataInd_country[, -which(names(dataInd_country) %in% which_SIAs_national)]
	}

	# Save the data
	save(all_SIAs, which_SIAs_subnational, dataInd_country_subnational, dataInd_country, dataSIA_country, file=paste0("./Data/Manipulated_data_for_GAM/", codes$DHS_code[which(codes$Country==codes$Country[i])], ".RData"))

}

################################################################################