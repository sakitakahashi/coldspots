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
source("./Code/Functions_For_Analysis.R")

# Read in the master list of country codes
codes <- as.data.frame(read.csv("./Data/Master_List.csv"))

################################################################################

# Table of DHS info

DHS_info <- matrix(NA, nrow=nrow(codes), ncol=7)
colnames(DHS_info) <- c("country", "survey_dates", "children_surveyed", "locations_surveyed", "prop_vacc_cards", "N_national_SIA", "N_subnational_SIA")
DHS_info <- as.data.frame(DHS_info)

for(i in 1:nrow(codes)) {

	# Load pre-processed data for the country
	load(paste0("./Data/Manipulated_data_for_GAM/", codes$DHS_code[i], ".RData"))

	# Which country?
	DHS_info$country[i] <- as.character(codes$Country[i])
	
	# Survey dates
	DHS_info$survey_dates[i] <- paste(summary(dataInd_country$survey_date)[1], summary(dataInd_country$survey_date)[6], sep="_")

	# Total children surveyed
	DHS_info$children_surveyed[i] <- nrow(dataInd_country)

	# Locations surveyed
	DHS_info$locations_surveyed[i] <- length(unique(dataInd_country$cluster_id))

	# Look only at vaccinated kids
	dataInd_country_vaccinated <- dataInd_country[dataInd_country$measles_status==1,]
	
	# Of these, what proportion have cards?
	DHS_info$prop_vacc_cards[i] <- round(length(which(!is.na(dataInd_country_vaccinated$measles_date)==T))/nrow(dataInd_country_vaccinated),4)

}

load("./Results/eCDF/All_countries.RData")

# Get rid of SIAs with 0 eligs
dat_SIA <- dat_SIA[-(which(is.infinite(dat_SIA$lb)==TRUE)),]
dat_SIA_table <- table(dat_SIA$national, by=dat_SIA$country)

DHS_info$N_national_SIA <- dat_SIA_table[2,]
DHS_info$N_subnational_SIA <- dat_SIA_table[1,]

write.csv(DHS_info, file="./Results/Tables/DHS_info.csv")

################################################################################

# Table of GAM output: by country

GAM_numbers <- matrix(NA, nrow=nrow(codes), ncol=5)
colnames(GAM_numbers) <- c("country", "df_longlat", "df_age", "dev", "r2")

for(i in 1:nrow(codes)) {

	# Load pre-processed data for the country
	load(paste0("./Results/GAM_fit_phenom/", codes$DHS_code[i], ".RData"))

	# Which country?
	GAM_numbers[i,"country"] <- as.character(codes$Country[i])
	
	# Estimated degrees of freedom for s(long,lat) and s(binned_survey_age)
	GAM_numbers[i,"df_longlat"] <- summary(fit_phenom)["edf"]$edf[1]
	GAM_numbers[i,"df_age"] <- summary(fit_phenom)["edf"]$edf[2]

	# Deviance expained
	GAM_numbers[i,"dev"] <- summary(fit_phenom)["dev.expl"]$dev.expl	
	
	# Adjusted R-squared
	GAM_numbers[i,"r2"] <- summary(fit_phenom)["r.sq"]$r.sq

}

GAM_numbers <- as.data.frame(GAM_numbers)
write.csv(GAM_numbers, file="./Results/Tables/GAM.csv")

################################################################################

# Table of how many/what % of people live in a Coldspot (defined as below cutoff_percent) for coldspot_age, between lb_age and ub_age

cutoff_percent <- 0.8
coldspot_age <- 24
lb_age <- 1
ub_age <- 60

data_final <- NULL

for(i in 1:nrow(codes)) {
		
	data_final$country[i] <- as.character(codes$Country[i])
	
	# Load the Della output for people counts
	load(paste0("./Results/GAMtoPeople_Della/GAMtoPeople_",codes$DHS_code[i],"_6-60m.RData"))

	# Get the number of people of each month group at each cell
	month_age_size <- grid_tmp_lt5y$lt5y/60
	data_final$month_age_size[i] <- sum(month_age_size)
	
	# Get the number of people between lb_age and ub_age at each cell
	total_month_age_size <- (grid_tmp_lt5y$lt5y/60) * length(lb_age:ub_age)
	data_final$total_month_age_size[i] <- sum(total_month_age_size)
		
	# Load the Della output for mean/SE at coldspot_age 
	load(paste0("./Results/GAMtoMeanAndSE_Della/GAMtoMeanAndSE_", codes$DHS_code[i],"_6-60m.RData"))
	
	# Figure out the mean/lb/ub probability of being vaccinated at coldspot_age, at each cell
	mean_data <- paste0("Mean_", coldspot_age, "m")
	se_data <- paste0("SE_", coldspot_age, "m")
	
	mean_cells_vacc_prob <- plogis(grid_tmp_lt5y[, mean_data])
	lb_cells_vacc_prob <- plogis(grid_tmp_lt5y[, mean_data] - 1.96*grid_tmp_lt5y[, se_data])
	ub_cells_vacc_prob <- plogis(grid_tmp_lt5y[, mean_data] + 1.96*grid_tmp_lt5y[, se_data])

	# Determine if coldspot or not (defined as vacc_prob < cutoff_percent), at each cell
	mean_coldspot <- ifelse(mean_cells_vacc_prob < cutoff_percent, 1, 0)
	lb_coldspot <- ifelse(lb_cells_vacc_prob < cutoff_percent, 1, 0)
	ub_coldspot <- ifelse(ub_cells_vacc_prob < cutoff_percent, 1, 0)

	# Make a new combined data frame, for ease
	data_combined <- data.frame(total_month_age_size=total_month_age_size, mean_coldspot=mean_coldspot, lb_coldspot=lb_coldspot, ub_coldspot=ub_coldspot)

	# Get number of children aged binned_survey_age, living in a coldspot for coldspot_age
	data_final$mean_number[i] <- round(sum(data_combined$total_month_age_size[data_combined$mean_coldspot==1]))
	data_final$lb_number[i] <- round(sum(data_combined$total_month_age_size[data_combined$ub_coldspot==1]))
	data_final$ub_number[i] <- round(sum(data_combined$total_month_age_size[data_combined$lb_coldspot==1]))
	data_final$to_paste[i] <- paste0(data_final$mean_number[i], " (", data_final$lb_number[i], "-", data_final$ub_number[i], ")")

}

data_final <- data.frame(data_final)

# Get proportion of children between lb_age and ub_age, living in a coldspot for coldspot_age
mean_prop <- 100 * (data_final$mean_number / data_final$total_month_age_size)
lb_prop <- 100 * (data_final$lb_number / data_final$total_month_age_size)
ub_prop <- 100 * (data_final$ub_number / data_final$total_month_age_size)
data_final$number <- data_final$to_paste
data_final$percentage <- paste0(round(mean_prop, 2), " (", round(lb_prop, 2), "-", round(ub_prop, 2), ")")

write.csv(data_final, file=paste0("./Results/Tables/ChildrenInColdspots_", coldspot_age, ".csv"))

################################################################################

# Table of GAM output: for the entire region

Regional_GAM_table <- NULL

# Load pre-processed data for the entire region
load("./Results/GAM_fit_phenom_Regional/Regional.RData")

# Coefficients by country
Regional_GAM_table$coef[1:10] <- round(summary(fit_phenom)["p.coeff"][[1]][1:10], 4)

# Standard errors by country
Regional_GAM_table$se[1:10] <- round(summary(fit_phenom)["se"][[1]][1:10],4)
	
Regional_GAM_table_df <- as.data.frame(Regional_GAM_table)
write.csv(Regional_GAM_table_df, file="./Results/Tables/GAM_Regional.csv")

################################################################################

# Table of unvaccinateds (number) by country and age group: no sub-national SIAs

getNumbers_Unvaccinateds <- function(lower_age, upper_age) {

	data <- matrix(NA, nrow=nrow(codes), ncol=4)
	
	for(i in 1:nrow(codes)) {
		
		# Load the Della output for people counts
		load(paste0("./Results/GAMtoPeople_Della/GAMtoPeople_",codes$DHS_code[i],"_6-60m.RData"))

		# Get the number of people of each month age group at each cell
		people <- grid_tmp_lt5y$lt5y/60
		rm(grid_tmp_lt5y)

		# Load the Della ouput for mean/SE
		load(paste0("./Results/GAMtoMeanAndSE_Della/GAMtoMeanAndSE_",codes$DHS_code[i],"_6-60m.RData"))

		# Figure out the mean/lb/ub probability of being NOT vaccinated between [lower_age, upper_age] at each cell
		mean_data <- paste0("Mean_", lower_age:upper_age, "m")
		se_data <- paste0("SE_", lower_age:upper_age, "m")

		mean_cells_unvacc_prob <- 1-plogis(as.matrix(grid_tmp_lt5y[, mean_data]))
		lb_cells_unvacc_prob <- 1-plogis(as.matrix(grid_tmp_lt5y[, mean_data]) - 1.96*as.matrix(grid_tmp_lt5y[, se_data]))
		ub_cells_unvacc_prob <- 1-plogis(as.matrix(grid_tmp_lt5y[, mean_data]) + 1.96*as.matrix(grid_tmp_lt5y[, se_data]))

		# Calculate the number of unvaccinated people between [lower_age, upper_age]
		unvacc_mean <- sum(rowSums(mean_cells_unvacc_prob) * people)
		unvacc_lb <- sum(rowSums(lb_cells_unvacc_prob) * people)
		unvacc_ub <- sum(rowSums(ub_cells_unvacc_prob) * people)
				
		# Output the mean
		data[i,1] <- as.character(codes$Country[i])
		data[i,2] <- as.numeric(round(unvacc_mean))
		data[i,3] <- as.numeric(round(unvacc_ub))
		data[i,4] <- as.numeric(round(unvacc_lb))
		
	}
	
	data_final <- data.frame(name=data[,1], mean=data[,2], lb=data[,3], ub=data[,4], to_paste=paste0(format(as.numeric(data[,2]), big.mark=",", scientific=FALSE), " (", format(as.numeric(data[,3]), big.mark=",", scientific=FALSE), "-", format(as.numeric(data[,4]), big.mark=",", scientific=FALSE), ")"))
	
	save(data_final, file=paste0("./Results/Tables/Unvacc_counts_", lower_age, "_", upper_age, ".RData"))

}

getNumbers_Unvaccinateds(lower_age=6, upper_age=24)
getNumbers_Unvaccinateds(lower_age=6, upper_age=60)

getNumbers_Unvaccinateds(lower_age=6, upper_age=12)
getNumbers_Unvaccinateds(lower_age=13, upper_age=24)
getNumbers_Unvaccinateds(lower_age=25, upper_age=36)
getNumbers_Unvaccinateds(lower_age=37, upper_age=48)
getNumbers_Unvaccinateds(lower_age=49, upper_age=60)

################################################################################

# Table of vaccinateds (proportion) by country and age group: no sub-national SIAs

getProp_Vaccinateds <- function(lower_age, upper_age) {

	data <- matrix(NA, nrow=nrow(codes), ncol=4)
	
	for(i in 1:nrow(codes)) {
		
		# Load the Della output for people counts
		load(paste0("./Results/GAMtoPeople_Della/GAMtoPeople_",codes$DHS_code[i],"_6-60m.RData"))

		# Get the number of people of each month age group at each cell
		people <- grid_tmp_lt5y$lt5y/60
		rm(grid_tmp_lt5y)
		
		# Get the number of people between [lower_age, upper_age] at each grid cell
		people_interest <- people * length(lower_age:upper_age)

		# Load the Della ouput for mean/SE
		load(paste0("./Results/GAMtoMeanAndSE_Della/GAMtoMeanAndSE_",codes$DHS_code[i],"_6-60m.RData"))

		# Figure out the mean/lb/ub probability of being vaccinated between [lower_age, upper_age] at each cell
		mean_data <- paste0("Mean_", lower_age:upper_age, "m")
		se_data <- paste0("SE_", lower_age:upper_age, "m")

		mean_cells_vacc_prob <- plogis(as.matrix(grid_tmp_lt5y[, mean_data]))
		lb_cells_vacc_prob <- plogis(as.matrix(grid_tmp_lt5y[, mean_data]) - 1.96*as.matrix(grid_tmp_lt5y[, se_data]))
		ub_cells_vacc_prob <- plogis(as.matrix(grid_tmp_lt5y[, mean_data]) + 1.96*as.matrix(grid_tmp_lt5y[, se_data]))

		# Calculate the proportion of VACCINATED people between [lower_age, upper_age]
		vacc_mean <- sum(rowSums(mean_cells_vacc_prob) * people)/sum(people_interest)
		vacc_lb <- sum(rowSums(lb_cells_vacc_prob) * people)/sum(people_interest)
		vacc_ub <- sum(rowSums(ub_cells_vacc_prob) * people)/sum(people_interest)
				
		# Output the mean
		data[i,1] <- as.character(codes$Country[i])
		data[i,2] <- as.numeric(round(vacc_mean,4))
		data[i,3] <- as.numeric(round(vacc_lb,4))
		data[i,4] <- as.numeric(round(vacc_ub,4))
		
	}
	
	data_final <- data.frame(name=data[,1], coverage_mean=data[,2], coverage_lb=data[,3], coverage_ub=data[,4], to_paste=paste0(format(as.numeric(data[,2]), big.mark=",", scientific=FALSE), " (", format(as.numeric(data[,3]), big.mark=",", scientific=FALSE), "-", format(as.numeric(data[,4]), big.mark=",", scientific=FALSE), ")"))
	
	save(data_final, file=paste0("./Results/Tables/Vacc_prop_", lower_age, "_", upper_age, ".RData"))

}

getProp_Vaccinateds(lower_age=6, upper_age=12)
getProp_Vaccinateds(lower_age=13, upper_age=24)
getProp_Vaccinateds(lower_age=25, upper_age=36)
getProp_Vaccinateds(lower_age=37, upper_age=48)
getProp_Vaccinateds(lower_age=49, upper_age=60)

################################################################################

# Table of total children (number) by country

getNumbers <- function(lower_age, upper_age) {

	data <- matrix(NA, nrow=nrow(codes), ncol=2)
	
	for(i in 1:nrow(codes)) {
		
		# Load the Della output for people counts
		load(paste0("./Results/GAMtoPeople_Della/GAMtoPeople_",codes$DHS_code[i],"_6-60m.RData"))

		# Get the number of people of each month age group at each cell
		people <- grid_tmp_lt5y$lt5y/60
		
		# How many months of people?
		n_months <- length(lower_age:upper_age)
	
		# Output the mean
		data[i,1] <- as.character(codes$Country[i])
		data[i,2] <- as.numeric(round(sum(people) * n_months))
		
	}
	
	data_final <- data.frame(name=data[,1], children=data[,2])
	
	save(data_final, file=paste0("./Results/Tables/Total_counts_", lower_age, "_", upper_age, ".RData"))

}

getNumbers(lower_age=6, upper_age=60)

################################################################################

# Table of unvaccinateds (number) by country and age group: with sub-national SIAs

getNumbers_Unvaccinateds_WithSubnationals <- function(codes, lower_age, upper_age) {

	data <- matrix(NA, nrow=nrow(codes), ncol=4)
	
	for(i in 1:nrow(codes)) {
		
		# Load the Della output for people counts
		load(paste0("./Results/GAMtoPeople_Della/GAMtoPeople_",codes$DHS_code[i],"_6-60m.RData"))

		# Get the number of people of each month age group at each cell
		people <- grid_tmp_lt5y$lt5y/60
		rm(grid_tmp_lt5y)

		# Load the Della ouput for mean/SE
		load(paste0("./Results/GAMtoMeanAndSE_Della_WithSubnationals/GAMtoMeanAndSE_WithSubnationals_",codes$DHS_code[i],"_6-60m.RData"))

		# Figure out the mean/lb/ub probability of being NOT vaccinated between [lower_age, upper_age] at each cell
		mean_data <- paste0("Mean_", lower_age:upper_age, "m")
		se_data <- paste0("SE_", lower_age:upper_age, "m")

		mean_cells_unvacc_prob <- 1-plogis(as.matrix(grid_tmp_lt5y[, mean_data]))
		lb_cells_unvacc_prob <- 1-plogis(as.matrix(grid_tmp_lt5y[, mean_data]) - 1.96*as.matrix(grid_tmp_lt5y[, se_data]))
		ub_cells_unvacc_prob <- 1-plogis(as.matrix(grid_tmp_lt5y[, mean_data]) + 1.96*as.matrix(grid_tmp_lt5y[, se_data]))

		# Calculate the number of unvaccinated people between [lower_age, upper_age]
		unvacc_mean <- sum(rowSums(mean_cells_unvacc_prob) * people)
		unvacc_lb <- sum(rowSums(lb_cells_unvacc_prob) * people)
		unvacc_ub <- sum(rowSums(ub_cells_unvacc_prob) * people)

		# Output the mean
		data[i,1] <- as.character(codes$Country[i])
		data[i,2] <- as.numeric(round(unvacc_mean))
		data[i,3] <- as.numeric(round(unvacc_ub))
		data[i,4] <- as.numeric(round(unvacc_lb))
		
	}
	
	data_final <- data.frame(name=data[,1], mean=data[,2], lb=data[,3], ub=data[,4], to_paste=paste0(format(as.numeric(data[,2]), big.mark=",", scientific=FALSE), " (", format(as.numeric(data[,3]), big.mark=",", scientific=FALSE), "-", format(as.numeric(data[,4]), big.mark=",", scientific=FALSE), ")"))
	
	save(data_final, file=paste0("./Results/Tables/Unvacc_counts_WithSubnationals_", lower_age, "_", upper_age, ".RData"))

}

# Using only countries with sub-national SIAs
codes_sub <- codes[c(1,2,7),]
codes_sub$Country <- factor(codes_sub$Country)
rownames(codes_sub) <- 1:nrow(codes_sub)

getNumbers_Unvaccinateds_WithSubnationals(codes=codes_sub, lower_age=6, upper_age=24)
getNumbers_Unvaccinateds_WithSubnationals(codes=codes_sub, lower_age=6, upper_age=60)

################################################################################

# Data processing 

data_process <- matrix(NA, nrow=nrow(codes), ncol=8)

for(i in 1:nrow(codes)) {

	# Get the raw data
	XX_YYYY <- as.character(codes$DHS_code[i])

	# Read STATA data into R (this is a data.frame)
	dataDHS <- read.dta(paste0("./Data/DHS/", XX_YYYY, "/", XX_YYYY, ".DTA"), convert.factors=TRUE)
  
	# Read DBF data into R (this is a data.frame), and remove the cluster(s) not in the STATA dataset, if any
	dbf <- read.dbf(paste0("./Data/DHS/", XX_YYYY, "/", XX_YYYY, ".dbf"), as.is=FALSE)
	dbf <- dbf[(dbf$DHSCLUST %in% unique(dataDHS$v001)),]

	# Make cluster-data dataset, and populate the cluster id numbers
	dataClust <- data.frame(cluster_id=as.numeric(as.character(dbf$DHSCLUST)))
	rownames(dataClust) <- 1:nrow(dataClust)	

	# Add geographic info
	dataClust$long <- as.numeric(as.character(dbf$LONGNUM))
	dataClust$lat <- as.numeric(as.character(dbf$LATNUM))

	# Make individual-level dataset, and populate the individual id numbers
	dataInd <- data.frame(ind_id=1:nrow(dataDHS))
	rownames(dataInd) <- 1:nrow(dataInd)
	
	# Add geographic info
	dataInd$cluster_id <- dataDHS$v001
  	dataInd$long <- dataClust$long[match(dataInd$cluster_id, dataClust$cluster_id)]
	dataInd$lat <- dataClust$lat[match(dataInd$cluster_id, dataClust$cluster_id)]

	# Get DHS survey date
	dataInd$survey_date <- as.Date(sprintf("%d-%d-%d", dataDHS$v007, dataDHS$v006, dataDHS$v016), format="%Y-%m-%d")
	
	# Get lower bound (lb) of DOB of child (assume born on the first of the month)	
	dataInd$dob_lb <- as.Date(sprintf("%d-%d-01", dataDHS$b2, dataDHS$b1), format="%Y-%m-%d")
  
	# Match up lb and ub dates
	dob_key <- data.frame(dob_lb=seq(min(dataInd$dob_lb), max(dataInd$dob_lb), by="1 month"), 
						  dob_ub=(seq(min(dataInd$dob_lb), max(dataInd$dob_lb)+31, by="1 month")-1)[-1])
  
	# Get upper bound (ub) of DOB of child (assume born on the last of the month)
	dataInd$dob_ub <- dob_key$dob_ub[match(dataInd$dob_lb, dob_key$dob_lb)]
  
	# Get lb and ub age at DHS survey (in months)
	dataInd$survey_age_lb_mos <- as.numeric(dataInd$survey_date-dataInd$dob_ub)/(365.25/12)
	dataInd$survey_age_ub_mos <- as.numeric(dataInd$survey_date-dataInd$dob_lb)/(365.25/12)
  	dataInd$binned_survey_age <- ceiling(dataInd$survey_age_ub_mos)

	# If survey_age_ub_mos < 1, add 0.0001 (this is so that the ages range from 1-60)
	dataInd$survey_age_ub_mos[which(dataInd$survey_age_ub_mos<1)] <- dataInd$survey_age_ub_mos[which(dataInd$survey_age_ub_mos<1)] + 0.0001

	# Get measles vaccination status
	h9tmp <- as.character(dataDHS$h9)
	h9tmp[dataDHS$h9=="Vaccination date on card"] <- 1	
	h9tmp[dataDHS$h9=="vaccination date on card"] <- 1
	h9tmp[dataDHS$h9=="Vacc. date on card"] <- 1
	h9tmp[dataDHS$h9=="vacc. date on card"] <- 1
	h9tmp[dataDHS$h9=="Reported by mother"] <- 2		
	h9tmp[dataDHS$h9=="reported by mother"] <- 2
	h9tmp[dataDHS$h9=="Vaccination marked on card"] <- 3		
	h9tmp[dataDHS$h9=="vaccination marked on card"] <- 3
	h9tmp[dataDHS$h9=="Vacc. marked on card"] <- 3
	h9tmp[dataDHS$h9=="vacc. marked on card"] <- 3
	h9tmp[dataDHS$h9=="No"] <- 0
	h9tmp[dataDHS$h9=="no"] <- 0
	h9tmp[dataDHS$h9=="Don't know"] <- NA		
	h9tmp[dataDHS$h9=="don't know"] <- NA
	h9tmp[dataDHS$h9=="dk"] <- NA
	h9tmp[dataDHS$h9==8] <- NA
	h9tmp[dataDHS$h9==9] <- NA
	dataInd$measles_status <- rep(NA, length(h9tmp))
	dataInd$measles_status[h9tmp==0] <- 0
	dataInd$measles_status[h9tmp>0 & h9tmp<4] <- 1
 
	# If day of measles vaccination is missing (h9d), assume occurred on the 15th of the month 
	# (and convert the numeric to integer) - these are the one(s) that are missing a day (i.e., DRC)
	dataDHS$h9d[!is.na(dataDHS$h9d) & dataDHS$h9d>31] <- 15
	dataDHS$h9d <- as.integer(dataDHS$h9d)
  
	# If h9m or h9y are strange, change them to NAs and get rid of these vaccination dates overall
	dataDHS$h9m[!is.na(dataDHS$h9m) & dataDHS$h9m>12] <- NA
	dataDHS$h9y[!is.na(dataDHS$h9y) & dataDHS$h9y>2015] <- NA
	dataDHS$h9d[is.na(dataDHS$h9m) | is.na(dataDHS$h9y)] <- NA
	dataDHS$h9m[is.na(dataDHS$h9y)] <- NA
	dataDHS$h9y[is.na(dataDHS$h9m)] <- NA
  
	# Get measles vaccination date
	measles_date <- sprintf("%d-%d-%d", dataDHS$h9y, dataDHS$h9m, dataDHS$h9d)
	measles_date[measles_date=="NA-NA-NA"] <- NA
	dataInd$measles_date <- as.Date(measles_date)
	dataInd$measles_age_lb_mos <- as.numeric(dataInd$measles_date-dataInd$dob_ub)/(365.25/12)
	dataInd$measles_age_ub_mos <- as.numeric(dataInd$measles_date-dataInd$dob_lb)/(365.25/12)

	# [Mozambique] has [2] clusters with latlong=c(0,0), so remove these
	dataInd <- dataInd[which(dataInd$long>1),]
	
	# Get the total number of people in the survey
	data_process[i,1] <- nrow(dataInd)
	
	# Get the total number of people in the survey with GPS locations
	data_process[i,2] <- nrow(dataInd[which(!is.na(dataInd$lat)),])
	
	# Get the total number of people in the survey with GPS locations + complete DOB
	data_process[i,3] <- nrow(dataInd[which(!is.na(dataInd$lat) & !is.na(dataInd$dob_lb)),])
	
	# Get the total number of people in the survey with GPS locations + complete DOB + between 6-60m of age
	data_process[i,4] <- nrow(dataInd[which(!is.na(dataInd$lat) & !is.na(dataInd$dob_lb) & dataInd$binned_survey_age>=6),])
	
	# Get the total number of people in the survey with GPS locations + complete DOB + between 6-60m of age + measles vaccination status known
	# And for GAM: [Kenya] has some people whose measles_age is negative, so remove these
	data_process[i,5] <- nrow(dataInd[which(!is.na(dataInd$lat) & !is.na(dataInd$dob_lb) & dataInd$binned_survey_age>=6 & !is.na(dataInd$measles_status) & (dataInd$measles_age_ub_mos>=0 | is.na(dataInd$measles_age_ub_mos))),])

	# Get the total number of clusters represented by the final data set
	data_process[i,6] <- length(unique(dataInd[which(!is.na(dataInd$lat) & !is.na(dataInd$dob_lb) & dataInd$binned_survey_age>=6 & !is.na(dataInd$measles_status) & (dataInd$measles_age_ub_mos>=0 | is.na(dataInd$measles_age_ub_mos))),"cluster_id"]))
	
	# Compare this with the input data (i.e., the data used for analysis)
	load(paste0("./Data/Manipulated_data_for_GAM/", XX_YYYY, ".RData"))
	
	data_process[i,7] <- nrow(dataInd_country_subnational)
	data_process[i,8] <- length(unique(dataInd_country_subnational$cluster_id))

}

data_process <- data.frame(data_process)
colnames(data_process) <- c("raw", "loc", "loc+dob", "loc+dob+6_60", "loc+dob+6_60+vacc", "clusters", "INPUT_ppl", "INPUT_clusters")

write.csv(data_process, file="./Results/Tables/DHSDataProcessing.csv")

################################################################################

# Filtering population density over grid cells

data_filter <- matrix(NA, nrow=nrow(codes), ncol=4)
colnames(data_filter) <- c("all", "atleast100", "atleast500", "atleast1000")

for(i in 1:nrow(codes)) {

	# Load the Della output
	load(paste0("./Results/GAMtoPeople_Della/GAMtoPeople_",codes$DHS_code[i],"_6-60m.RData"))

	data_filter[i,1] <- length(grid_tmp_lt5y[, "lt5y"])
	data_filter[i,2] <- length(grid_tmp_lt5y[which(grid_tmp_lt5y[, "lt5y"]>= 100), "lt5y"])
	data_filter[i,3] <- length(grid_tmp_lt5y[which(grid_tmp_lt5y[, "lt5y"]>= 500), "lt5y"])
	data_filter[i,4] <- length(grid_tmp_lt5y[which(grid_tmp_lt5y[, "lt5y"]>= 1000), "lt5y"])

}

write.csv(data_filter, file="./Results/Tables/PopulationDensityGridCells.csv")

################################################################################

# Country-specific predictors from the regional model

load("./Results/GAM_fit_phenom_Regional/Regional.RData")

fit_table <- data.frame(value=rep(NA,10), se=rep(NA,10))
fit_table$value <- round(summary(fit_phenom)$p.coeff[1:10], 4)
fit_table$se <- round(summary(fit_phenom)$se[1:10], 4)

write.csv(fit_table, file="./Results/Tables/RegionalModelPredictors.csv")

################################################################################