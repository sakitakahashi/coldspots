library(descr)
library(ggplot2)
library(RColorBrewer)

# Read in the master list of country codes
codes <- as.data.frame(read.csv("./Data/Master_List.csv"))

################################################################################

# Get age distribution of survey participants by country

dat_AgeDist <- NULL
for(i in 1:nrow(codes)) {
	
	# Load the manipulated data
	load(paste0("./Data/Manipulated_data_for_GAM/", codes$DHS_code[i], ".RData"))

	# Get the binned_survey_age
	dat_AgeDist$country <- c(dat_AgeDist$country, rep(as.character(codes$Country[i]), times=length(dataInd_country$binned_survey_age)))
	dat_AgeDist$AgeDist <-  c(dat_AgeDist$AgeDist, dataInd_country$binned_survey_age)
	
}

dat_AgeDist <- as.data.frame(dat_AgeDist)
save(dat_AgeDist, file="./Results/AgeDistribution/All_countries.RData")

################################################################################

# This is to make an ROC-type curve of age by country

dat_AgeDist <- NULL
for(i in 1:nrow(codes)) {
	
	# Load the manipulated data
	load(paste0("./Data/Manipulated_data_for_GAM/", codes$DHS_code[i], ".RData"))

	# Get the binned_survey_age counts
	binned_survey_age_counts <- c(table(dataInd_country$binned_survey_age))
	month <- as.numeric(names(table(dataInd_country$binned_survey_age)))
	ROC <- cumsum(binned_survey_age_counts)/sum(binned_survey_age_counts)

	# Get the binned_survey_age
	dat_AgeDist$country <- c(dat_AgeDist$country, rep(as.character(codes$Country[i]), times=length(month)+1))
	dat_AgeDist$month <- c(dat_AgeDist$month, 0, month)
	dat_AgeDist$ROC <- c(dat_AgeDist$ROC, 0, ROC)
	
}

dat_AgeDist <- as.data.frame(dat_AgeDist)
save(dat_AgeDist, file="./Results/AgeDistribution/All_countries_ROC.RData")

################################################################################

# Get empirical CDF (eCDF) by country, and SIA lines

dat_SIA <- dat_eCDF <- NULL
for(i in 1:nrow(codes)) {
	
	# Load the manipulated data
	load(paste0("./Data/Manipulated_data_for_GAM/", codes$DHS_code[i], ".RData"))

	# Calculate the (weighted) empirical CDF
	ecdf <- NULL
	ecdf <- descr::crosstab(dataInd_country$binned_survey_age, dataInd_country$measles_status, plot=F, weight=dataInd_country$DHS_weight/1000000)
	ecdf$ages <- as.numeric(names(ecdf$t[,1]))
	ecdf$ecdf <- ecdf$t[,2]/rowSums(ecdf$t)

	# Save the data
	dat_eCDF$x <- c(dat_eCDF$x, ecdf$ages)
	dat_eCDF$y <- c(dat_eCDF$y, ecdf$ecdf)
	dat_eCDF$z <- c(dat_eCDF$z, rep(codes$Country[i], times=length(ecdf$ages)))
	
	# Get endpoints and national/sub-national status of each SIA
	SIA_lb <- SIA_ub <- SIA_national <- NULL

	for(j in 1:length(all_SIAs)) {
	
		# Min and max ages of eligibility for the SIA
		ages <- dataInd_country$survey_age_ub_mos[dataInd_country$dob_ub %in% seq(as.Date(dataSIA_country$dob_youngest[j]), as.Date(dataSIA_country$dob_oldest[j]), by="-1 day")]
		SIA_lb[j] <- min(ages)
		SIA_ub[j] <- max(ages)
	
		# National or sub-national?
		if(dataSIA_country[j,"Campaign_name"] %in% which_SIAs_subnational)
			SIA_national[j] <- 0
			
		else
			SIA_national[j] <- 1
			
	}
	
	# Save the data
	dat_SIA$lb <- c(dat_SIA$lb, SIA_lb)
	dat_SIA$ub <- c(dat_SIA$ub, SIA_ub)
	dat_SIA$national <- c(dat_SIA$national, SIA_national)
	dat_SIA$country <- c(dat_SIA$country, rep(codes$Country[i], times=length(SIA_lb)))
	
}

dat_eCDF <- as.data.frame(dat_eCDF)
dat_eCDF$zz <- codes$Country[match(dat_eCDF$z, rownames(codes))]
dat_SIA <- as.data.frame(dat_SIA)
dat_SIA$zz <- codes$Country[match(dat_SIA$country, rownames(codes))]
save(dat_eCDF, dat_SIA, file="./Results/eCDF/All_countries.RData")

################################################################################