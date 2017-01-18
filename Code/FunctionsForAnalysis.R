################################################################################

# Function to jitter a long/lat point by the amount determined by urban_rural
# Output: a new long/lat point
lat_long_distance_jitter <- function(lat1, long1, urban_rural) {
    
	# earths radius in Km
	R <- 6371
 
    # convert to radians
    lat1 <- lat1*pi/180
    long1 <- long1*pi/180
	
	## URBAN: jitter up to 2 km
	if(urban_rural=="U") {
	
		# jitter on the radian scale
		lat2 <- lat1 + rnorm(1,0,0.00008)
		long2 <- long1 + rnorm(1,0,0.00008)
	 
		# get distance
		d <- sin(long1)*sin(long2)+cos(long1)*cos(long2)*cos(abs(lat1-lat2))
		d <- acos(min(1,d))
		distance <- R*d
	
		
	}

	## RURAL: jitter up to 5 km
	if(urban_rural=="R") {
	
		# jitter on the radian scale
		lat2 <- lat1 + rnorm(1,0,0.00018)
		long2 <- long1 + rnorm(1,0,0.00018)
	 
		# get distance
		d <- sin(long1)*sin(long2)+cos(long1)*cos(long2)*cos(abs(lat1-lat2))
		d <- acos(min(1,d))
		distance <- R*d
			
	}
	
	# Convert back to long/lat
	lat2 <- 180 * lat2 / pi
	long2 <- 180 * long2 / pi
	
	return(c(long2, lat2, distance))

}

################################################################################

# Function to get country-specific data; need to do this first!
# Outputs: RData file containing dataInd and dataClust (removed dead + NA measles vaccine status)
getDataByInd_Cluster <- function(codes, input_country) {
  
  # Get the appropriate country codes
  XX_YYYY <- as.character(codes$DHS_code[which(codes$Country==input_country)])

  # Read STATA data into R (this is a data.frame)
  dataDHS <- read.dta(paste0("./Data/DHS/", XX_YYYY, "/", XX_YYYY, ".DTA"), convert.factors=TRUE)
  
  # Read DBF data into R (this is a data.frame)
  dbf <- read.dbf(paste0("./Data/DHS/", XX_YYYY, "/", XX_YYYY, ".dbf"), as.is=FALSE)
  
  # Check to see that the STATA and DBF data line up for DHS clusters
  # meaning that all of the DHS clusters in the STATA data are contained in the DBF data
  # It's okay if there are extra in the DBF file, 
  # as long as you don't have any FALSE here (i.e., Kenya)
  table(unique(dataDHS$v001) %in% dbf$DHSCLUST)
  
  # Get the cluster(s) not in the STATA dataset, if any
  omit <- dbf$DHSCLUST[!(dbf$DHSCLUST %in% unique(dataDHS$v001))]
  omit
  
  # Make individual-level dataset, and populate the individual id numbers
  dataInd <- data.frame(ind_id=1:nrow(dataDHS))
  rownames(dataInd) <- 1:nrow(dataInd)
  
  # Get the DHS cluster for each observation in the DHS (these are integers). 
  # Won't be in ascending order for some (i.e., Burundi, Rwanda), but that's ok: 
  # this will be TRUE if it is indeed ascending
  dataInd$cluster_id <- dataDHS$v001
  !is.unsorted(dataInd$cluster_id)
  head(dataInd)
  
  # Make cluster-data dataset, and populate the cluster id numbers
  dataClust <- data.frame(cluster_id=as.numeric(as.character(dbf$DHSCLUST)))
  rownames(dataClust) <- 1:nrow(dataClust)
  
  # Check that the DBF clusters are in ascending order (this should ALWAYS be TRUE)
  !is.unsorted(dataClust$cluster_id, strictly=TRUE)
  
  # Get longitude and latitude coordinates in decimal degrees of each DHS cluster
  dataClust$long <- as.numeric(as.character(dbf$LONGNUM))
  dataClust$lat <- as.numeric(as.character(dbf$LATNUM))
  
  # Are any of the DHS clusters missing decimal degree coordinates?
  length(dataClust$long[which(dataClust$long==0)])
  length(dataClust$lat[which(dataClust$lat==0)])
  
  # Replace 0's with NA because that means data could not be fully verified
  dataClust$long[which(dataClust$long==0)] <- NA
  dataClust$lat[which(dataClust$lat==0)] <- NA
  
  # Explore the regional DBF data a bit more
  table(dbf$DHSREGNA) # DHS region name (this is a factor)
  table(dbf$DHSREGCO) # DHS region code (these are integers or numerics)
  
  table(dbf$ADM1NAME) # adm1 division name (this is a factor)
  table(dbf$ADM1DHS) # adm1 division code (these are integers or numerics)
  
  table(dataDHS$v024) # STATA data region name (this is a factor)
  
  # Check to see that the STATA and DBF data line up for region 
  # (either 1 or 2: they don't have to)
  # Two different tests, depending on if there are omitted clusters in the STATA 
  # dataset or not
  all.equal(sort(tolower(levels(dataDHS$v024))), sort(tolower(levels(dbf$DHSREGNA))))
  all.equal(sort(tolower(levels(dataDHS$v024))), sort(tolower(levels(dbf$ADM1NAME))))
  
  all.equal(sort(tolower(levels(dataDHS$v024))), 
            sort(tolower(unique(as.character(dbf$DHSREGNA[-omit])))))
  all.equal(sort(tolower(levels(dataDHS$v024))), 
            sort(tolower(unique(as.character(dbf$ADM1NAME[-omit])))))
  
  # Get region (type 1) of each DHS cluster
  dataClust$region1_name <- as.character(dbf$DHSREGNA)
  dataClust$region1_id <- as.numeric(as.character(dbf$DHSREGCO))
  
  # Get region (type 2) of each DHS cluster
  dataClust$region2_name <- as.character(dbf$ADM1NAME)
  dataClust$region2_id <- as.numeric(as.character(dbf$ADM1DHS))
    
  # Create color codes for region1 and region2
  region1_key <- data.frame(region1_name=dataClust$region1_name, 
                            region1_id=dataClust$region1_id)
  region1_key <- region1_key[!duplicated(region1_key),]
  region1_key <- region1_key[order(region1_key$region1_id),]
  
  region2_key <- data.frame(region2_name=dataClust$region2_name, 
                            region2_id=dataClust$region2_id)
  region2_key <- region2_key[!duplicated(region2_key),]
  region2_key <- region2_key[order(region2_key$region2_id),]
  
  region1_key$region1_color <- 1:nrow(region1_key)
  region2_key$region2_color <- 1:nrow(region2_key)
  
  # Get the other region (type 1/2) for each region (type 2/1)
  region1_key$region2_name <- dataClust$region2_name[match(region1_key$region1_name, dataClust$region1_name)]
  region2_key$region1_name <- dataClust$region1_name[match(region2_key$region2_name, dataClust$region2_name)]
  
  # Generate new id codes for coloring, since the old id's might not match up 
  # (i.e., Tanzania)
  dataClust$region1_color <- region1_key$region1_color[match(dataClust$region1_id, 
                                                             region1_key$region1_id)]
  dataClust$region2_color <- region2_key$region2_color[match(dataClust$region2_id, 
                                                             region2_key$region2_id)]
  
  # Get urban/rural status of each DHS cluster (R=1=rural, U=2=urban) (this is a factor)
  dataClust$urban_rural <- as.character(dbf$URBAN_RURA)
  dataClust$urban_rural_cex <- NA
  dataClust$urban_rural_cex[which(dataClust$urban_rural=="U")] <- 0.75
  dataClust$urban_rural_cex[which(dataClust$urban_rural=="R")] <- 1.5
  
  # Get DHS survey date
  survey_date <- as.Date(sprintf("%d-%d-%d", dataDHS$v007, dataDHS$v006, dataDHS$v016), format="%Y-%m-%d")
  
  # Manually change the survey dates in the Ethiopian calendar to the Gregorian calendar
  if(input_country=="Ethiopia"){
  
	# Change anything with a 31 to a 30
	survey_date[which(substr(survey_date, 9, 10)=="31")] <- survey_date[which(substr(survey_date, 9, 10)=="31")]-1
	
	greg_dates <- c(seq(as.Date("2010-12-27"), length.out=length(seq(as.Date("2003-04-18"), as.Date("2003-05-30"), by="1 day")), by="1 day"),
					seq(as.Date("2011-02-08"), length.out=length(seq(as.Date("2003-06-01"), as.Date("2003-07-30"), by="1 day")), by="1 day"),
					seq(as.Date("2011-04-09"), length.out=length(seq(as.Date("2003-08-01"), as.Date("2003-08-30"), by="1 day")), by="1 day"),
					as.Date("2011-05-15"), as.Date("2011-05-22"), as.Date("2011-05-23"), as.Date("2011-05-24"))		
	
	survey_date <- greg_dates[match(survey_date, sort(unique(survey_date)))]
	
  }

  dataInd$survey_date <- survey_date

  # Manually change the dob in the Ethiopian calendar to the Gregorian calendar 
  # (since we just have the month and year, this is easy!)
  if(input_country=="Ethiopia"){
    
	  # Get the Ethiopian CMC of dob
	  Eth_CMC <- dataDHS$b3
		
	  # Convert to Gregorian CMC 
	  Greg_CMC <- Eth_CMC + 92
		
	  # Get Gregorian year
	  Greg_yr <- 1900 + floor((Greg_CMC-1)/12)
		
	  # Get Gregorian month
	  Greg_mo <- Greg_CMC - 12*(Greg_yr - 1900)
	  
	  # Adjust the DHS dates to reflect this
	  dataDHS$b2 <- Greg_yr
	  dataDHS$b1 <- Greg_mo
    
  }
  
  # Get lower bound (lb) of DOB of child (assume born on the first of the month)	
  dob_lb <- as.Date(sprintf("%d-%d-01", dataDHS$b2, dataDHS$b1), format="%Y-%m-%d")
  dataInd$dob_lb <- dob_lb
  
  # Match up lb and ub dates
  dob_key <- data.frame(dob_lb=seq(min(dob_lb), max(dob_lb), by="1 month"), 
                        dob_ub=(seq(min(dob_lb), max(dob_lb)+31, by="1 month")-1)[-1])
  dob_key
  
  # Get upper bound (ub) of DOB of child (assume born on the last of the month)
  dob_ub <- dob_key$dob_ub[match(dob_lb, dob_key$dob_lb)]
  dataInd$dob_ub <- dob_ub
  
  # Get lb and ub age at DHS survey (in months)
  dataInd$survey_age_lb_mos <- as.numeric(survey_date-dob_ub)/(365.25/12)
  dataInd$survey_age_ub_mos <- as.numeric(survey_date-dob_lb)/(365.25/12)
  
  # Get age at death (in months)
  dataInd$death_age_mos <- dataDHS$b7
  
  # Estimate proportion vaccinated against measles
  table(dataDHS$h9, exclude=NULL)
  
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
  
  table(h9tmp, exclude=NULL)		
  
  dataInd$measles_status <- rep(NA, length(h9tmp))
  dataInd$measles_status[h9tmp==0] <- 0
  dataInd$measles_status[h9tmp>0 & h9tmp<4] <- 1
  
  summary(dataInd$measles_status)	
  
  # If day of measles vaccination is missing (h9d), assume occurred on the 15th of the month 
  # (and convert the numeric to integer) - these are the one(s) that are missing a day (i.e., DRC)
  table(dataDHS$h9d[dataDHS$h9d>31], exclude=NULL)
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
  measles_date <- as.Date(measles_date)

  # Manually change the survey dates in the Ethiopian calendar to the Gregorian calendar
  if(input_country=="Ethiopia"){
  
	# Change anything with a 31 to a 30
	measles_date[which(substr(measles_date, 9, 10)=="31")] <- measles_date[which(substr(measles_date, 9, 10)=="31")]-1
	summary(measles_date)
	
	# Make a vector of consecutive days in the Ethiopian calendar between min(measles_date) and max(measles_date)
	# Skipping leap years (so assuming only 365 days each year: 12x30+5) 
	# This is a little buggy since the dates in the DHS are questionable, but should be within the correct week
	YYYY <- c(rep(1998, 136), rep(1999, 365), rep(2000, 366), rep(2001, 365), rep(2002, 365), rep(2003, 220))
	MM <- c(rep(8,11), rep(9:12,each=30), rep(13, 5), 
			rep(1:12, each=30), rep(13, 5), 
			rep(1:12, each=30), rep(13, 6), 
			rep(c(rep(1:12, each=30), rep(13, 5)), length=950))
	DD <- c(20:30, rep(1:30, times=4), 1:5, 
			rep(1:30, times=12), 1:5,
			rep(1:30, times=12), 1:6,
			rep(c(rep(1:30, times=12), 1:5), length=950))
	eth_dates_complete <- paste(YYYY, "-", sprintf( "%02d", MM), "-", sprintf( "%02d", DD), sep="")
	min(eth_dates_complete)
	max(eth_dates_complete)
	
	# Get the corresponding Gregorian dates
	greg_dates <- seq(as.Date("2006-04-28"), as.Date("2011-04-18"), by="1 day") 

	measles_date <- greg_dates[match(as.character(measles_date), eth_dates_complete)]
	
  }

  dataInd$measles_date <- measles_date
  
  # Get lb and ub age at measles vaccination (in months)
  dataInd$measles_age_lb_mos <- as.numeric(measles_date-dob_ub)/(365.25/12)
  dataInd$measles_age_ub_mos <- as.numeric(measles_date-dob_lb)/(365.25/12)
  
  dataInd$DHS_weight <- dataDHS$v005

  # Remove kids who died (those with death_age_mos values that are not NA)
  dataInd <- dataInd[is.na(dataInd$death_age_mos),]
  
  # Remove the column for death_age_mos
  dataInd <- subset(dataInd, select=-death_age_mos)

  # Remove kids with NA as measles_status
  dataInd <- dataInd[!is.na(dataInd$measles_status),]  
  
  save(dataInd, dataClust, region1_key, region2_key,
       file=paste0("./Data/Manipulated_data/", XX_YYYY, ".RData"))
  
}

################################################################################

# Function to clean up the csv of SIA information
# (Downloaded from part 7: http://www.who.int/immunization/monitoring_surveillance/data/en/)
# If need to manually clean some things, do it at the end of this function (see Ethiopia example)
# Outputs: SIA start/end date (translates to dob of elig kids), regions, age range
getCleanSIAs <- function(codes, dataInd, dataSIA, input_country) {
	
	# Get the appropriate country codes
	XX_YYYY <- as.character(codes$DHS_code[which(codes$Country==input_country)])
	ZZZ <- as.character(codes$ADM_code[which(codes$Country==input_country)])	
	
	# If Extent is known to be National campaign, set Regions equal to 0
	dataSIA$Regions[dataSIA$Extent=="National"] <- 0
	
	# If there isn't any information on Regions, assume National campaign and set Regions equal to 0
	dataSIA$Regions[is.na(dataSIA$Regions)] <- 0
	
	# Format the date
	dataSIA$Start_Date <- as.Date(dataSIA$Start_Date, format="%m/%d/%Y")
	dataSIA$End_Date <- as.Date(dataSIA$End_Date, format="%m/%d/%Y")
  
	# Translate the Age_Group column to useable age range of each SIA campaign
	dataSIA$age_ub_mos <- dataSIA$age_lb_mos <- NA
  
	for(i in 1:nrow(dataSIA)){
    
		if(dataSIA$Age_Group[i]=="0-11 M"){
			dataSIA$age_lb_mos[i] <- 0
			dataSIA$age_ub_mos[i] <- 11} 	

		if(dataSIA$Age_Group[i]=="0-59 M"){
			dataSIA$age_lb_mos[i] <- 0
			dataSIA$age_ub_mos[i] <- 59} 		  
		  
		if(dataSIA$Age_Group[i]=="0 M-14 Y"){
			dataSIA$age_lb_mos[i] <- 0
			dataSIA$age_ub_mos[i] <- 179} # 12*15-1	
			
		if(dataSIA$Age_Group[i]=="6-59 M"){
			dataSIA$age_lb_mos[i] <- 6
			dataSIA$age_ub_mos[i] <- 59}
		
		if(dataSIA$Age_Group[i]=="6 M-9 Y"){ 
			dataSIA$age_lb_mos[i] <- 6
			dataSIA$age_ub_mos[i] <- 119} # 12*10-1	

		if(dataSIA$Age_Group[i]=="6 M-10 Y"){ 
			dataSIA$age_lb_mos[i] <- 6
			dataSIA$age_ub_mos[i] <- 131} # 12*11-1	
		
		if(dataSIA$Age_Group[i]=="6 M-14 Y"){
			dataSIA$age_lb_mos[i] <- 6
			dataSIA$age_ub_mos[i] <- 179} # 12*15-1
		
		if(dataSIA$Age_Group[i]=="6 M-15 Y"){
			dataSIA$age_lb_mos[i] <- 6
			dataSIA$age_ub_mos[i] <- 191} # 12*16-1
		  
		if(dataSIA$Age_Group[i]=="9-23 M"){
			dataSIA$age_lb_mos[i] <- 9
			dataSIA$age_ub_mos[i] <- 23} 
	
		if(dataSIA$Age_Group[i]=="9-47 M"){
			dataSIA$age_lb_mos[i] <- 9
			dataSIA$age_ub_mos[i] <- 47} 
		
		if(dataSIA$Age_Group[i]=="9-49 M"){
			dataSIA$age_lb_mos[i] <- 9
			dataSIA$age_ub_mos[i] <- 49} 
	  
		if(dataSIA$Age_Group[i]=="9-59 M"){
			dataSIA$age_lb_mos[i] <- 9
			dataSIA$age_ub_mos[i] <- 59} 
			  
		if(dataSIA$Age_Group[i]=="9 M-14 Y"){
			dataSIA$age_lb_mos[i] <- 9
			dataSIA$age_ub_mos[i] <- 179} # 12*15-1

		if(dataSIA$Age_Group[i]=="<15 Y"){ 
			dataSIA$age_lb_mos[i] <- 9	# Assuming starts at 9 months
			dataSIA$age_ub_mos[i] <- 179} # 12*15-1

		if(dataSIA$Age_Group[i]=="9 M-15 Y"){
			dataSIA$age_lb_mos[i] <- 9
			dataSIA$age_ub_mos[i] <- 191} # 12*16-1

		if(dataSIA$Age_Group[i]=="18-23 M"){
			dataSIA$age_lb_mos[i] <- 18
			dataSIA$age_ub_mos[i] <- 23} 
		  
		if(dataSIA$Age_Group[i]=="6-14 Y"){ # Assuming the 6 means 6 years
			dataSIA$age_lb_mos[i] <- 72	# 12*6
			dataSIA$age_ub_mos[i] <- 179} # 12*15-1		  

		if(dataSIA$Age_Group[i]=="7-14 Y"){ # Assuming the 7 means 7 years
			dataSIA$age_lb_mos[i] <- 84	# 12*7
			dataSIA$age_ub_mos[i] <- 179} # 12*15-1

		if(dataSIA$Age_Group[i]=="School-age"){ # Assuming this means 6y-14y
			dataSIA$age_lb_mos[i] <- 72	# 12*6
			dataSIA$age_ub_mos[i] <- 179} # 12*15-1
	  	  
	}	

	# Need to impute some dates here if NA...
	# Let's assume that if either the Start_Date or End_Date is unknown, the SIA lasted 7 days
	dataSIA$End_Date[is.na(dataSIA$End_Date)] <- dataSIA$Start_Date[is.na(dataSIA$End_Date)] + 7
	dataSIA$Start_Date[is.na(dataSIA$Start_Date)] <- dataSIA$End_Date[is.na(dataSIA$Start_Date)] - 7

	# Look at just your country of interest
	dataSIA_country <- dataSIA[dataSIA$Country==input_country,]

	# Remove SIAs where both the Start_Date and End_Date are unknown
	dataSIA_country <- dataSIA_country[!is.na(dataSIA_country$Start_Date) & !is.na(dataSIA_country$End_Date),]
	
	# Figure out the earliest end date & latest start date of SIA campaigns for the DHS
	
    # Get the earliest SIA campaign end date:
    # This is the dob of the oldest kid in the DHS
    earliest_end_date <- min(dataInd$dob_lb)
  
    # Get the latest SIA campaign start date:
    # This is the last survey date
    latest_start_date <- max(dataInd$survey_date) 
  
	# Decide whether or not an SIA campaign could have possibly had any age-eligible individuals in this country's DHS
	# This is just to first-pass remove the SIAs that were way in the past/future wrt the DHS date;
	# The actual SIAs that make it into the final analysis are decided in getRegionalData() or getNationalData() below
	any_eligible_by_age <- rep(0, times=nrow(dataSIA_country))
	
	for(i in 1:nrow(dataSIA_country)) {

			if(as.Date(dataSIA_country$End_Date[i]) >= as.Date(earliest_end_date) &
			   as.Date(dataSIA_country$Start_Date[i]) <= as.Date(latest_start_date))

				any_eligible_by_age[i] <- 1
	  
	  }
	  
	# Only keep the SIAs with non-zero eligibles
	dataSIA_country <- cbind(dataSIA_country, any_eligible_by_age)
	dataSIA_country <- dataSIA_country[dataSIA_country$any_eligible_by_age==1,]
  
	# Get age-based eligibility to each SIAs
	dataSIA_country$dob_youngest <- dataSIA_country$dob_oldest <- NA
 
	for(i in 1:nrow(dataSIA_country)){
  
		  # Youngest eligible individual would turn age_lb_mos on End_Date
		  dataSIA_country$dob_youngest[i] <- as.character(min(seq(dataSIA_country$End_Date[i], 
		  length=dataSIA_country$age_lb_mos[i]+1, by="-1 month")))
		  
		  # Oldest eligible individual would turn age_ub_mos on the day after Start_Date
		  dataSIA_country$dob_oldest[i] <-  as.character(min(seq(dataSIA_country$Start_Date[i], 
		  length=dataSIA_country$age_ub_mos[i]+2, by="-1 month"))+1)

	}

	# Figure out how the Regions column in dataSIA_country translates to region1 and region2 in the DHS data
	load(paste0("./Data/Manipulated_data/", codes$DHS_code[which(codes$Country==input_country)], ".RData"))

	for(i in 1:nrow(dataSIA_country)) {

		print(c(as.character(dataSIA_country$Regions[i]),
			agrep(as.character(dataSIA_country$Regions[i]), unique(dataClust$region1_name), 
			max=0.1, value=TRUE)[1],
			agrep(as.character(dataSIA_country$Regions[i]), unique(dataClust$region2_name), 
			max=0.1, value=TRUE)[1]))
	
		# Compare with region1
		dataSIA_country$region1_name[i] <- as.character(agrep(as.character(dataSIA_country$Regions[i]), 
			unique(dataClust$region1_name), max=0.1, value=TRUE)[1])

		# Compare with region2

		# Manually change "Mara" region in Tanzania because otherwise it'll break
		if(dataSIA_country$Country=="Tanzania" && as.character(dataSIA_country$Regions[i])=="Mara") {
		
			dataSIA_country$region2_name[i] <- "Mara"
			print(paste0("Changed to ", dataSIA_country$region2_name[i]))
		}
		
		else {
		
			dataSIA_country$region2_name[i] <- as.character(agrep(as.character(dataSIA_country$Regions[i]), 
				unique(dataClust$region2_name), max=0.1, value=TRUE)[1])

		}

		# Change the national ones back to National
		if(dataSIA_country$Regions[i]==0) {
			dataSIA_country$region1_name[i] <- 0
			dataSIA_country$region2_name[i] <- 0 }

	}
	
	# Name the SIAs
	SIA_number <- 1:nrow(dataSIA_country)
	dataSIA_country$Campaign_name <- paste0("SIA_", SIA_number)
	
	# If need to manually clean some things, do it here
	dataSIA_country$region2_name[dataSIA_country$Country=="Ethiopia" & is.na(dataSIA_country$region2_name)] <- 0
	
	return(dataSIA_country[,c("Country","Activity","Start_Date","End_Date","Extent",
							  "age_lb_mos", "age_ub_mos", "dob_oldest", "dob_youngest",
							  "Regions", "region1_name", "region2_name", "Campaign_name")])

}

################################################################################

# Function to get individual eligibility for SIA by age and location
# Outputs: dataInd with new indicator columns for each SIA (SIA names should match up perfectly with dataSIA_country)
getSIAelibilityInd <- function(dataInd, dataSIA_country, input_country, SIA_region_type) {

	# This will determine eligibility for each SIA for each individual in the DHS by age and location

	# Which type of region do the SIAs follow?
	tmp_name <- paste0("region", SIA_region_type, "_name")

	# Name the SIAs
	SIAs <- 1:nrow(dataSIA_country)
	dataInd[,paste0("SIA_", SIAs)] <- 0

	# Get eligibility by age and location (region)
	# Remember that a region name of 0 means that it's a National campaign (everyone is eligible wrt loc)
	for(i in 1:nrow(dataSIA_country)) {
	  
	  dob_oldest <- as.Date(dataSIA_country$dob_oldest[i])
	  dob_youngest <- as.Date(dataSIA_country$dob_youngest[i])
	  region <- dataSIA_country[i,tmp_name]
	  
	  for(j in 1:nrow(dataInd)) {
	  
	   age_indicator <- 0
	   loc_indicator <- 0
		
		if(as.Date(dataInd$dob_lb[j]) >= dob_oldest & as.Date(dataInd$dob_lb[j]) <= dob_youngest)
		   age_indicator <- 1
	   
		if(as.Date(dataInd$dob_ub[j]) >= dob_oldest & as.Date(dataInd$dob_ub[j]) <= dob_youngest)
		  age_indicator <- 1
		  
		if(region==0)
		  loc_indicator <- 1
			
		if(dataClust[dataClust$cluster_id==dataInd$cluster_id[j],tmp_name]==region)
		  loc_indicator <- 1

		dataInd[j,paste0("SIA_", SIAs[i])] <- age_indicator * loc_indicator
		
		rm(age_indicator, loc_indicator)
	  
	  }
	  
	  rm(dob_oldest, dob_youngest, region)
		
	}
	
	return(dataInd)
	
}

################################################################################

# Function to fill in NAs with the last non-NA value
# Output: fixed vector
fillNAgaps <- function(x, firstBack=FALSE) {
    ## NA's in a vector or factor are replaced with last non-NA values
    ## If firstBack is TRUE, it will fill in leading NA's with the first
    ## non-NA value. If FALSE, it will not change leading NA's.
    
    # If it's a factor, store the level labels and convert to integer
    lvls <- NULL
    if (is.factor(x)) {
        lvls <- levels(x)
        x    <- as.integer(x)
    }
 
    goodIdx <- !is.na(x)
 
    # These are the non-NA values from x only
    # Add a leading NA or take the first good value, depending on firstBack   
    if (firstBack)   goodVals <- c(x[goodIdx][1], x[goodIdx])
    else             goodVals <- c(NA,            x[goodIdx])

    # Fill the indices of the output vector with the indices pulled from
    # these offsets of goodVals. Add 1 to avoid indexing to zero.
    fillIdx <- cumsum(goodIdx)+1
    
    x <- goodVals[fillIdx]

    # If it was originally a factor, convert it back
    if (!is.null(lvls)) {
        x <- factor(x, levels=seq_along(lvls), labels=lvls)
    }

    x
}

################################################################################

# Helper function and function to draw a scale bar
# From: https://github.com/yusriy/reservoir_fluxes/blob/master/R/tools/tool_map_createscale.R
createScaleBar <- function(lon,lat,distanceLon,distanceLat,distanceLegend, dist.units = "km"){

    # First rectangle
    bottomRight <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distanceLon, dist.units = dist.units, model = "WGS84")
    topLeft <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distanceLat, dist.units = dist.units, model = "WGS84")
    rectangle <- cbind(lon=c(lon, lon, bottomRight[1,"long"], bottomRight[1,"long"], lon), lat = c(lat, topLeft[1,"lat"], topLeft[1,"lat"],lat, lat))
    rectangle <- data.frame(rectangle, stringsAsFactors = FALSE)
     
    # Second rectangle at right of the first rectangle
    bottomRight2 <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distanceLon*2, dist.units = dist.units, model = "WGS84")
    rectangle2 <- cbind(lon = c(bottomRight[1,"long"], bottomRight[1,"long"], bottomRight2[1,"long"], bottomRight2[1,"long"], bottomRight[1,"long"]), lat=c(lat, topLeft[1,"lat"], topLeft[1,"lat"], lat, lat))
    rectangle2 <- data.frame(rectangle2, stringsAsFactors = FALSE)
     
    # Now let's deal with the text
    onTop <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distanceLegend, dist.units = dist.units, model = "WGS84")
    onTop2 <- onTop3 <- onTop
    onTop2[1,"long"] <- bottomRight[1,"long"]
    onTop3[1,"long"] <- bottomRight2[1,"long"]
     
    legend <- rbind(onTop, onTop2, onTop3)
    legend <- data.frame(cbind(legend, text = c(0, distanceLon, distanceLon*2)), stringsAsFactors = FALSE, row.names = NULL)
    return(list(rectangle = rectangle, rectangle2 = rectangle2, legend = legend))
	
}

scaleBar <- function(lon, lat, distanceLon, distanceLat, distanceLegend, dist.unit = "km", rec.fill = "white", rec.colour = "black", rec2.fill = "black", rec2.colour = "black", legend.colour = "black", legend.size = 3, orientation = FALSE, arrow.length = 500, arrow.distance = 300, arrow.North.size = 6){

    laScaleBar <- createScaleBar(lon = lon, lat = lat, distanceLon = distanceLon, distanceLat = distanceLat, distanceLegend = distanceLegend, dist.unit = dist.unit)
	
    # First rectangle
    rectangle1 <- geom_polygon(data = laScaleBar$rectangle, aes(x = lon, y = lat), fill = rec.fill, colour = rec.colour)
     
    # Second rectangle
    rectangle2 <- geom_polygon(data = laScaleBar$rectangle2, aes(x = lon, y = lat), fill = rec2.fill, colour = rec2.colour)
     
    # Legend
    scaleBarLegend <- annotate("text", 
							   label = paste(laScaleBar$legend[,"text"], dist.unit, sep=" "), 
							   x = laScaleBar$legend[,"long"], 
							   y = laScaleBar$legend[,"lat"], 
							   size = legend.size, 
							   colour = legend.colour)
     
    res <- list(rectangle1, rectangle2, scaleBarLegend)
     
    if(orientation){# Add an arrow pointing North
        coordsArrow <- createOrientationArrow(scaleBar = laScaleBar, length = arrow.length, distance = arrow.distance, dist.unit = dist.unit)
        arrow <- list(geom_segment(data = coordsArrow$res, aes(x = x, y = y, xend = xend, yend = yend)), annotate("text", label = "N", x = coordsArrow$coordsN[1,"x"], y = coordsArrow$coordsN[1,"y"], size = arrow.North.size, colour = "black"))
        res <- c(res, arrow)
    }
    return(res)
}

################################################################################