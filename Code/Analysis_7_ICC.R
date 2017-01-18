library(maptools)
library(maps)
library(mgcv)
library(splancs)
library(rgeos)
library(grid)
library(sp)
library(rgdal)	
library(geoR)
library(lme4)

# Source code for analysis
source("./Code/FunctionsForAnalysis.R")

# Read in the master list of country codes
codes <- as.data.frame(read.csv("./Data/Master_List.csv"))

# Re-name years, as necessary
levels(codes$Year)[levels(codes$Year)=="2010-2011"] <- 2010
levels(codes$Year)[levels(codes$Year)=="2013-2014"] <- 2013

################################################################################

# Estimate ICC by adm and country, restricting to a max of 3 adm levels

ICC_restrict_to_3levels <- function(i, binned_survey_age) {

	# Load the GAM ouput
	load(paste0("./Results/GAMtoMeanAndSE_Della/GAMtoMeanAndSE_", codes$DHS_code[i], "_6-60m.RData"))
	
	# Which survey age do we want the mean of?
	which_data <- paste0("Mean_", binned_survey_age, "m")
	
	# Save only the first two columns (lat,long) of grid_tmp_lt5y as well as the mean probability at the binned survey age
	grid_tmp <- data.frame(long=grid_tmp_lt5y[,"long"], lat=grid_tmp_lt5y[,"lat"], z=grid_tmp_lt5y[,which_data])
		
	# Which one is the finest?
	max_ADM_level <- 3
	if(i==9) max_ADM_level <- 2
	if(i==10) max_ADM_level <- 2

	# Download all the possible ADM boundary shapefiles for a country (some of these might not exist for certain datasets)
	ADM_code <- codes$ADM_code[i]	
	adm0 <- readShapeSpatial(paste0("./Data/ADM/", ADM_code, "_adm/", ADM_code, "_adm0.shp"))
	adm1 <- readShapeSpatial(paste0("./Data/ADM/", ADM_code, "_adm/", ADM_code, "_adm1.shp"))
	adm2 <- readShapeSpatial(paste0("./Data/ADM/", ADM_code, "_adm/", ADM_code, "_adm2.shp"))
	if(max_ADM_level>2) adm3 <- readShapeSpatial(paste0("./Data/ADM/", ADM_code, "_adm/", ADM_code, "_adm3.shp"))

	# Pick the finest data set as the base, since from there we can just use the ADM data to tell us which of the higher level groups the data is in
	dat_finest <- eval(parse(text=paste0("adm", max_ADM_level)))
	ID_finest <- paste0("ID_", max_ADM_level)
	
	# How many different polygons are there in this data set?
	# print(length(dat_finest@data[,ID_finest]))

	for(j in 1:length(dat_finest@data[,ID_finest])) {
	
		# Let's take each polygon, and figure out which grid cells are in it
		def <- inout(pts=as.points(grid_tmp$long, grid_tmp$lat), 
					 poly=dat_finest@polygons[[j]]@Polygons[[1]]@coords, 
					 bound=TRUE)	
	
		# Assign those a value of 1 for that polygon
		grid_tmp[,paste0("adm", max_ADM_level, "_", j)] <- ifelse(def, 1, 0)
		
		rm(def)
	
	}
	
	# print(table(rowSums(grid_tmp[,4:ncol(grid_tmp)])))

	# Plot it
	# windows()
	# plot(dat_finest)
	
	# In 0 polygons
	# points(grid_tmp$long[which(rowSums(grid_tmp[,4:ncol(grid_tmp)])==0)], grid_tmp$lat[which(rowSums(grid_tmp[,4:ncol(grid_tmp)])==0)], col="red", pch=16) 
	
	# In more than one polygon
	# points(grid_tmp$long[which(rowSums(grid_tmp[,4:ncol(grid_tmp)])>1)], grid_tmp$lat[which(rowSums(grid_tmp[,4:ncol(grid_tmp)])>1)], col="blue", pch=16) 
	
	# In exactly 1 polygon	
	# points(grid_tmp$long[which(rowSums(grid_tmp[,4:ncol(grid_tmp)])==1)], grid_tmp$lat[which(rowSums(grid_tmp[,4:ncol(grid_tmp)])==1)], col="green", pch=16)

	# Let's just look at the adm_max polygons
	a <- grid_tmp[,4:ncol(grid_tmp)]
	a <- as.matrix(a)
	
	# In order to make the finest scale adm to the rest...
	adm_key <- dat_finest@data[,grep("ID", names(dat_finest@data))]
	
	# Look at the non-zero values
	b <- which(a!=0, arr.ind=T)
		
	# If a grid cell is in >1 polygon, assign it to the one with LOWER representation:
	
	# Which cells have been assigned to more than one polygon?
	cells_more_than_once <- unique(b[duplicated(b[,1]),1])
	
	# If there is at least one duplicated cell, proceed
	if(length(cells_more_than_once)>0) {
	
		for(k in 1:length(cells_more_than_once)) {

			# For a given cell, which polygons was it assigned to?
			which_polygons <- c(b[which(b[,1]==cells_more_than_once[k]),2])
			
			# What's the prevalence of each of these polygon?
			prevalence_each_polygon <- colMeans(a)[which_polygons]
			
			# Which polygon has the lowest prevalence?			
			polygons_to_remove <- names(which(prevalence_each_polygon!=min(prevalence_each_polygon)))
	
			# Let's get rid of the non-lowest prevalence ones
			grid_tmp[cells_more_than_once[k], polygons_to_remove] <- 0
			
		}		
	
	}
	
	# And then assign each to a numerical adm_max ID
	grid_tmp[,paste0("adm", max_ADM_level)] <- b[match(as.numeric(rownames(grid_tmp)), b[,1]), 2]
	grid_final <- grid_tmp[, !grepl(paste0("adm", max_ADM_level, "_"), colnames(grid_tmp))]
	
	# If a grid cell is in 0 polygons, assign it to the one before/after
	grid_final[,paste0("adm", max_ADM_level)] <- fillNAgaps(grid_final[,paste0("adm", max_ADM_level)], firstBack=TRUE)

	# Add the other adm levels
	max_ADM_level_placeholder <- max_ADM_level
	while(max_ADM_level_placeholder>0) {
		grid_final[,paste0("adm", max_ADM_level_placeholder-1)] <- adm_key[match(grid_final[,paste0("adm", max_ADM_level)], adm_key[,ID_finest]), paste0("ID_", max_ADM_level_placeholder-1)]
		max_ADM_level_placeholder <- max_ADM_level_placeholder-1
		# print(max_ADM_level_placeholder)
	}
	
	# Fit a random intercepts model to this
	if(max_ADM_level==2) model <- lmer(z ~ 1 + (1|adm1) + (1|adm2), data=grid_final, REML=FALSE)
	else if(max_ADM_level==3) model <- lmer(z ~ 1 + (1|adm1) + (1|adm2) + (1|adm3), data=grid_final, REML=FALSE)
	
	# Extract the intra-class correlations (and plot them)
	varnames <- as.data.frame(summary(model)[13][[1]])$grp
	vars <- as.data.frame(summary(model)[13][[1]])$vcov
	
	# plot(vars/sum(vars), xaxt="n", xlab="predictor", ylab="intra-class correlation", cex=2, pch=15, col="firebrick", ylim=c(0,1))
	# axis(1, at=1:length(varnames), labels=varnames)	

	# This is for the region-level analysis
	grid_final$country_for_regional_analysis <- i
	
	save(grid_final, model, varnames, vars, file=paste0("./Results/ICC/", codes$DHS_code[i], "_", binned_survey_age, "m_3LevelsMax.RData"))
	
}

for(i in 1:nrow(codes)) ICC_restrict_to_3levels(i=i, binned_survey_age=24)

################################################################################

# To tabulate ICC

ICC_table <- NULL

binned_survey_age <- 24
for(i in 1:nrow(codes)) {

	# Load the ICC ouput
	load(paste0("./Results/ICC/", codes$DHS_code[i], "_", binned_survey_age, "m_3LevelsMax.RData"))
	
	ICC <- round(vars/sum(vars), 4)
	
	# Populate the table
	ICC_table$country[i] <- as.character(codes$Country[i])
	
	ICC_table$adm1[i] <- ICC[(length(ICC)-1)]
	
	ICC_table$adm2[i] <- ICC[(length(ICC)-2)]
	
	if(length(ICC)==3) ICC_table$adm3[i] <- NA
	else ICC_table$adm3[i] <- ICC[(length(ICC)-3)]
	
	ICC_table$Residual[i] <- ICC[length(ICC)]

}

ICC_table_df <- as.data.frame(ICC_table)

write.csv(ICC_table_df, file="./Results/Tables/ICC.csv")

################################################################################

# Estimate ICC of the entire region

grid_final_all <- NULL

# Loop over the countries
for(i in 1:nrow(codes)) {

	# Load the ICC ouput
	load(paste0("./Results/ICC/", codes$DHS_code[i], "_24m_3LevelsMax.RData"))

	# Append to grid_final_all
	grid_final_all$z <- c(grid_final_all$z, grid_final$z)
	grid_final_all$country <- c(grid_final_all$country, grid_final$country_for_regional_analysis)
	# grid_final_all$year <- c(grid_final_all$year, rep(as.character(codes$Year[i]), times=length(grid_final$z)))

	rm(grid_final)
	
}

# Save as data frame
grid_final_all_df <- as.data.frame(grid_final_all)
	
# Run the model
model <- lmer(z ~ 1 + (1|country), data=grid_final_all_df, REML=FALSE)
# print(summary(model))

# Extract the intra-class correlations
varnames <- as.data.frame(summary(model)[13][[1]])$grp
vars <- as.data.frame(summary(model)[13][[1]])$vcov

ICC <- round(vars/sum(vars), 4)
print(c(ICC[(length(ICC)-1):1], ICC[length(ICC)]))

################################################################################

# To tabulate bootstrap CIs for ICCs: by country

# Loop over the countries
for(i in 1:nrow(codes)) {

	age <- 24
	load(paste0("./Results/ICC/", codes$DHS_code[i], "_", age, "m_3LevelsMax.RData"))

	# Function to estimate ICC, if max Adm==3
	calc.icc_maxAdm3 <- function(y) {
	
		sumy <- summary(y)
		c(sumy$varcor$adm1, sumy$varcor$adm2, sumy$varcor$adm3, sumy$sigma^2)/sum(c(sumy$varcor$adm1, sumy$varcor$adm2, sumy$varcor$adm3, sumy$sigma^2))
	
	}
	
	# Function to estimate ICC, if max Adm==2
	calc.icc_maxAdm2 <- function(y) {
	
		sumy <- summary(y)
		c(sumy$varcor$adm1, sumy$varcor$adm2, sumy$sigma^2)/sum(c(sumy$varcor$adm1, sumy$varcor$adm2, sumy$sigma^2))
	
	}
	
	# Bootstrap CI with 1000 iterations
	if(codes$max_ADM_level[i]>2) 
		boot.icc <- bootMer(model, calc.icc_maxAdm3, nsim=1000)
		
	else 
		boot.icc <- bootMer(model, calc.icc_maxAdm2, nsim=1000)

	ICC_CI <- round(apply(boot.icc$t, 2, quantile, c(0.025, 0.975)), 4)

	save(boot.icc, ICC_CI, file=paste0("./Results/ICC/", codes$DHS_code[i], "_", age, "m_3LevelsMax_ICC_CI.RData"))
	
}

################################################################################

# To obtain bootstrap CIs for ICCs: for the entire region

grid_final_all <- NULL

# Loop over the countries
for(i in 1:nrow(codes)) {

	# Load the ICC ouput
	load(paste0("./Results/ICC/", codes$DHS_code[i], "_24m_3LevelsMax.RData"))

	# Append to grid_final_all
	grid_final_all$z <- c(grid_final_all$z, grid_final$z)
	grid_final_all$country <- c(grid_final_all$country, grid_final$country_for_regional_analysis)
	# grid_final_all$year <- c(grid_final_all$year, rep(as.character(codes$Year[i]), times=length(grid_final$z)))

	rm(grid_final)
	
}

# Save as data frame
grid_final_all_df <- as.data.frame(grid_final_all)
	
# Run the model
model <- lmer(z ~ 1 + (1|country), data=grid_final_all_df, REML=FALSE)

# Function to estimate ICC of country
calc.icc_regional <- function(y) {

	sumy <- summary(y)
	(sumy$varcor$country[1]) / (sumy$varcor$country[1] + sumy$sigma^2)

}

# Bootstrap CI with 1000 iterations
boot.icc <- bootMer(model, calc.icc_regional, nsim=1000)
quantile(boot.icc$t, c(0.025, 0.975))

################################################################################