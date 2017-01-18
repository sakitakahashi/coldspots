################################################################################

## Description: GAM mean/SE map at binned_survey_age
## Description: or delta SE, the difference between SE at binned_survey_age and SE at 12 months
## Sub-national SIAs: not included in prediction
## Spatial scale: single country
## MeanOrSE: "Mean", "SE"

getSmoothedMap_noSubSIA_Single <- function(Country, binned_survey_age, MeanOrSE="Mean", deltaSEfrom12=TRUE) {
	
	# Get the rest
	i <- as.numeric(rownames(codes))[which(Country==codes$Country)]
	
	# Placeholders
	DHS_code <- codes$DHS_code[i]
	DHS_name <- codes$DHS_name[i]
	ADM_code <- codes$ADM_code[i]
	
	# Load the Della output
	load(paste0("./Results/GAMtoMeanAndSE_Della/GAMtoMeanAndSE_", DHS_code,"_6-60m.RData"))

	# Get border lines to add
	adm0 <- readShapeSpatial(paste0("./Data/ADM/", ADM_code, "_adm/", ADM_code, "_adm0.shp"))
	
	# Plot	
	tmp <- ggplot()		
	
	# Which column are we plotting?
	which_data <- paste0(MeanOrSE, "_", binned_survey_age, "m")

	# Get water bodies
	water <- readShapeSpatial("./Data/waterbodies_africa/waterbodies_africa.shp")

	# For a map of means:
	if(MeanOrSE=="Mean"){
	
		# Legend name
		legend_name <- substitute(paste("Mean at ", binned_survey_age, "m"), list(binned_survey_age=binned_survey_age))
						  
		tmp <- tmp + 
		geom_polygon(data=g_df, mapping=aes(x=long, y=lat, group=group, fill=plogis(g_df[,which_data]))) +
		geom_contour(data=grid_tmp_lt5y, mapping=aes(x=long, y=lat, z=plogis(grid_tmp_lt5y[,which_data])), size=0.1, breaks=seq(0,1,by=0.05), colour="gray50") +
		scale_fill_gradientn(name=legend_name, 
							 limits=c(0,1), 	
							 breaks=c(0, 0.5, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1),
							 values=c(0, 0.5, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1),
							 colours=brewer.pal(10, "RdYlGn")) +
		geom_polygon(data=adm0, aes(x=long, y=lat, z=group), colour="gray40", fill=NA, cex=0.25) +
		guides(fill=guide_colorbar(barwidth=2, barheight=20))
				
	}
	
	# For a map of SEs:
	else if(MeanOrSE=="SE") {
			
		if(binned_survey_age==12 | deltaSEfrom12==FALSE) {
		
			# Legend name
			legend_name <- substitute(paste("SE at ", binned_survey_age, "m"), list(binned_survey_age=binned_survey_age))

			tmp <- tmp + 
			geom_polygon(data=g_df, mapping=aes(x=long, y=lat, group=group, fill=g_df[,which_data])) +
			geom_contour(data=grid_tmp_lt5y, mapping=aes(x=long, y=lat, z=grid_tmp_lt5y[,which_data]), size=0.1, breaks=seq(0,1,by=0.05), colour="gray50") +
			scale_fill_gradientn(name=legend_name,
								 limits=c(0.05,0.9),
								 breaks=seq(0.1,0.85,by=0.15),
								 colours=rev(brewer.pal(10, "Spectral"))) + 
			geom_polygon(data=adm0, aes(x=long, y=lat, z=group), colour="gray40", fill=NA, cex=0.25) +
			guides(fill=guide_colorbar(barwidth=2, barheight=20))	
		
		}

		else if (deltaSEfrom12==TRUE) {

			# Legend name
			legend_name <- bquote(paste(Delta, "SE at ", .(binned_survey_age), "m"))

			tmp <- tmp + 
			geom_polygon(data=g_df, mapping=aes(x=long, y=lat, group=group, fill=g_df[,which_data]-g_df[,"SE_12m"])) +
			geom_contour(data=grid_tmp_lt5y, mapping=aes(x=long, y=lat, z=grid_tmp_lt5y[,which_data]-grid_tmp_lt5y[,"SE_12m"]), size=0.1, breaks=seq(-0.05,0.3,by=0.01), colour="gray50") +
			scale_fill_gradient2(name=legend_name,
								 limits=c(-0.06, 0.3),
								 breaks=seq(-0.05, 0.3, by=0.05)) +
			geom_polygon(data=adm0, aes(x=long, y=lat, z=group), colour="gray40", fill=NA, cex=0.25) +
			guides(fill=guide_colorbar(barwidth=2, barheight=20))	
		
		}
		
	}
	
	# Add water in that country, if applicable
	water_in_country <- rgeos::gIntersection(water, adm0, drop_lower_td=TRUE)
	
	if(!is.null(water_in_country)) {
	
		# Plot water
		tmp <- tmp + geom_polygon(data=water_in_country, aes(x=long, y=lat, group=group), colour="gray40", size=0.2, fill="azure", alpha=1)
	
	}
		
	# Settings
	tmp <- tmp + coord_fixed() + 
			  theme(axis.line=element_blank(),
					axis.text.x=element_blank(),
					axis.text.y=element_blank(),
					axis.ticks=element_blank(),
					axis.title.x=element_blank(),
					axis.title.y=element_blank(),
					panel.background=element_blank(),
					panel.border=element_blank(),
					panel.grid.major=element_blank(),
					panel.grid.minor=element_blank(),
					plot.background=element_blank())
	
	# Change legend tick marks to dark
	g <- ggplotGrob(tmp)
	g$grobs[[8]][[1]][[1]][[1]][[5]]$gp$col <- "gray50"
	grid.draw(g)
	
}

## Description: GAM mean/SE map at binned_survey_age
## Description: or delta SE, the difference between SE at binned_survey_age and SE at 12 months
## Sub-national SIAs: not included in prediction
## Spatial scale: all countries
## MeanOrSE: "Mean", "SE"
## Note: this code is also used to make a movie

getSmoothedMap_noSubSIA <- function(binned_survey_age, MeanOrSE="Mean", deltaSEfrom12=TRUE, movie=FALSE) {

	# For plot
	tmp <- ggplot()
	
	# Which column are we plotting?
	which_data <- paste0(MeanOrSE, "_", binned_survey_age, "m")

	# Get water bodies
	water <- readShapeSpatial("./Data/waterbodies_africa/waterbodies_africa.shp")

	# Get the outline of all the countries
	adm0_all <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[1], "_adm/", codes$ADM_code[1], "_adm0.shp"))
	for(i in 2:nrow(codes)) {

		## Get border lines to add
		adm0 <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[i], "_adm/", codes$ADM_code[i], "_adm0.shp"))
		adm0_all <- rbind(adm0_all, adm0, makeUniqueIDs=TRUE)
	
	}
	
	# Aggregate data across all countries
	g_df_all <- NULL
	for(i in 1:nrow(codes)) {
	
		# Load the Della output
		load(paste0("./Results/GAMtoMeanAndSE_Della/GAMtoMeanAndSE_",codes$DHS_code[i],"_6-60m.RData"))
		
		# Aggregate the cells using rbind
		g_df$country <- i
		g_df$group_final <- paste0(g_df$country, "_", g_df$group)
		g_df_all <- rbind(g_df_all, g_df)
		
		# Create a new data frame for contour
		assign(paste0("contour_", i), grid_tmp_lt5y)
		
	}
	
	# For a map of means:
	if(MeanOrSE=="Mean"){
		
		# Legend name
		legend_name <- substitute(paste("Mean at ", binned_survey_age, "m"), list(binned_survey_age=binned_survey_age))

		tmp <- tmp + 
			geom_polygon(data=g_df_all, mapping=aes(x=long, y=lat, group=group_final, fill=plogis(g_df_all[,which_data]))) + 
			scale_fill_gradientn(name=legend_name, 
									 limits=c(0,1), 	
									 breaks=c(0, 0.5, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1),
									 values=c(0, 0.5, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1),
									 colours=brewer.pal(10, "RdYlGn")) +					 
			guides(fill=guide_colorbar(barwidth=2, barheight=20))

		# Add the contour lines
		for(i in 1:nrow(codes)) {
			
			data <- get(paste0("contour_", i))[,c("long", "lat", which_data)]
			names(data) <- c("long", "lat", "z")

			tmp <- tmp + 
			geom_contour(data=data, mapping=aes(x=long, y=lat, z=plogis(z)), size=0.1, breaks=seq(0,1,by=0.05), colour="gray50") 
		
		}

	}
		
	# For a map of SEs:
	else if(MeanOrSE=="SE") {
			
		if(binned_survey_age==12 | deltaSEfrom12==FALSE) {

			# Legend name
			legend_name <- substitute(paste("SE at ", binned_survey_age, "m"), list(binned_survey_age=binned_survey_age))

			tmp <- tmp + 
				geom_polygon(data=g_df_all, mapping=aes(x=long, y=lat, group=group_final, fill=g_df_all[,which_data])) + 
				scale_fill_gradientn(name=legend_name, 
									 limits=c(0.05,0.9),
									 breaks=seq(0.1,0.85,by=0.15),
									 colours=rev(brewer.pal(10, "Spectral"))) + 			 
				guides(fill=guide_colorbar(barwidth=2, barheight=20))

			# Add the contour lines
			for(i in 1:nrow(codes)) {
				
				data <- get(paste0("contour_", i))[,c("long", "lat", which_data)]
				names(data) <- c("long", "lat", "z")

				tmp <- tmp + 
				geom_contour(data=data, mapping=aes(x=long, y=lat, z=z), size=0.1, breaks=seq(0,1,by=0.05), colour="gray50") 
			
			}

		}		

		else if (deltaSEfrom12==TRUE) {

			# Legend name
			legend_name <- bquote(paste(Delta, "SE at ", .(binned_survey_age), "m"))
			
			tmp <- tmp + 
			geom_polygon(data=g_df_all, mapping=aes(x=long, y=lat, group=group_final, fill=g_df_all[,which_data]-g_df_all[,"SE_12m"])) +
			scale_fill_gradient2(name=legend_name,
								 limits=c(-0.06, 0.3),
								 breaks=seq(-0.05, 0.3, by=0.05)) +
			guides(fill=guide_colorbar(barwidth=2, barheight=10))	

			# Add the contour lines
			for(i in 1:nrow(codes)) {
				
				data <- get(paste0("contour_", i))[,c("long", "lat", which_data)]
				names(data) <- c("long", "lat", "z")
				
				data_base <- get(paste0("contour_", i))[,c("long", "lat", "SE_12m")]
				names(data_base) <- c("long", "lat", "z")
				
				data$z <- data$z - data_base$z

				tmp <- tmp + 
				geom_contour(data=data, mapping=aes(x=long, y=lat, z=z), size=0.1, breaks=seq(-0.05,0.3,by=0.01), colour="gray50") 
			
			}
			
		}

	}
	
	# Add the adm0 boundaries
	tmp <- tmp + geom_polygon(data=adm0_all, aes(x=long, y=lat, z=group), colour="gray40", fill=NA, cex=0.25)

	# Add water in all the countries
	water_in_region <- rgeos::gIntersection(water, adm0_all, drop_lower_td=TRUE)
		
	# Plot water
	tmp <- tmp + geom_polygon(data=water_in_region, aes(x=long, y=lat, group=group), colour="gray40", size=0.2, fill="azure", alpha=1)	
	
	# Get capitals
	cg.cap <- c(-4.316667, 15.316667)
	ug.cap <- c(0.313611, 32.581111)
	ke.cap <- c(-1.283333, 36.816667)
	rw.cap <- c(-1.943889, 30.059444)
	by.cap <- c(-3.383333, 29.366667)
	tz.cap <- c(-6.173056, 35.741944)
	za.cap <- c(-15.416667, 28.283333)
	mi.cap <- c(-13.98972222, 33.78861111)
	mo.cap <- c(-25.966667, 32.583333)
	zi.cap <- c(-17.863889, 31.029722)
	cap <- rbind(cg.cap,ug.cap,ke.cap,rw.cap,by.cap,tz.cap,za.cap,mi.cap,mo.cap,zi.cap)
	cap <- data.frame(cap)
	colnames(cap) <- c("lat", "long")

	# If you're not plotting the movie
	if(movie==FALSE) {
		
		# Plot with capitals
		tmp <- tmp + 
				  geom_point(data=cap, aes(long,lat), cex=1.2, fill="plum", col="gray30", pch=21) +
				  coord_fixed() + 
				  theme(axis.line=element_blank(),
						axis.text.x=element_blank(),
						axis.text.y=element_blank(),
						axis.ticks=element_blank(),
						axis.title.x=element_blank(),
						axis.title.y=element_blank(),
						panel.background=element_blank(),
						panel.border=element_blank(),
						panel.grid.major=element_blank(),
						panel.grid.minor=element_blank(),
						plot.background=element_blank())
	
	}
	
	# If you are plotting the movie
	if(movie==TRUE) {
	
		# Plot with capitals and ticker at the bottom
		tmp <- tmp +  
				  geom_point(data=cap, aes(long,lat), cex=1.2, fill="plum", col="gray30", pch=21) +
				  coord_fixed() + 
				  theme(axis.line=element_blank(),
						axis.text.x=element_blank(),
						axis.text.y=element_blank(),
						axis.ticks=element_blank(),
						axis.title.x=element_blank(),
						axis.title.y=element_blank(),
						panel.background=element_blank(),
						panel.border=element_blank(),
						panel.grid.major=element_blank(),
						panel.grid.minor=element_blank(),
						plot.background=element_blank()) +	  
	 
			# Horizontal bar
			geom_segment(aes(x=9.9, xend=30.1, y=-30, yend=-30), size=1.3) +
			 
			# X-axis ticks	
			geom_segment(aes(x=9.96, xend=9.96, y=-30.5, yend=-29.91), size=1.3) +
			geom_segment(aes(x=seq(10,30,length=60)[12], xend=seq(10,30,length=60)[12], y=-30.5, yend=-30), size=1.3) +
			geom_segment(aes(x=seq(10,30,length=60)[24], xend=seq(10,30,length=60)[24], y=-30.5, yend=-30), size=1.3) +
			geom_segment(aes(x=seq(10,30,length=60)[36], xend=seq(10,30,length=60)[36], y=-30.5, yend=-30), size=1.3) +
			geom_segment(aes(x=seq(10,30,length=60)[48], xend=seq(10,30,length=60)[48], y=-30.5, yend=-30), size=1.3) +
			geom_segment(aes(x=30, xend=30, y=-30.5, yend=-30), size=1.3) +

			# X-axis labels:seq(10,30,length=60)[c(1,12,24,36,48,60)]
			geom_text(aes(label="01m", x=10, y=-31)) +
			geom_text(aes(label="12m", x=seq(10,30,length=60)[12], y=-31)) +
			geom_text(aes(label="24m", x=seq(10,30,length=60)[24], y=-31)) +		  
			geom_text(aes(label="36m", x=seq(10,30,length=60)[36], y=-31)) +		  
			geom_text(aes(label="48m", x=seq(10,30,length=60)[48], y=-31)) +
			geom_text(aes(label="60m", x=30, y=-31)) +

			# Color bar
			geom_segment(data=LAB,aes(x=xmin,xend=xmax,y=-29.9,yend=-29),size=1.5,col="seagreen")	
				
	}

	# Change legend tick marks to dark
	g <- ggplotGrob(tmp)
	g$grobs[[8]][[1]][[1]][[1]][[5]]$gp$col <- "gray50"
	grid.draw(g)
	
}

################################################################################

## Description: coldspots map
## Description: grid cells with < cutoff_percent mean coverage at binned_survey_age
## Sub-national SIAs: not included in prediction
## Spatial scale: all countries
## Note: this code is also used to make a movie

getSmoothedMap_noSubSIA_ColdSpots <- function(binned_survey_age=12, cutoff_percent=0.8, movie=FALSE) {

	# For plot
	tmp <- ggplot()
	
	# Which column are we plotting?
	which_data <- paste0("Mean_", binned_survey_age, "m")
	
	# Get water bodies
	water <- readShapeSpatial("./Data/waterbodies_africa/waterbodies_africa.shp")
	
	# Aggregate data across all countries
	g_df_all <- NULL
	for(i in 1:nrow(codes)) {
	
		# Load the Della output
		load(paste0("./Results/GAMtoMeanAndSE_Della/GAMtoMeanAndSE_",codes$DHS_code[i],"_6-60m.RData"))
		
		# Aggregate the cells using rbind
		g_df$country <- i
		g_df$group_final <- paste0(g_df$country, "_", g_df$group)
		g_df_all <- rbind(g_df_all, g_df)
		
	}
	
	# Plot the blank grid cells
	tmp <- tmp + geom_polygon(data=g_df_all, mapping=aes(x=long, y=lat, group=group_final), fill=NA)
	
	# Find and fill the coldspots, aka cells with less than cutoff_percent coverage
	g_df_all$coverage_at_cutoff_age <- plogis(g_df_all[,which_data])
	
	tmp <- tmp + geom_polygon(data=g_df_all[which(g_df_all$coverage_at_cutoff_age < cutoff_percent),], mapping=aes(x=long, y=lat, group=group_final, fill=factor(dummy))) + scale_fill_manual(name="Coldspots", values="gray75", labels=paste0("< ", cutoff_percent*100, "% at ", sprintf("%02d", binned_survey_age), "m")) + theme(legend.position=c(0.2,0.2), legend.background=element_rect(fill=NA, size=0.1, linetype="solid", colour="black"))
	
	# Get the outline of all the countries (adm0 and adm1)
	adm0_all <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[1], "_adm/", codes$ADM_code[1], "_adm0.shp"))
	adm1_all <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[1], "_adm/", codes$ADM_code[1], "_adm1.shp"))	
	
	for(i in 2:nrow(codes)) {

		# Get border lines to add
		adm0 <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[i], "_adm/", codes$ADM_code[i], "_adm0.shp"))
		adm1 <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[i], "_adm/", codes$ADM_code[i], "_adm1.shp"))
		
		# Stitch together
		adm0_all <- rbind(adm0_all, adm0, makeUniqueIDs=TRUE)
		adm1_all <- rbind(adm1_all, adm1, makeUniqueIDs=TRUE)
		
	}
		
	# Add the adm1 boundaries
	tmp <- tmp + geom_polygon(data=adm1_all, aes(x=long, y=lat, z=group), colour="gray60", fill=NA, cex=0.1)	

	# Add the adm0 boundaries
	tmp <- tmp + geom_polygon(data=adm0_all, aes(x=long, y=lat, z=group), colour="gray40", fill=NA, cex=0.25)

	# Add water in all the countries
	water_in_region <- rgeos::gIntersection(water, adm0_all, drop_lower_td=TRUE)
		
	# Plot water
	tmp <- tmp + geom_polygon(data=water_in_region, aes(x=long, y=lat, group=group), colour="gray40", size=0.2, fill="azure", alpha=1)
	
	# Get capitals
	cg.cap <- c(-4.316667, 15.316667)
	ug.cap <- c(0.313611, 32.581111)
	ke.cap <- c(-1.283333, 36.816667)
	rw.cap <- c(-1.943889, 30.059444)
	by.cap <- c(-3.383333, 29.366667)
	tz.cap <- c(-6.173056, 35.741944)
	za.cap <- c(-15.416667, 28.283333)
	mi.cap <- c(-13.98972222, 33.78861111)
	mo.cap <- c(-25.966667, 32.583333)
	zi.cap <- c(-17.863889, 31.029722)
	cap <- rbind(cg.cap,ug.cap,ke.cap,rw.cap,by.cap,tz.cap,za.cap,mi.cap,mo.cap,zi.cap)
	cap <- data.frame(cap)
	colnames(cap) <- c("lat", "long")

	# Ticker marks
	WHERE <- seq(10, 30, length=60)
	LAB <- data.frame(xmin=WHERE[1:binned_survey_age], xmax=WHERE[1:binned_survey_age])		

	# If you're not plotting the movie
	if(movie==FALSE) {
		
		# Plot with capitals
		tmp <- tmp + 
				  geom_point(data=cap, aes(long,lat), cex=1.2, fill="plum", col="gray30", pch=21) +
				  coord_fixed() + 
				  theme(axis.line=element_blank(),
						axis.text.x=element_blank(),
						axis.text.y=element_blank(),
						axis.ticks=element_blank(),
						axis.title.x=element_blank(),
						axis.title.y=element_blank(),
						panel.background=element_blank(),
						panel.border=element_blank(),
						panel.grid.major=element_blank(),
						panel.grid.minor=element_blank(),
						plot.background=element_blank())

	}
	
	# If you are plotting the movie
	if(movie==TRUE) {
	
		# Plot with capitals and ticker at the bottom
		tmp <- tmp +  
				  geom_point(data=cap, aes(long,lat), cex=1.2, fill="plum", col="gray30", pch=21) +
				  coord_fixed() + 
				  theme(axis.line=element_blank(),
						axis.text.x=element_blank(),
						axis.text.y=element_blank(),
						axis.ticks=element_blank(),
						axis.title.x=element_blank(),
						axis.title.y=element_blank(),
						panel.background=element_blank(),
						panel.border=element_blank(),
						panel.grid.major=element_blank(),
						panel.grid.minor=element_blank(),
						plot.background=element_blank()) +	  
	 
			# Horizontal bar
			geom_segment(aes(x=9.9, xend=30.1, y=-30, yend=-30), size=1.3) +
			 
			# X-axis ticks	
			geom_segment(aes(x=9.96, xend=9.96, y=-30.5, yend=-29.91), size=1.3) +
			geom_segment(aes(x=seq(10,30,length=60)[12], xend=seq(10,30,length=60)[12], y=-30.5, yend=-30), size=1.3) +
			geom_segment(aes(x=seq(10,30,length=60)[24], xend=seq(10,30,length=60)[24], y=-30.5, yend=-30), size=1.3) +
			geom_segment(aes(x=seq(10,30,length=60)[36], xend=seq(10,30,length=60)[36], y=-30.5, yend=-30), size=1.3) +
			geom_segment(aes(x=seq(10,30,length=60)[48], xend=seq(10,30,length=60)[48], y=-30.5, yend=-30), size=1.3) +
			geom_segment(aes(x=30, xend=30, y=-30.5, yend=-30), size=1.3) +

			# X-axis labels:seq(10,30,length=60)[c(1,12,24,36,48,60)]
			geom_text(aes(label="01m", x=10, y=-31)) +
			geom_text(aes(label="12m", x=seq(10,30,length=60)[12], y=-31)) +
			geom_text(aes(label="24m", x=seq(10,30,length=60)[24], y=-31)) +		  
			geom_text(aes(label="36m", x=seq(10,30,length=60)[36], y=-31)) +		  
			geom_text(aes(label="48m", x=seq(10,30,length=60)[48], y=-31)) +
			geom_text(aes(label="60m", x=30, y=-31)) +

			# Color bar
			geom_segment(data=LAB,aes(x=xmin,xend=xmax,y=-29.9,yend=-29),size=1.5,col="seagreen")	
				
	}
		
	print(tmp)
	
}

## Description: coldspots significance map
## Description: the proportion of the CDF at binned_survey_age that is < cutoff_percent
## Description: essentially, how confident are we that a cell is a coldspot?
## Sub-national SIAs: not included in prediction
## Spatial scale: all countries

getSmoothedMap_noSubSIA_Coldspots_Significance <- function(binned_survey_age=12, cutoff_percent=0.8) {

	# For plot
	tmp <- ggplot()
	
	# Get water bodies
	water <- readShapeSpatial("./Data/waterbodies_africa/waterbodies_africa.shp")
	
	# Aggregate data across all countries
	g_df_all <- NULL
	for(i in 1:nrow(codes)) {
	
		# Load the Della output
		load(paste0("./Results/GAMtoMeanAndSE_Della/GAMtoMeanAndSE_",codes$DHS_code[i],"_6-60m.RData"))
		
		# Aggregate the cells using rbind
		g_df$country <- i
		g_df$group_final <- paste0(g_df$country, "_", g_df$group)
		g_df_all <- rbind(g_df_all, g_df)
	
		# Create a new data frame for contour
		assign(paste0("contour_", i), grid_tmp_lt5y)
		
	}
	
	# Plot the blank grid cells
	tmp <- tmp + geom_polygon(data=g_df_all, mapping=aes(x=long, y=lat, group=group_final), fill=NA)
	
	# Figure out the proportion of the distribution of the mean that's below cutoff_percent at binned_survey_age (on logit scale), at each cell
	mean_data <- paste0("Mean_", binned_survey_age, "m")
	se_data <- paste0("SE_", binned_survey_age, "m")
	g_df_all$prop_below_cutoff_percent_at_cutoff_age <- pnorm(qlogis(cutoff_percent), mean=g_df_all[, mean_data], sd=g_df_all[, se_data])
		
	# Plot the significance map
	tmp <- tmp + geom_polygon(data=g_df_all, mapping=aes(x=long, y=lat, group=group_final, fill=prop_below_cutoff_percent_at_cutoff_age)) +
		   scale_fill_gradientn(name=paste0("Proportion of\ndistribution of mean\n< ", cutoff_percent*100, "% at ", sprintf("%02d", binned_survey_age), "m"), 
								 colours=colorRampPalette(rev(brewer.pal(9, "RdYlBu")))(50), 
								 limits=c(0,1)) +
		   guides(fill=guide_colorbar(barwidth=2, barheight=20))
	
	# Get an outline of where (mean) coldspots are, and add to plot	
	for(i in 1:nrow(codes)) {
	
		data <- get(paste0("contour_", i))[,c("long", "lat", mean_data)]
		names(data) <- c("long", "lat", "z")

		tmp <- tmp + 
		geom_contour(data=data, mapping=aes(x=long, y=lat, z=plogis(z)), size=0.1, breaks=0.8, colour="black") 

	}

	# Get the outline of all the countries (adm0 and adm1)
	adm0_all <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[1], "_adm/", codes$ADM_code[1], "_adm0.shp"))
	adm1_all <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[1], "_adm/", codes$ADM_code[1], "_adm1.shp"))	
	
	for(i in 2:nrow(codes)) {

		# Get border lines to add
		adm0 <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[i], "_adm/", codes$ADM_code[i], "_adm0.shp"))
		adm1 <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[i], "_adm/", codes$ADM_code[i], "_adm1.shp"))
		
		# Stitch together
		adm0_all <- rbind(adm0_all, adm0, makeUniqueIDs=TRUE)
		adm1_all <- rbind(adm1_all, adm1, makeUniqueIDs=TRUE)
		
	}
		
	# Add the adm1 boundaries
	tmp <- tmp + geom_polygon(data=adm1_all, aes(x=long, y=lat, z=group), colour="gray60", fill=NA, cex=0.1)	

	# Add the adm0 boundaries
	tmp <- tmp + geom_polygon(data=adm0_all, aes(x=long, y=lat, z=group), colour="gray40", fill=NA, cex=0.25)

	# Add water in all the countries
	water_in_region <- rgeos::gIntersection(water, adm0_all, drop_lower_td=TRUE)
		
	# Plot water
	tmp <- tmp + geom_polygon(data=water_in_region, aes(x=long, y=lat, group=group), colour="gray40", size=0.2, fill="azure", alpha=1)
	
	# Get capitals
	cg.cap <- c(-4.316667, 15.316667)
	ug.cap <- c(0.313611, 32.581111)
	ke.cap <- c(-1.283333, 36.816667)
	rw.cap <- c(-1.943889, 30.059444)
	by.cap <- c(-3.383333, 29.366667)
	tz.cap <- c(-6.173056, 35.741944)
	za.cap <- c(-15.416667, 28.283333)
	mi.cap <- c(-13.98972222, 33.78861111)
	mo.cap <- c(-25.966667, 32.583333)
	zi.cap <- c(-17.863889, 31.029722)
	cap <- rbind(cg.cap,ug.cap,ke.cap,rw.cap,by.cap,tz.cap,za.cap,mi.cap,mo.cap,zi.cap)
	cap <- data.frame(cap)
	colnames(cap) <- c("lat", "long")

	# Ticker marks
	WHERE <- seq(10, 30, length=60)
	LAB <- data.frame(xmin=WHERE[1:binned_survey_age], xmax=WHERE[1:binned_survey_age])		

	# Plot with capitals
	tmp <- tmp + 
			  geom_point(data=cap, aes(long,lat), cex=1.2, fill="plum", col="gray30", pch=21) +
			  coord_fixed() + 
			  theme(axis.line=element_blank(),
					axis.text.x=element_blank(),
					axis.text.y=element_blank(),
					axis.ticks=element_blank(),
					axis.title.x=element_blank(),
					axis.title.y=element_blank(),
					panel.background=element_blank(),
					panel.border=element_blank(),
					panel.grid.major=element_blank(),
					panel.grid.minor=element_blank(),
					plot.background=element_blank())
	
	# Change legend tick marks to dark
	g <- ggplotGrob(tmp)
	g$grobs[[8]][[1]][[1]][[1]][[5]]$gp$col <- "gray50"
	grid.draw(g)
	
}

################################################################################

## Description: map of the prevalence of grid cells as coldspots, between lower_age and upper_age
## Description: grid cells with < cutoff_percent mean coverage, at ages between lower_age and upper_age
## Sub-national SIAs: not included in prediction
## Spatial scale: all countries

getSmoothedMap_noSubSIA_Coldspots_Prevalence <- function(lower_age, upper_age, cutoff_percent) {

	# For plot
	tmp <- ggplot()
	
	# Which column are we plotting?
	which_data <- paste0("Mean_", lower_age:upper_age, "m")
	
	# Get water bodies
	water <- readShapeSpatial("./Data/waterbodies_africa/waterbodies_africa.shp")
	
	# Aggregate data across all countries
	g_df_all <- NULL
	for(i in 1:nrow(codes)) {
	
		# Load the Della output
		load(paste0("./Results/GAMtoMeanAndSE_Della/GAMtoMeanAndSE_",codes$DHS_code[i],"_6-60m.RData"))
		
		# Aggregate the cells using rbind
		g_df$country <- i
		g_df$group_final <- paste0(g_df$country, "_", g_df$group)
		g_df_all <- rbind(g_df_all, g_df)
		
	}
	
	# Plot the blank grid cells
	tmp <- tmp + geom_polygon(data=g_df_all, mapping=aes(x=long, y=lat, group=group_final), fill=NA)
	
	# Calculate the proportion of months that each grid cell spends as a coldspot
	logit_coverage_at_included_ages <- as.matrix(g_df_all[,which_data])
	coverage_at_included_ages <- plogis(logit_coverage_at_included_ages)
	indicator_for_coldspot <- ifelse(coverage_at_included_ages < cutoff_percent, 1, 0)
	total_months <- ncol(indicator_for_coldspot)
	prevalence_coldspot <- rowSums(indicator_for_coldspot)/total_months
	g_df_all$coverage_at_cutoff_age <- prevalence_coldspot
	
	# Legend name	
	legend_name <- paste("Coldspot prevalence\n", lower_age, "-", upper_age, "m\n", "All cells", sep="")
		
	# Plot
	tmp <- tmp + geom_polygon(data=g_df_all, mapping=aes(x=long, y=lat, group=group_final, fill=coverage_at_cutoff_age)) + scale_fill_gradient(name=legend_name, limits=c(0,1), low="white", high="red", na.value="green") + guides(fill=guide_colorbar(barwidth=2, barheight=10))
	
	# Get the outline of all the countries (adm0 and adm1)
	adm0_all <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[1], "_adm/", codes$ADM_code[1], "_adm0.shp"))
	adm1_all <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[1], "_adm/", codes$ADM_code[1], "_adm1.shp"))	
	
	for(i in 2:nrow(codes)) {

		# Get border lines to add
		adm0 <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[i], "_adm/", codes$ADM_code[i], "_adm0.shp"))
		adm1 <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[i], "_adm/", codes$ADM_code[i], "_adm1.shp"))
		
		# Stitch together
		adm0_all <- rbind(adm0_all, adm0, makeUniqueIDs=TRUE)
		adm1_all <- rbind(adm1_all, adm1, makeUniqueIDs=TRUE)
		
	}
		
	# Add the adm1 boundaries
	tmp <- tmp + geom_polygon(data=adm1_all, aes(x=long, y=lat, z=group), colour="gray60", fill=NA, cex=0.1)
	
	# Add the adm0 boundaries
	tmp <- tmp + geom_polygon(data=adm0_all, aes(x=long, y=lat, z=group), colour="gray40", fill=NA, cex=0.25)

	# Add water in all the countries
	water_in_region <- rgeos::gIntersection(water, adm0_all, drop_lower_td=TRUE)
		
	# Plot water
	tmp <- tmp + geom_polygon(data=water_in_region, aes(x=long, y=lat, group=group), colour="gray40", size=0.2, fill="azure", alpha=1)
	
	# Get capitals
	cg.cap <- c(-4.316667, 15.316667)
	ug.cap <- c(0.313611, 32.581111)
	ke.cap <- c(-1.283333, 36.816667)
	rw.cap <- c(-1.943889, 30.059444)
	by.cap <- c(-3.383333, 29.366667)
	tz.cap <- c(-6.173056, 35.741944)
	za.cap <- c(-15.416667, 28.283333)
	mi.cap <- c(-13.98972222, 33.78861111)
	mo.cap <- c(-25.966667, 32.583333)
	zi.cap <- c(-17.863889, 31.029722)
	cap <- rbind(cg.cap,ug.cap,ke.cap,rw.cap,by.cap,tz.cap,za.cap,mi.cap,mo.cap,zi.cap)
	cap <- data.frame(cap)
	colnames(cap) <- c("lat", "long")

	# Plot with capitals
	tmp <- tmp + 
		geom_point(data=cap, aes(long,lat), cex=1.2, fill="plum", col="gray30", pch=21) +
		coord_fixed() + 
		theme(axis.line=element_blank(),
						axis.text.x=element_blank(),
						axis.text.y=element_blank(),
						axis.ticks=element_blank(),
						axis.title.x=element_blank(),
						axis.title.y=element_blank(),
						panel.background=element_blank(),
						panel.border=element_blank(),
						panel.grid.major=element_blank(),
						panel.grid.minor=element_blank(),
						plot.background=element_blank())

	# Change legend tick marks to dark
	g <- ggplotGrob(tmp)
	g$grobs[[8]][[1]][[1]][[1]][[5]]$gp$col <- "gray50"
	grid.draw(g)
	
}

## Description: same as above, filtering population size of under 5 year olds (cutoff_popsize)
## Sub-national SIAs: not included in prediction
## Spatial scale: all countries

getSmoothedMap_noSubSIA_Coldspots_Prevalence_Pop <- function(lower_age, upper_age, cutoff_percent, cutoff_popsize) {

	# For plot
	tmp <- ggplot()
	
	# Which column are we plotting?
	which_data <- paste0("Mean_", lower_age:upper_age, "m")
	
	# Get water bodies
	water <- readShapeSpatial("./Data/waterbodies_africa/waterbodies_africa.shp")
	
	# Aggregate data across all countries
	g_df_all <- NULL
	for(i in 1:nrow(codes)) {
	
		# Load the Della output
		load(paste0("./Results/GAMtoMeanAndSE_Della/GAMtoMeanAndSE_",codes$DHS_code[i],"_6-60m.RData"))
		
		# Aggregate the cells using rbind
		g_df$country <- i
		g_df$group_final <- paste0(g_df$country, "_", g_df$group)
		g_df_all <- rbind(g_df_all, g_df)
		
	}
	
	# Plot the blank grid cells
	tmp <- tmp + geom_polygon(data=g_df_all, mapping=aes(x=long, y=lat, group=group_final), fill=NA)
	
	# Calculate the proportion of months that each grid cell spends as a coldspot
	logit_coverage_at_included_ages <- as.matrix(g_df_all[,which_data])
	coverage_at_included_ages <- plogis(logit_coverage_at_included_ages)
	indicator_for_coldspot <- ifelse(coverage_at_included_ages < cutoff_percent, 1, 0)
	total_months <- ncol(indicator_for_coldspot)
	prevalence_coldspot <- rowSums(indicator_for_coldspot)/total_months
	g_df_all$coverage_at_cutoff_age <- prevalence_coldspot
	
	# Get the number of people at each grid cell
	g_df_all_2 <- NULL
	for(i in 1:nrow(codes)) {
	
		# Load the Della output
		load(paste0("./Results/GAMtoPeople_Della/GAMtoPeople_",codes$DHS_code[i],"_6-60m.RData"))

		rowname <- match(g_df$dummy, g$dummy)
		g_df$lt5y <- grid_tmp_lt5y[rowname, "lt5y"]	
		
		# Aggregate the cells using rbind
		g_df$country <- i
		g_df$group_final <- paste0(g_df$country, "_", g_df$group)
		g_df_all_2 <- rbind(g_df_all_2, g_df)
		
	}

	# Let lt5y_multiplier be 0 if < cutoff_popsize, and 1 if > cutoff_popsize
	g_df_all$lt5y_mult <- ifelse(g_df_all_2$lt5y < cutoff_popsize, 0, 1)
	
	# Legend name
	legend_name <- paste("Coldspot prevalence\n", lower_age, "-", upper_age, "m\n", "Threshold: ", cutoff_popsize, sep="")
		
	# Plot
	tmp <- tmp + geom_polygon(data=g_df_all, mapping=aes(x=long, y=lat, group=group_final, fill=coverage_at_cutoff_age * lt5y_mult)) + scale_fill_gradient(name=legend_name, limits=c(0,1), low="white", high="red", na.value="green") + guides(fill=guide_colorbar(barwidth=2, barheight=10))
	
	# Get the outline of all the countries (adm0 and adm1)
	adm0_all <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[1], "_adm/", codes$ADM_code[1], "_adm0.shp"))
	adm1_all <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[1], "_adm/", codes$ADM_code[1], "_adm1.shp"))	
	
	for(i in 2:nrow(codes)) {

		# Get border lines to add
		adm0 <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[i], "_adm/", codes$ADM_code[i], "_adm0.shp"))
		adm1 <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[i], "_adm/", codes$ADM_code[i], "_adm1.shp"))
		
		# Stitch together
		adm0_all <- rbind(adm0_all, adm0, makeUniqueIDs=TRUE)
		adm1_all <- rbind(adm1_all, adm1, makeUniqueIDs=TRUE)
		
	}
		
	# Add the adm1 boundaries
	tmp <- tmp + geom_polygon(data=adm1_all, aes(x=long, y=lat, z=group), colour="gray60", fill=NA, cex=0.1)	
	
	# Add the adm0 boundaries
	tmp <- tmp + geom_polygon(data=adm0_all, aes(x=long, y=lat, z=group), colour="gray40", fill=NA, cex=0.25)

	# Add water in all the countries
	water_in_region <- rgeos::gIntersection(water, adm0_all, drop_lower_td=TRUE)
		
	# Plot water
	tmp <- tmp + geom_polygon(data=water_in_region, aes(x=long, y=lat, group=group), colour="gray40", size=0.2, fill="azure", alpha=1)
	
	# Get capitals
	cg.cap <- c(-4.316667, 15.316667)
	ug.cap <- c(0.313611, 32.581111)
	ke.cap <- c(-1.283333, 36.816667)
	rw.cap <- c(-1.943889, 30.059444)
	by.cap <- c(-3.383333, 29.366667)
	tz.cap <- c(-6.173056, 35.741944)
	za.cap <- c(-15.416667, 28.283333)
	mi.cap <- c(-13.98972222, 33.78861111)
	mo.cap <- c(-25.966667, 32.583333)
	zi.cap <- c(-17.863889, 31.029722)
	cap <- rbind(cg.cap,ug.cap,ke.cap,rw.cap,by.cap,tz.cap,za.cap,mi.cap,mo.cap,zi.cap)
	cap <- data.frame(cap)
	colnames(cap) <- c("lat", "long")

	# Plot with capitals
	tmp <- tmp + geom_point(data=cap, aes(long,lat), cex=1.2, fill="plum", col="gray30", pch=21) +
		coord_fixed() + 
		theme(axis.line=element_blank(),
						axis.text.x=element_blank(),
						axis.text.y=element_blank(),
						axis.ticks=element_blank(),
						axis.title.x=element_blank(),
						axis.title.y=element_blank(),
						panel.background=element_blank(),
						panel.border=element_blank(),
						panel.grid.major=element_blank(),
						panel.grid.minor=element_blank(),
						plot.background=element_blank())

	# Change legend tick marks to dark
	g <- ggplotGrob(tmp)
	g$grobs[[8]][[1]][[1]][[1]][[5]]$gp$col <- "gray50"
	grid.draw(g)
	
}

################################################################################

## Description: map of the number of people between [0, 60] months of age
## Description: assuming that monthly age bins are uniformly distributed
## Spatial scale: all countries

getSmoothedMap_WorldPop <- function() {

	# For plot
	tmp <- ggplot()

	# Get water bodies
	water <- readShapeSpatial("./Data/waterbodies_africa/waterbodies_africa.shp")

	for(i in 1:nrow(codes)) {
		
		# Load the Della output
		load(paste0("./Results/GAMtoPeople_Della/GAMtoPeople_",codes$DHS_code[i],"_6-60m.RData"))

		# Get border lines to add
		adm0 <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[i], "_adm/", codes$ADM_code[i], "_adm0.shp"))
		
		# Calculate the number of people between [0, 60] months of age
		rowname <- match(g_df$dummy, g$dummy)
		unvacc <- grid_tmp_lt5y[rowname, "lt5y"] #+ 1 # since we show on log scale
		g_df$unvacc <- unvacc
		
		# Legend name
		legend_name <- paste("Population size\n", "0-60m", sep="")
		
		# Plot
		tmp <- tmp + 
		geom_polygon(data=g_df, mapping=aes(x=long, y=lat, group=group, fill=unvacc)) +
		scale_fill_gradientn(name=legend_name, 
						 colours=colorRampPalette(rev(brewer.pal(11, "RdBu")))(50), 
						 trans="log",
						 na.value="royalblue4", # aka those with 0 people
						 limits=c(1,250000),
						 breaks=c(1,10,100,1000,10000,100000)) +
		geom_polygon(data=adm0, aes(x=long, y=lat, z=group), colour="gray40", fill=NA, cex=0.25) +
		guides(fill=guide_colorbar(barwidth=2, barheight=20))
				
	}

	# Get the outline of all the countries
	adm0_all <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[1], "_adm/", codes$ADM_code[1], "_adm0.shp"))
	for(i in 2:nrow(codes)) {

		# Get border lines to add
		adm0 <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[i], "_adm/", codes$ADM_code[i], "_adm0.shp"))
		adm0_all <- rbind(adm0_all, adm0, makeUniqueIDs=TRUE)
		
	}

	# Add water in all the countries
	water_in_region <- rgeos::gIntersection(water, adm0_all, drop_lower_td=TRUE)
		
	# Plot water
	tmp <- tmp + geom_polygon(data=water_in_region, aes(x=long, y=lat, group=group), colour="gray40", size=0.2, fill="azure", alpha=1)

	# Get capitals
	cg.cap <- c(-4.316667, 15.316667)
	ug.cap <- c(0.313611, 32.581111)
	ke.cap <- c(-1.283333, 36.816667)
	rw.cap <- c(-1.943889, 30.059444)
	by.cap <- c(-3.383333, 29.366667)
	tz.cap <- c(-6.173056, 35.741944)
	za.cap <- c(-15.416667, 28.283333)
	mi.cap <- c(-13.98972222, 33.78861111)
	mo.cap <- c(-25.966667, 32.583333)
	zi.cap <- c(-17.863889, 31.029722)
	cap <- rbind(cg.cap,ug.cap,ke.cap,rw.cap,by.cap,tz.cap,za.cap,mi.cap,mo.cap,zi.cap)
	cap <- data.frame(cap)
	colnames(cap) <- c("lat", "long")

	# Settings
	tmp <- tmp + coord_fixed() + 
		theme(axis.line=element_blank(),
						axis.text.x=element_blank(),
						axis.text.y=element_blank(),
						axis.ticks=element_blank(),
						axis.title.x=element_blank(),
						axis.title.y=element_blank(),
						panel.background=element_blank(),
						panel.border=element_blank(),
						panel.grid.major=element_blank(),
						panel.grid.minor=element_blank(),
						plot.background=element_blank())

	# Change legend tick marks to dark
	g <- ggplotGrob(tmp)
	g$grobs[[8]][[1]][[1]][[1]][[5]]$gp$col <- "gray50"
	grid.draw(g)
	
}

## Description: map of the number of unvaccinated people between [lower_age, upper_age] months of age
## Description: assuming that monthly age bins are uniformly distributed
## Description: the minimum lower_age here is 6 months (i.e., eligibility for the analysis)
## Sub-national SIAs: not included in prediction
## Spatial scale: all countries

getSmoothedMap_Unvaccinateds <- function(lower_age, upper_age) {

	# For plot
	tmp <- ggplot()

	# Get water bodies
	water <- readShapeSpatial("./Data/waterbodies_africa/waterbodies_africa.shp")

	for(i in 1:nrow(codes)) {
		
		# Load the Della output
		load(paste0("./Results/GAMtoPeople_Della/GAMtoPeople_",codes$DHS_code[i],"_6-60m.RData"))

		# Get border lines to add
		adm0 <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[i], "_adm/", codes$ADM_code[i], "_adm0.shp"))
		
		# Calculate the number of unvaccinated people between [lower_age, upper_age]
		rowname <- match(g_df$dummy, g$dummy)
		unvacc <- (rowSums(grid_tmp_lt5y[rowname, paste0("p_unvacc_", lower_age:upper_age, "m")]) * (grid_tmp_lt5y[rowname, "lt5y"]/60)) #+ 1 # since we show on log scale
		g_df$unvacc <- unvacc
		
		# Legend name
		legend_name <- paste("Unvaccinateds\n", lower_age, "-", upper_age, "m", sep="")
		
		# Plot
		tmp <- tmp + 
		geom_polygon(data=g_df, mapping=aes(x=long, y=lat, group=group, fill=unvacc)) +
		scale_fill_gradientn(name=legend_name, 
						 colours=colorRampPalette(rev(brewer.pal(9, "RdYlBu")))(50), 
						 trans="log",
						 na.value="royalblue4", # aka those with 0 people
						 limits=c(1,50000),
						 breaks=c(1,10,100,1000,10000)) +
		geom_polygon(data=adm0, aes(x=long, y=lat, z=group), colour="gray40", fill=NA, cex=0.25) +
		guides(fill=guide_colorbar(barwidth=2, barheight=20))
				
	}

	# Get the outline of all the countries
	adm0_all <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[1], "_adm/", codes$ADM_code[1], "_adm0.shp"))
	for(i in 2:nrow(codes)) {

		# Get border lines to add
		adm0 <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[i], "_adm/", codes$ADM_code[i], "_adm0.shp"))
		adm0_all <- rbind(adm0_all, adm0, makeUniqueIDs=TRUE)
		
	}

	# Add water in all the countries
	water_in_region <- rgeos::gIntersection(water, adm0_all, drop_lower_td=TRUE)
		
	# Plot water
	tmp <- tmp + geom_polygon(data=water_in_region, aes(x=long, y=lat, group=group), colour="gray40", size=0.2, fill="azure", alpha=1)

	# Get capitals
	cg.cap <- c(-4.316667, 15.316667)
	ug.cap <- c(0.313611, 32.581111)
	ke.cap <- c(-1.283333, 36.816667)
	rw.cap <- c(-1.943889, 30.059444)
	by.cap <- c(-3.383333, 29.366667)
	tz.cap <- c(-6.173056, 35.741944)
	za.cap <- c(-15.416667, 28.283333)
	mi.cap <- c(-13.98972222, 33.78861111)
	mo.cap <- c(-25.966667, 32.583333)
	zi.cap <- c(-17.863889, 31.029722)
	cap <- rbind(cg.cap,ug.cap,ke.cap,rw.cap,by.cap,tz.cap,za.cap,mi.cap,mo.cap,zi.cap)
	cap <- data.frame(cap)
	colnames(cap) <- c("lat", "long")

	# Settings
	tmp <- tmp + coord_fixed() + 
		theme(axis.line=element_blank(),
						axis.text.x=element_blank(),
						axis.text.y=element_blank(),
						axis.ticks=element_blank(),
						axis.title.x=element_blank(),
						axis.title.y=element_blank(),
						panel.background=element_blank(),
						panel.border=element_blank(),
						panel.grid.major=element_blank(),
						panel.grid.minor=element_blank(),
						plot.background=element_blank())

	# Change legend tick marks to dark
	g <- ggplotGrob(tmp)
	g$grobs[[8]][[1]][[1]][[1]][[5]]$gp$col <- "gray50"
	grid.draw(g)
	
}

################################################################################

## Description: GAM mean map at binned_survey_age, with effect of sub-national SIAs
## Description: with and without contour lines
## Sub-national SIAs: included in prediction
## Spatial scale: single country

getSmoothedMap_WithSubSIA_Mean_Single_NoContour <- function(Country, binned_survey_age) {
	
	# Get the rest
	i <- as.numeric(rownames(codes))[which(Country==codes$Country)]
	
	# Placeholders
	DHS_code <- codes$DHS_code[i]
	DHS_name <- codes$DHS_name[i]
	ADM_code <- codes$ADM_code[i]
	
	# Load the Della output
	load(paste0("./Results/GAMtoMeanAndSE_Della_WithSubnationals/GAMtoMeanAndSE_WithSubnationals_", DHS_code,"_6-60m.RData"))

	# Get border lines to add
	adm0 <- readShapeSpatial(paste0("./Data/ADM/", ADM_code, "_adm/", ADM_code, "_adm0.shp"))
	
	# Plot	
	tmp <- ggplot()		
	
	# Which column are we plotting?
	which_data <- paste0("Mean_", binned_survey_age, "m")

	# Get water bodies
	water <- readShapeSpatial("./Data/waterbodies_africa/waterbodies_africa.shp")
	
	# Legend name
	legend_name <- paste("Mean at ", binned_survey_age, "m\nSub-national SIAs", sep="")

	tmp <- tmp + 
	geom_polygon(data=g_df, mapping=aes(x=long, y=lat, group=group, fill=plogis(g_df[,which_data]))) +
	scale_fill_gradientn(name=legend_name, 
						 limits=c(0,1), 	
						 breaks=c(0, 0.5, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1),
						 values=c(0, 0.5, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1),
						 colours=brewer.pal(10, "RdYlGn")) +
	geom_polygon(data=adm0, aes(x=long, y=lat, z=group), colour="gray40", fill=NA, cex=0.25) +
	guides(fill=guide_colorbar(barwidth=2, barheight=20))
	
	# Add water in that country, if applicable
	water_in_country <- rgeos::gIntersection(water, adm0, drop_lower_td=TRUE)
	
	if(!is.null(water_in_country)) {
	
		# Plot water
		tmp <- tmp + geom_polygon(data=water_in_country, aes(x=long, y=lat, group=group), colour="gray40", size=0.2, fill="azure", alpha=1)
	
	}
		
	# Settings
	tmp <- tmp + coord_fixed() + 
			  theme(axis.line=element_blank(),
					axis.text.x=element_blank(),
					axis.text.y=element_blank(),
					axis.ticks=element_blank(),
					axis.title.x=element_blank(),
					axis.title.y=element_blank(),
					panel.background=element_blank(),
					panel.border=element_blank(),
					panel.grid.major=element_blank(),
					panel.grid.minor=element_blank(),
					plot.background=element_blank())
	
	# Change legend tick marks to dark
	g <- ggplotGrob(tmp)
	g$grobs[[8]][[1]][[1]][[1]][[5]]$gp$col <- "gray50"
	grid.draw(g)
	
}

getSmoothedMap_WithSubSIA_Mean_Single <- function(Country, binned_survey_age) {
	
	# Get the rest
	i <- as.numeric(rownames(codes))[which(Country==codes$Country)]
	
	# Placeholders
	DHS_code <- codes$DHS_code[i]
	DHS_name <- codes$DHS_name[i]
	ADM_code <- codes$ADM_code[i]
	
	# Load the Della output
	load(paste0("./Results/GAMtoMeanAndSE_Della_WithSubnationals/GAMtoMeanAndSE_WithSubnationals_", DHS_code,"_6-60m.RData"))

	# Get border lines to add
	adm0 <- readShapeSpatial(paste0("./Data/ADM/", ADM_code, "_adm/", ADM_code, "_adm0.shp"))
	
	# Plot	
	tmp <- ggplot()		
	
	# Which column are we plotting?
	which_data <- paste0("Mean_", binned_survey_age, "m")

	# Get water bodies
	water <- readShapeSpatial("./Data/waterbodies_africa/waterbodies_africa.shp")
	
	# Legend name
	legend_name <- paste("Mean at ", binned_survey_age, "m\nSub-national SIAs", sep="")

	tmp <- tmp + 
	geom_polygon(data=g_df, mapping=aes(x=long, y=lat, group=group, fill=plogis(g_df[,which_data]))) +
	geom_contour(data=grid_tmp_lt5y, mapping=aes(x=long, y=lat, z=plogis(grid_tmp_lt5y[,which_data])), size=0.1, breaks=seq(0,1,by=0.05), colour="gray50") +
	scale_fill_gradientn(name=legend_name, 
						 limits=c(0,1), 	
						 breaks=c(0, 0.5, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1),
						 values=c(0, 0.5, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1),
						 colours=brewer.pal(10, "RdYlGn")) +
	geom_polygon(data=adm0, aes(x=long, y=lat, z=group), colour="gray40", fill=NA, cex=0.25) +
	guides(fill=guide_colorbar(barwidth=2, barheight=20))

	# Add water in that country, if applicable
	water_in_country <- rgeos::gIntersection(water, adm0, drop_lower_td=TRUE)
	
	if(!is.null(water_in_country)) {
	
		# Plot water
		tmp <- tmp + geom_polygon(data=water_in_country, aes(x=long, y=lat, group=group), colour="gray40", size=0.2, fill="azure", alpha=1)
	
	}
		
	# Settings
	tmp <- tmp + coord_fixed() + 
			  theme(axis.line=element_blank(),
					axis.text.x=element_blank(),
					axis.text.y=element_blank(),
					axis.ticks=element_blank(),
					axis.title.x=element_blank(),
					axis.title.y=element_blank(),
					panel.background=element_blank(),
					panel.border=element_blank(),
					panel.grid.major=element_blank(),
					panel.grid.minor=element_blank(),
					plot.background=element_blank())
	
	# Change legend tick marks to dark
	g <- ggplotGrob(tmp)
	g$grobs[[8]][[1]][[1]][[1]][[5]]$gp$col <- "gray50"
	grid.draw(g)
	
}

################################################################################

## Description: GAM mean map at binned_survey_age, with jittering or DHS weights
## Sub-national SIAs: not included in prediction
## Spatial scale: all countries

getSmoothedMap_noSubSIA_Mean_Jitter <- function(binned_survey_age) {

	# For plot
	tmp <- ggplot()
	
	# Which column are we plotting?
	which_data <- paste0("Mean_", binned_survey_age, "m")

	# Get water bodies
	water <- readShapeSpatial("./Data/waterbodies_africa/waterbodies_africa.shp")

	# Get the outline of all the countries
	adm0_all <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[1], "_adm/", codes$ADM_code[1], "_adm0.shp"))
	for(i in 2:nrow(codes)) {

		## Get border lines to add
		adm0 <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[i], "_adm/", codes$ADM_code[i], "_adm0.shp"))
		adm0_all <- rbind(adm0_all, adm0, makeUniqueIDs=TRUE)
	
	}
	
	# Aggregate data across all countries
	g_df_all <- NULL
	for(i in 1:nrow(codes)) {
	
		# Load the Della output
		load(paste0("./Results/GAMtoMeanAndSE_Della_JitterAndWeights/GAMtoMeanAndSE_",codes$DHS_code[i],"_6-60m_Jitter.RData"))
		
		# Aggregate the cells using rbind
		g_df$country <- i
		g_df$group_final <- paste0(g_df$country, "_", g_df$group)
		g_df_all <- rbind(g_df_all, g_df)
		
		# Create a new data frame for contour
		assign(paste0("contour_", i), grid_tmp_lt5y)
		
	}
	
	# Legend name
	legend_name <- paste("Mean at ", binned_survey_age, "m\n", "Jittered locations", sep="")

	tmp <- tmp + 
		geom_polygon(data=g_df_all, mapping=aes(x=long, y=lat, group=group_final, fill=plogis(g_df_all[,which_data]))) + 
		scale_fill_gradientn(name=legend_name, 
								 limits=c(0,1), 	
								 breaks=c(0, 0.5, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1),
								 values=c(0, 0.5, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1),
								 colours=brewer.pal(10, "RdYlGn")) +					 
		guides(fill=guide_colorbar(barwidth=2, barheight=20))

	# Add the contour lines
	for(i in 1:nrow(codes)) {
		
		data <- get(paste0("contour_", i))[,c("long", "lat", which_data)]
		names(data) <- c("long", "lat", "z")

		tmp <- tmp + 
		geom_contour(data=data, mapping=aes(x=long, y=lat, z=plogis(z)), size=0.1, breaks=seq(0,1,by=0.05), colour="gray50") 
	
	}

	# Add the adm0 boundaries
	tmp <- tmp + geom_polygon(data=adm0_all, aes(x=long, y=lat, z=group), colour="gray40", fill=NA, cex=0.25)

	# Add water in all the countries
	water_in_region <- rgeos::gIntersection(water, adm0_all, drop_lower_td=TRUE)
		
	# Plot water
	tmp <- tmp + geom_polygon(data=water_in_region, aes(x=long, y=lat, group=group), colour="gray40", size=0.2, fill="azure", alpha=1)	
	
	# Get capitals
	cg.cap <- c(-4.316667, 15.316667)
	ug.cap <- c(0.313611, 32.581111)
	ke.cap <- c(-1.283333, 36.816667)
	rw.cap <- c(-1.943889, 30.059444)
	by.cap <- c(-3.383333, 29.366667)
	tz.cap <- c(-6.173056, 35.741944)
	za.cap <- c(-15.416667, 28.283333)
	mi.cap <- c(-13.98972222, 33.78861111)
	mo.cap <- c(-25.966667, 32.583333)
	zi.cap <- c(-17.863889, 31.029722)
	cap <- rbind(cg.cap,ug.cap,ke.cap,rw.cap,by.cap,tz.cap,za.cap,mi.cap,mo.cap,zi.cap)
	cap <- data.frame(cap)
	colnames(cap) <- c("lat", "long")

	# Plot with capitals
	tmp <- tmp + 
			  geom_point(data=cap, aes(long,lat), cex=1.2, fill="plum", col="gray30", pch=21) +
			  coord_fixed() + 
			  theme(axis.line=element_blank(),
					axis.text.x=element_blank(),
					axis.text.y=element_blank(),
					axis.ticks=element_blank(),
					axis.title.x=element_blank(),
					axis.title.y=element_blank(),
					panel.background=element_blank(),
					panel.border=element_blank(),
					panel.grid.major=element_blank(),
					panel.grid.minor=element_blank(),
					plot.background=element_blank())

	# Change legend tick marks to dark
	g <- ggplotGrob(tmp)
	g$grobs[[8]][[1]][[1]][[1]][[5]]$gp$col <- "gray50"
	grid.draw(g)
	
}

getSmoothedMap_noSubSIA_Mean_Weights <- function(binned_survey_age) {

	# For plot
	tmp <- ggplot()
	
	# Which column are we plotting?
	which_data <- paste0("Mean_", binned_survey_age, "m")

	# Get water bodies
	water <- readShapeSpatial("./Data/waterbodies_africa/waterbodies_africa.shp")

	# Get the outline of all the countries
	adm0_all <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[1], "_adm/", codes$ADM_code[1], "_adm0.shp"))
	for(i in 2:nrow(codes)) {

		## Get border lines to add
		adm0 <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[i], "_adm/", codes$ADM_code[i], "_adm0.shp"))
		adm0_all <- rbind(adm0_all, adm0, makeUniqueIDs=TRUE)
	
	}
	
	# Aggregate data across all countries
	g_df_all <- NULL
	for(i in 1:nrow(codes)) {
	
		# Load the Della output
		load(paste0("./Results/GAMtoMeanAndSE_Della_JitterAndWeights/GAMtoMeanAndSE_",codes$DHS_code[i],"_6-60m_Weights.RData"))
		
		# Aggregate the cells using rbind
		g_df$country <- i
		g_df$group_final <- paste0(g_df$country, "_", g_df$group)
		g_df_all <- rbind(g_df_all, g_df)
		
		# Create a new data frame for contour
		assign(paste0("contour_", i), grid_tmp_lt5y)
		
	}

	# Legend name
	legend_name <- paste("Mean at ", binned_survey_age, "m\n", "With DHS weights", sep="")

	tmp <- tmp + 
		geom_polygon(data=g_df_all, mapping=aes(x=long, y=lat, group=group_final, fill=plogis(g_df_all[,which_data]))) + 
		scale_fill_gradientn(name=legend_name, 
								 limits=c(0,1), 	
								 breaks=c(0, 0.5, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1),
								 values=c(0, 0.5, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1),
								 colours=brewer.pal(10, "RdYlGn")) +					 
		guides(fill=guide_colorbar(barwidth=2, barheight=20))

	# Add the contour lines
	for(i in 1:nrow(codes)) {
		
		data <- get(paste0("contour_", i))[,c("long", "lat", which_data)]
		names(data) <- c("long", "lat", "z")

		tmp <- tmp + 
		geom_contour(data=data, mapping=aes(x=long, y=lat, z=plogis(z)), size=0.1, breaks=seq(0,1,by=0.05), colour="gray50") 
	
	}

	# Add the adm0 boundaries
	tmp <- tmp + geom_polygon(data=adm0_all, aes(x=long, y=lat, z=group), colour="gray40", fill=NA, cex=0.25)

	# Add water in all the countries
	water_in_region <- rgeos::gIntersection(water, adm0_all, drop_lower_td=TRUE)
		
	# Plot water
	tmp <- tmp + geom_polygon(data=water_in_region, aes(x=long, y=lat, group=group), colour="gray40", size=0.2, fill="azure", alpha=1)	
	
	# Get capitals
	cg.cap <- c(-4.316667, 15.316667)
	ug.cap <- c(0.313611, 32.581111)
	ke.cap <- c(-1.283333, 36.816667)
	rw.cap <- c(-1.943889, 30.059444)
	by.cap <- c(-3.383333, 29.366667)
	tz.cap <- c(-6.173056, 35.741944)
	za.cap <- c(-15.416667, 28.283333)
	mi.cap <- c(-13.98972222, 33.78861111)
	mo.cap <- c(-25.966667, 32.583333)
	zi.cap <- c(-17.863889, 31.029722)
	cap <- rbind(cg.cap,ug.cap,ke.cap,rw.cap,by.cap,tz.cap,za.cap,mi.cap,mo.cap,zi.cap)
	cap <- data.frame(cap)
	colnames(cap) <- c("lat", "long")

	# Plot with capitals
	tmp <- tmp + 
			  geom_point(data=cap, aes(long,lat), cex=1.2, fill="plum", col="gray30", pch=21) +
			  coord_fixed() + 
			  theme(axis.line=element_blank(),
					axis.text.x=element_blank(),
					axis.text.y=element_blank(),
					axis.ticks=element_blank(),
					axis.title.x=element_blank(),
					axis.title.y=element_blank(),
					panel.background=element_blank(),
					panel.border=element_blank(),
					panel.grid.major=element_blank(),
					panel.grid.minor=element_blank(),
					plot.background=element_blank())

	# Change legend tick marks to dark
	g <- ggplotGrob(tmp)
	g$grobs[[8]][[1]][[1]][[1]][[5]]$gp$col <- "gray50"
	grid.draw(g)
	
}

################################################################################

## Description: coldspots map at binned_survey_age, with jittering or DHS weights
## Description: grid cells with < cutoff_percent mean coverage at binned_survey_age
## Sub-national SIAs: not included in prediction
## Spatial scale: all countries

getSmoothedMap_noSubSIA_ColdSpots_Jitter <- function(binned_survey_age=12, cutoff_percent=0.8) {

	# For plot
	tmp <- ggplot()
	
	# Which column are we plotting?
	which_data <- paste0("Mean_", binned_survey_age, "m")
	
	# Get water bodies
	water <- readShapeSpatial("./Data/waterbodies_africa/waterbodies_africa.shp")
	
	# Aggregate data across all countries
	g_df_all <- NULL
	for(i in 1:nrow(codes)) {
	
		# Load the Della output
		load(paste0("./Results/GAMtoMeanAndSE_Della_JitterAndWeights/GAMtoMeanAndSE_",codes$DHS_code[i],"_6-60m_Jitter.RData"))
		
		# Aggregate the cells using rbind
		g_df$country <- i
		g_df$group_final <- paste0(g_df$country, "_", g_df$group)
		g_df_all <- rbind(g_df_all, g_df)
		
	}
	
	# Plot the blank grid cells
	tmp <- tmp + geom_polygon(data=g_df_all, mapping=aes(x=long, y=lat, group=group_final), fill=NA)
	
	# Find and fill the coldspots, aka cells with less than cutoff_percent coverage
	g_df_all$coverage_at_cutoff_age <- plogis(g_df_all[,which_data])
	
	tmp <- tmp + geom_polygon(data=g_df_all[which(g_df_all$coverage_at_cutoff_age < cutoff_percent),], mapping=aes(x=long, y=lat, group=group_final, fill=factor(dummy))) + scale_fill_manual(name="Coldspots (Jittered locations)", values="gray75", labels=paste0("< ", cutoff_percent*100, "% at ", sprintf("%02d", binned_survey_age), "m")) + theme(legend.position=c(0.2,0.2), legend.background=element_rect(fill=NA, size=0.1, linetype="solid", colour="black"))
	
	# Get the outline of all the countries (adm0 and adm1)
	adm0_all <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[1], "_adm/", codes$ADM_code[1], "_adm0.shp"))
	adm1_all <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[1], "_adm/", codes$ADM_code[1], "_adm1.shp"))	
	
	for(i in 2:nrow(codes)) {

		# Get border lines to add
		adm0 <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[i], "_adm/", codes$ADM_code[i], "_adm0.shp"))
		adm1 <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[i], "_adm/", codes$ADM_code[i], "_adm1.shp"))
		
		# Stitch together
		adm0_all <- rbind(adm0_all, adm0, makeUniqueIDs=TRUE)
		adm1_all <- rbind(adm1_all, adm1, makeUniqueIDs=TRUE)
		
	}
		
	# Add the adm1 boundaries
	tmp <- tmp + geom_polygon(data=adm1_all, aes(x=long, y=lat, z=group), colour="gray60", fill=NA, cex=0.1)	

	# Add the adm0 boundaries
	tmp <- tmp + geom_polygon(data=adm0_all, aes(x=long, y=lat, z=group), colour="gray40", fill=NA, cex=0.25)

	# Add water in all the countries
	water_in_region <- rgeos::gIntersection(water, adm0_all, drop_lower_td=TRUE)
		
	# Plot water
	tmp <- tmp + geom_polygon(data=water_in_region, aes(x=long, y=lat, group=group), colour="gray40", size=0.2, fill="azure", alpha=1)
	
	# Get capitals
	cg.cap <- c(-4.316667, 15.316667)
	ug.cap <- c(0.313611, 32.581111)
	ke.cap <- c(-1.283333, 36.816667)
	rw.cap <- c(-1.943889, 30.059444)
	by.cap <- c(-3.383333, 29.366667)
	tz.cap <- c(-6.173056, 35.741944)
	za.cap <- c(-15.416667, 28.283333)
	mi.cap <- c(-13.98972222, 33.78861111)
	mo.cap <- c(-25.966667, 32.583333)
	zi.cap <- c(-17.863889, 31.029722)
	cap <- rbind(cg.cap,ug.cap,ke.cap,rw.cap,by.cap,tz.cap,za.cap,mi.cap,mo.cap,zi.cap)
	cap <- data.frame(cap)
	colnames(cap) <- c("lat", "long")

	# Plot with capitals
	tmp <- tmp + 
			  geom_point(data=cap, aes(long,lat), cex=1.2, fill="plum", col="gray30", pch=21) +
			  coord_fixed() + 
			  theme(axis.line=element_blank(),
					axis.text.x=element_blank(),
					axis.text.y=element_blank(),
					axis.ticks=element_blank(),
					axis.title.x=element_blank(),
					axis.title.y=element_blank(),
					panel.background=element_blank(),
					panel.border=element_blank(),
					panel.grid.major=element_blank(),
					panel.grid.minor=element_blank(),
					plot.background=element_blank())

	print(tmp)
	
}

getSmoothedMap_noSubSIA_ColdSpots_Weights <- function(binned_survey_age=12, cutoff_percent=0.8) {

	# For plot
	tmp <- ggplot()
	
	# Which column are we plotting?
	which_data <- paste0("Mean_", binned_survey_age, "m")
	
	# Get water bodies
	water <- readShapeSpatial("./Data/waterbodies_africa/waterbodies_africa.shp")
	
	# Aggregate data across all countries
	g_df_all <- NULL
	for(i in 1:nrow(codes)) {
	
		# Load the Della output
		load(paste0("./Results/GAMtoMeanAndSE_Della_JitterAndWeights/GAMtoMeanAndSE_",codes$DHS_code[i],"_6-60m_Weights.RData"))
		
		# Aggregate the cells using rbind
		g_df$country <- i
		g_df$group_final <- paste0(g_df$country, "_", g_df$group)
		g_df_all <- rbind(g_df_all, g_df)
		
	}
	
	# Plot the blank grid cells
	tmp <- tmp + geom_polygon(data=g_df_all, mapping=aes(x=long, y=lat, group=group_final), fill=NA)
	
	# Find and fill the coldspots, aka cells with less than cutoff_percent coverage
	g_df_all$coverage_at_cutoff_age <- plogis(g_df_all[,which_data])
	
	tmp <- tmp + geom_polygon(data=g_df_all[which(g_df_all$coverage_at_cutoff_age < cutoff_percent),], mapping=aes(x=long, y=lat, group=group_final, fill=factor(dummy))) + scale_fill_manual(name="Coldspots (With DHS weights)", values="gray75", labels=paste0("< ", cutoff_percent*100, "% at ", sprintf("%02d", binned_survey_age), "m")) + theme(legend.position=c(0.2,0.2), legend.background=element_rect(fill=NA, size=0.1, linetype="solid", colour="black"))
	
	# Get the outline of all the countries (adm0 and adm1)
	adm0_all <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[1], "_adm/", codes$ADM_code[1], "_adm0.shp"))
	adm1_all <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[1], "_adm/", codes$ADM_code[1], "_adm1.shp"))	
	
	for(i in 2:nrow(codes)) {

		# Get border lines to add
		adm0 <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[i], "_adm/", codes$ADM_code[i], "_adm0.shp"))
		adm1 <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[i], "_adm/", codes$ADM_code[i], "_adm1.shp"))
		
		# Stitch together
		adm0_all <- rbind(adm0_all, adm0, makeUniqueIDs=TRUE)
		adm1_all <- rbind(adm1_all, adm1, makeUniqueIDs=TRUE)
		
	}
		
	# Add the adm1 boundaries
	tmp <- tmp + geom_polygon(data=adm1_all, aes(x=long, y=lat, z=group), colour="gray60", fill=NA, cex=0.1)	

	# Add the adm0 boundaries
	tmp <- tmp + geom_polygon(data=adm0_all, aes(x=long, y=lat, z=group), colour="gray40", fill=NA, cex=0.25)

	# Add water in all the countries
	water_in_region <- rgeos::gIntersection(water, adm0_all, drop_lower_td=TRUE)
		
	# Plot water
	tmp <- tmp + geom_polygon(data=water_in_region, aes(x=long, y=lat, group=group), colour="gray40", size=0.2, fill="azure", alpha=1)
	
	# Get capitals
	cg.cap <- c(-4.316667, 15.316667)
	ug.cap <- c(0.313611, 32.581111)
	ke.cap <- c(-1.283333, 36.816667)
	rw.cap <- c(-1.943889, 30.059444)
	by.cap <- c(-3.383333, 29.366667)
	tz.cap <- c(-6.173056, 35.741944)
	za.cap <- c(-15.416667, 28.283333)
	mi.cap <- c(-13.98972222, 33.78861111)
	mo.cap <- c(-25.966667, 32.583333)
	zi.cap <- c(-17.863889, 31.029722)
	cap <- rbind(cg.cap,ug.cap,ke.cap,rw.cap,by.cap,tz.cap,za.cap,mi.cap,mo.cap,zi.cap)
	cap <- data.frame(cap)
	colnames(cap) <- c("lat", "long")

	# Plot with capitals
	tmp <- tmp + 
			  geom_point(data=cap, aes(long,lat), cex=1.2, fill="plum", col="gray30", pch=21) +
			  coord_fixed() + 
			  theme(axis.line=element_blank(),
					axis.text.x=element_blank(),
					axis.text.y=element_blank(),
					axis.ticks=element_blank(),
					axis.title.x=element_blank(),
					axis.title.y=element_blank(),
					panel.background=element_blank(),
					panel.border=element_blank(),
					panel.grid.major=element_blank(),
					panel.grid.minor=element_blank(),
					plot.background=element_blank())

	print(tmp)
	
}

################################################################################

## Description: GAM mean map at binned_survey_age, incorporating urbanicity
## Sub-national SIAs: not included in prediction
## Spatial scale: all countries
## Map_types: "Urban", "Rural", "Urban-Rural"

getSmoothedMap_noSubSIA_Mean_UrbanRural <- function(binned_survey_age, map_type) {

	# For plot
	tmp <- ggplot()
	
	# Which column are we plotting?
	which_data <- paste0("Mean_", binned_survey_age, "m")

	# Get water bodies
	water <- readShapeSpatial("./Data/waterbodies_africa/waterbodies_africa.shp")

	# Get the outline of all the countries
	adm0_all <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[1], "_adm/", codes$ADM_code[1], "_adm0.shp"))
	for(i in 2:nrow(codes)) {

		## Get border lines to add
		adm0 <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[i], "_adm/", codes$ADM_code[i], "_adm0.shp"))
		adm0_all <- rbind(adm0_all, adm0, makeUniqueIDs=TRUE)
	
	}
	
	## For a map of Urban:
	if(map_type=="Urban") {
		
		# Aggregate data across all countries
		g_df_all <- NULL
		for(i in 1:nrow(codes)) {
		
			# Load the Della output
			load(paste0("./Results/GAMtoMeanAndSE_Della_urbanrural/GAMtoMeanAndSE_",codes$DHS_code[i],"_6-60m_Urban.RData"))
			
			# Aggregate the cells using rbind
			g_df$country <- i
			g_df$group_final <- paste0(g_df$country, "_", g_df$group)
			g_df_all <- rbind(g_df_all, g_df)
			
			# Create a new data frame for contour
			assign(paste0("contour_", i), grid_tmp_lt5y)
			
		}

		# Legend name
		legend_name <- paste("Mean at ", binned_survey_age, "m\n", "Urban", sep="")

		tmp <- tmp + 
			geom_polygon(data=g_df_all, mapping=aes(x=long, y=lat, group=group_final, fill=plogis(g_df_all[,which_data]))) + 
			scale_fill_gradientn(name=legend_name, 
									 limits=c(0,1), 	
									 breaks=c(0, 0.5, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1),
									 values=c(0, 0.5, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1),
									 colours=brewer.pal(10, "RdYlGn")) +					 
			guides(fill=guide_colorbar(barwidth=2, barheight=20))

		# Add the contour lines
		for(i in 1:nrow(codes)) {
			
			data <- get(paste0("contour_", i))[,c("long", "lat", which_data)]
			names(data) <- c("long", "lat", "z")

			tmp <- tmp + 
			geom_contour(data=data, mapping=aes(x=long, y=lat, z=plogis(z)), size=0.1, breaks=seq(0,1,by=0.05), colour="gray50") 
		
		}

	}
	
	## For a map of Rural:
	if(map_type=="Rural") {
		
		# Aggregate data across all countries
		g_df_all <- NULL
		for(i in 1:nrow(codes)) {
		
			# Load the Della output
			load(paste0("./Results/GAMtoMeanAndSE_Della_urbanrural/GAMtoMeanAndSE_",codes$DHS_code[i],"_6-60m_Rural.RData"))
			
			# Aggregate the cells using rbind
			g_df$country <- i
			g_df$group_final <- paste0(g_df$country, "_", g_df$group)
			g_df_all <- rbind(g_df_all, g_df)
			
			# Create a new data frame for contour
			assign(paste0("contour_", i), grid_tmp_lt5y)
			
		}

		# Legend name
		legend_name <- paste("Mean at ", binned_survey_age, "m\n", "Rural", sep="")

		tmp <- tmp + 
			geom_polygon(data=g_df_all, mapping=aes(x=long, y=lat, group=group_final, fill=plogis(g_df_all[,which_data]))) + 
			scale_fill_gradientn(name=legend_name, 
									 limits=c(0,1), 	
									 breaks=c(0, 0.5, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1),
									 values=c(0, 0.5, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1),
									 colours=brewer.pal(10, "RdYlGn")) +					 
			guides(fill=guide_colorbar(barwidth=2, barheight=20))

		# Add the contour lines
		for(i in 1:nrow(codes)) {
			
			data <- get(paste0("contour_", i))[,c("long", "lat", which_data)]
			names(data) <- c("long", "lat", "z")

			tmp <- tmp + 
			geom_contour(data=data, mapping=aes(x=long, y=lat, z=plogis(z)), size=0.1, breaks=seq(0,1,by=0.05), colour="gray50") 
		
		}

	}
	
	## For a map of Urban-Rural:
	if(map_type=="Urban-Rural") {
		
		# Urban - Aggregate data across all countries
		g_df_all <- NULL
		for(i in 1:nrow(codes)) {
		
			# Load the Della output
			load(paste0("./Results/GAMtoMeanAndSE_Della_urbanrural/GAMtoMeanAndSE_",codes$DHS_code[i],"_6-60m_Urban.RData"))
			
			# Aggregate the cells using rbind
			g_df$country <- i
			g_df$group_final <- paste0(g_df$country, "_", g_df$group)
			g_df_all <- rbind(g_df_all, g_df)
			
			# Create a new data frame for contour
			assign(paste0("contour_urban_", i), grid_tmp_lt5y)
			
		}
		g_df_all_urban <- g_df_all
		
		# Rural - Aggregate data across all countries
		g_df_all <- NULL
		for(i in 1:nrow(codes)) {
		
			# Load the Della output
			load(paste0("./Results/GAMtoMeanAndSE_Della_urbanrural/GAMtoMeanAndSE_",codes$DHS_code[i],"_6-60m_Rural.RData"))
			
			# Aggregate the cells using rbind
			g_df$country <- i
			g_df$group_final <- paste0(g_df$country, "_", g_df$group)
			g_df_all <- rbind(g_df_all, g_df)
			
			# Create a new data frame for contour
			assign(paste0("contour_rural_", i), grid_tmp_lt5y)
			
		}
		g_df_all_rural <- g_df_all		

		# Combine U and R
		g_df_all <- data.frame(long=g_df_all_urban$long, lat=g_df_all_urban$lat, group_final=g_df_all_urban$group_final, to_plot=g_df_all_urban[,which_data]-g_df_all_rural[,which_data])
		
		# Legend name
		legend_name <- paste("Mean at ", binned_survey_age, "m\n", "Urban-Rural", sep="")

		tmp <- tmp + 
			geom_polygon(data=g_df_all, mapping=aes(x=long, y=lat, group=group_final, fill=to_plot)) + 
			scale_fill_gradientn(name=legend_name, 
									 limits=c(-0.1,0.6), 	
									 breaks=seq(-0.05, 0.55, by=0.1),
									 colours=brewer.pal(9, "YlGnBu")) +					 
			guides(fill=guide_colorbar(barwidth=2, barheight=20))

	}
	
	# Add the adm0 boundaries
	tmp <- tmp + geom_polygon(data=adm0_all, aes(x=long, y=lat, z=group), colour="gray40", fill=NA, cex=0.25)

	# Add water in all the countries
	water_in_region <- rgeos::gIntersection(water, adm0_all, drop_lower_td=TRUE)
		
	# Plot water
	tmp <- tmp + geom_polygon(data=water_in_region, aes(x=long, y=lat, group=group), colour="gray40", size=0.2, fill="azure", alpha=1)	
	
	# Get capitals
	cg.cap <- c(-4.316667, 15.316667)
	ug.cap <- c(0.313611, 32.581111)
	ke.cap <- c(-1.283333, 36.816667)
	rw.cap <- c(-1.943889, 30.059444)
	by.cap <- c(-3.383333, 29.366667)
	tz.cap <- c(-6.173056, 35.741944)
	za.cap <- c(-15.416667, 28.283333)
	mi.cap <- c(-13.98972222, 33.78861111)
	mo.cap <- c(-25.966667, 32.583333)
	zi.cap <- c(-17.863889, 31.029722)
	cap <- rbind(cg.cap,ug.cap,ke.cap,rw.cap,by.cap,tz.cap,za.cap,mi.cap,mo.cap,zi.cap)
	cap <- data.frame(cap)
	colnames(cap) <- c("lat", "long")

	# Plot with capitals
	tmp <- tmp + 
			  geom_point(data=cap, aes(long,lat), cex=1.2, fill="plum", col="gray30", pch=21) +
			  coord_fixed() + 
			  theme(axis.line=element_blank(),
					axis.text.x=element_blank(),
					axis.text.y=element_blank(),
					axis.ticks=element_blank(),
					axis.title.x=element_blank(),
					axis.title.y=element_blank(),
					panel.background=element_blank(),
					panel.border=element_blank(),
					panel.grid.major=element_blank(),
					panel.grid.minor=element_blank(),
					plot.background=element_blank())

	# Change legend tick marks to dark
	g <- ggplotGrob(tmp)
	g$grobs[[8]][[1]][[1]][[1]][[5]]$gp$col <- "gray50"
	grid.draw(g)
	
}

################################################################################

## Description: difference in GAM mean, between model with and without the sub-national SIA covariate
## Spatial scale: single country

getSmoothedMap_noSubSIA_InModel_Minus_noSubSIA_Mean_Single <- function(Country, binned_survey_age) {
	
	# Get the rest
	i <- as.numeric(rownames(codes))[which(Country==codes$Country)]
	
	# Placeholders
	DHS_code <- codes$DHS_code[i]
	DHS_name <- codes$DHS_name[i]
	ADM_code <- codes$ADM_code[i]
		
	# Which column are we plotting?
	which_data <- paste0("Mean_", binned_survey_age, "m")

	# Load the Della output for "noSubSIA"
	load(paste0("./Results/GAMtoMeanAndSE_Della/GAMtoMeanAndSE_", DHS_code,"_6-60m.RData"))
	g_df_offset <- plogis(g_df[,which_data])
	grid_tmp_lt5y_offset <- plogis(grid_tmp_lt5y[,which_data])

	# Load the Della output for "noSubSIA_InModel"
	load(paste0("./Results/GAMtoMeanAndSE_Della_NoSubnationals/GAMtoMeanAndSE_", DHS_code,"_6-60m_NoSubnationals.RData"))
	
	# Get border lines to add
	adm0 <- readShapeSpatial(paste0("./Data/ADM/", ADM_code, "_adm/", ADM_code, "_adm0.shp"))
	
	# Plot	
	tmp <- ggplot()		
	
	# Get water bodies
	water <- readShapeSpatial("./Data/waterbodies_africa/waterbodies_africa.shp")

	# Legend name
	legend_name <- bquote(paste(Delta, "Mean at ", .(binned_survey_age), "m"))
	
	tmp <- tmp + 
	geom_polygon(data=g_df, mapping=aes(x=long, y=lat, group=group, fill=plogis(g_df[,which_data])-g_df_offset)) +
	geom_contour(data=grid_tmp_lt5y, mapping=aes(x=long, y=lat, z=plogis(grid_tmp_lt5y[,which_data])-grid_tmp_lt5y_offset), size=0.1, breaks=seq(-0.05,0.05,by=0.005), colour="gray50") +
		scale_fill_gradient2(name=legend_name,
							 limits=c(-0.02, 0.02),
							 breaks=seq(-0.02, 0.02, by=0.01)) +
	geom_polygon(data=adm0, aes(x=long, y=lat, z=group), colour="gray40", fill=NA, cex=0.25) +
	guides(fill=guide_colorbar(barwidth=2, barheight=20))
		
	# Add water in that country, if applicable
	water_in_country <- rgeos::gIntersection(water, adm0, drop_lower_td=TRUE)
	
	if(!is.null(water_in_country)) {
	
		# Plot water
		tmp <- tmp + geom_polygon(data=water_in_country, aes(x=long, y=lat, group=group), colour="gray40", size=0.2, fill="azure", alpha=1)
	
	}
		
	# Settings
	tmp <- tmp + coord_fixed() + 
			  theme(axis.line=element_blank(),
					axis.text.x=element_blank(),
					axis.text.y=element_blank(),
					axis.ticks=element_blank(),
					axis.title.x=element_blank(),
					axis.title.y=element_blank(),
					panel.background=element_blank(),
					panel.border=element_blank(),
					panel.grid.major=element_blank(),
					panel.grid.minor=element_blank(),
					plot.background=element_blank())
	
	# Change legend tick marks to dark
	g <- ggplotGrob(tmp)
	g$grobs[[8]][[1]][[1]][[1]][[5]]$gp$col <- "gray50"
	grid.draw(g)
	
}

################################################################################

## Description: maps of DHS data points

MainFigure_DHS <- function() {

	# Get the long/lat coordinates of points in each country
	dat_DHS <- NULL
	
	for(i in 1:nrow(codes_noRB)) {
	   
		# Call in the country-specific data from getDataByInd_Cluster
		load(paste0("./Data/Manipulated_data/", as.character(codes_noRB$DHS_code[i]), ".RData"))
	  
		# If some clusters have 0 people, remove them from the plot
		empty_clusters <- subset(unique(dataClust$cluster_id), !(unique(dataClust$cluster_id) %in% unique(dataInd$cluster_id)))
	
		# Save the subsetted data
		long <- dataClust$long[!(dataClust$cluster_id %in% (empty_clusters))]
		lat <- dataClust$lat[!(dataClust$cluster_id %in% (empty_clusters))]
		inds <- c(table(dataInd$cluster_id))
	  
		# Save the data
		dat_DHS$long <- c(dat_DHS$long, long)
		dat_DHS$lat <- c(dat_DHS$lat, lat)
		dat_DHS$inds <- c(dat_DHS$inds, inds)
		dat_DHS$country <- c(dat_DHS$country, rep(as.character(codes_noRB$Country[i]), times=length(long)))
		
	}

	# Convert to data.frame
	dat_DHS <- as.data.frame(dat_DHS)
	dat_DHS <- dat_DHS[complete.cases(dat_DHS),]
	
	# Remove the (0,0) point manually
	dat_DHS <- dat_DHS[-which(dat_DHS$long<1),]	
	dat_DHS$inds <- as.numeric(dat_DHS$inds)
	
	# Get the outline of all the countries (adm0 and adm1)
	adm0_all <- readShapeSpatial(paste0("./Data/ADM/", codes_noRB$ADM_code[1], "_adm/", codes_noRB$ADM_code[1], "_adm0.shp"))
	adm1_all <- readShapeSpatial(paste0("./Data/ADM/", codes_noRB$ADM_code[1], "_adm/", codes_noRB$ADM_code[1], "_adm1.shp"))	

	for(i in 2:nrow(codes_noRB)) {

		# Get border lines to add
		adm0 <- readShapeSpatial(paste0("./Data/ADM/", codes_noRB$ADM_code[i], "_adm/", codes_noRB$ADM_code[i], "_adm0.shp"))
		adm1 <- readShapeSpatial(paste0("./Data/ADM/", codes_noRB$ADM_code[i], "_adm/", codes_noRB$ADM_code[i], "_adm1.shp"))
		
		# Stitch together
		adm0_all <- rbind(adm0_all, adm0, makeUniqueIDs=TRUE)
		adm1_all <- rbind(adm1_all, adm1, makeUniqueIDs=TRUE)
		
	}

	# Color in Burundi and Rwanda
	adm0_B <- readShapeSpatial(paste0("./Data/ADM/", codes_RB$ADM_code[1], "_adm/", codes_RB$ADM_code[1], "_adm0.shp"))	
	adm0_R <- readShapeSpatial(paste0("./Data/ADM/", codes_RB$ADM_code[2], "_adm/", codes_RB$ADM_code[2], "_adm0.shp"))
	adm0_RB <- rbind(adm0_B, adm0_R, makeUniqueIDs=TRUE)

	# For plot
	tmp <- ggplot()

	# Add the adm1 boundaries
	tmp <- tmp + geom_polygon(data=adm1_all, aes(x=long, y=lat, z=group), colour="gray75", fill=NA, cex=0.1)	
	
	# Color in Burundi and Rwanda
	tmp <- tmp + geom_polygon(data=adm0_RB, aes(x=long, y=lat, z=group), colour="gray90", fill="gray80", cex=0.1)
	
	# Add the adm0 boundaries
	tmp <- tmp + geom_polygon(data=adm0_all, aes(x=long, y=lat, z=group), colour="gray40", fill=NA, cex=0.1)

	# Get water bodies
	water <- readShapeSpatial("./Data/waterbodies_africa/waterbodies_africa.shp")

	# Add water in all the countries
	water_in_region <- rgeos::gIntersection(water, adm0_all, drop_lower_td=TRUE)
		
	# Plot water
	tmp <- tmp + geom_polygon(data=water_in_region, aes(x=long, y=lat, group=group), colour="gray40", size=0.1, fill="azure", alpha=1)
	
	# Plot the (binned) DHS data points
	binned <- cut(dat_DHS$inds, c(0,19,39,59))
	cols <- brewer.pal(3,"Set1")
	names(cols) <- levels(binned)
	levels(binned) <- cols
	dat_DHS$colour <- binned
	
	tmp <- tmp + geom_point(data=dat_DHS, mapping=aes(x=long, y=lat, colour=colour), size=0.05) + scale_colour_identity("Individuals", labels=names(cols), guide="legend")	

	# Plot with scalebar
	tmp <- tmp + coord_fixed() + 
			  theme(axis.line=element_blank(),
					axis.text.x=element_blank(),
					axis.text.y=element_blank(),
					axis.ticks=element_blank(),
					axis.title.x=element_blank(),
					axis.title.y=element_blank(),
					panel.background=element_blank(),
					panel.border=element_blank(),
					panel.grid.major=element_blank(),
					panel.grid.minor=element_blank(),
					plot.background=element_blank())

	tmp <- tmp + scaleBar(lon=15, lat=-24, distanceLon=400, distanceLat=20, distanceLegend=80, dist.unit="km", orientation=FALSE, legend.size=3)

	tmp

}

SubFigure_DHS <- function() {

	# Get the long/lat coordinates of points in each country
	dat_DHS <- NULL
	
	for(i in 1:nrow(codes_RB)) {
	   
		# Call in the country-specific data from getDataByInd_Cluster
		load(paste0("./Data/Manipulated_data/", as.character(codes_RB$DHS_code[i]), ".RData"))
	  
		# If some clusters have 0 people, remove them from the plot
		empty_clusters <- subset(unique(dataClust$cluster_id), !(unique(dataClust$cluster_id) %in% unique(dataInd$cluster_id)))
	  
		# Save the subsetted data
		long <- dataClust$long[!(dataClust$cluster_id %in% (empty_clusters))]
		lat <- dataClust$lat[!(dataClust$cluster_id %in% (empty_clusters))]
		inds <- c(table(dataInd$cluster_id))
	  
		# Save the data
		dat_DHS$long <- c(dat_DHS$long, long)
		dat_DHS$lat <- c(dat_DHS$lat, lat)
		dat_DHS$inds <- c(dat_DHS$inds, inds)
		dat_DHS$country <- c(dat_DHS$country, rep(as.character(codes_RB$Country[i]), times=length(long)))
		
	}

	# Convert to data.frame
	dat_DHS <- as.data.frame(dat_DHS)
	dat_DHS <- dat_DHS[complete.cases(dat_DHS),]
	dat_DHS$inds <- as.numeric(dat_DHS$inds)
	
	# Get the outline of all the countries (adm0 and adm1)
	adm0_all <- readShapeSpatial(paste0("./Data/ADM/", codes_RB$ADM_code[1], "_adm/", codes_RB$ADM_code[1], "_adm0.shp"))
	adm1_all <- readShapeSpatial(paste0("./Data/ADM/", codes_RB$ADM_code[1], "_adm/", codes_RB$ADM_code[1], "_adm1.shp"))	

	for(i in 2:nrow(codes_RB)) {

		# Get border lines to add
		adm0 <- readShapeSpatial(paste0("./Data/ADM/", codes_RB$ADM_code[i], "_adm/", codes_RB$ADM_code[i], "_adm0.shp"))
		adm1 <- readShapeSpatial(paste0("./Data/ADM/", codes_RB$ADM_code[i], "_adm/", codes_RB$ADM_code[i], "_adm1.shp"))
		
		# Stitch together
		adm0_all <- rbind(adm0_all, adm0, makeUniqueIDs=TRUE)
		adm1_all <- rbind(adm1_all, adm1, makeUniqueIDs=TRUE)
		
	}

	# For plot
	tmp <- ggplot()
		
	# Add the adm1 boundaries
	tmp <- tmp + geom_polygon(data=adm1_all, aes(x=long, y=lat, z=group), colour="gray75", fill=NA, cex=0.1)	

	# Add the adm0 boundaries
	tmp <- tmp + geom_polygon(data=adm0_all, aes(x=long, y=lat, z=group), colour="gray40", fill=NA, cex=0.1)

	# Get water bodies
	water <- readShapeSpatial("./Data/waterbodies_africa/waterbodies_africa.shp")

	# Add water in all the countries
	water_in_region <- rgeos::gIntersection(water, adm0_all, drop_lower_td=TRUE)
		
	# Plot water
	tmp <- tmp + geom_polygon(data=water_in_region, aes(x=long, y=lat, group=group), colour="gray40", size=0.1, fill="azure", alpha=1)
	
	# Plot the (binned) DHS data points
	binned <- cut(dat_DHS$inds, c(0,19,39,59))
	cols <- brewer.pal(3,"Set1")
	names(cols) <- levels(binned)
	levels(binned) <- cols
	dat_DHS$colour <- binned
	
	tmp <- tmp + geom_point(data=dat_DHS, mapping=aes(x=long, y=lat, colour=colour), size=0.05) + scale_colour_identity("Individuals", labels=names(cols), guide="legend")	

	# Plot
	tmp <- tmp + coord_fixed() + 
			  theme(axis.line=element_blank(),
					axis.text.x=element_blank(),
					axis.text.y=element_blank(),
					axis.ticks=element_blank(),
					axis.title.x=element_blank(),
					axis.title.y=element_blank(),
					panel.background=element_blank(),
					panel.border=element_blank(),
					panel.grid.major=element_blank(),
					panel.grid.minor=element_blank(),
					plot.background=element_blank())
					
	tmp

}

################################################################################