library(descr)
library(ggplot2)
library(RColorBrewer)
library(foreign)
library(maptools)
library(mgcv)
library(splancs)
library(rgeos)
library(grid)
library(sp)
library(rgdal)	
library(geoR)
library(maps)

## Read in the master list of country codes
codes <- as.data.frame(read.csv("./Data/Master_List.csv"))

################################################################################

## Text: Supplementary Fig. 5
## Variogram of GAM model residuals

dat_variogram_final <- NULL

## Collate all the countries' data
for(i in 1:nrow(codes)) {

	## Read in the envelopes
	load(paste0("./Results/Variogram_Della/Variogram_resid_", codes$DHS_code[i], ".RData"))
	
	## Change the first point
	dat_variogram$x[1] <- 0
	dat_variogram <- dat_variogram[-1,]
		
	## ALL: only the first X km (1/2 of the # of rows)
	dat_variogram <- dat_variogram[1:(nrow(dat_variogram)/2),]
	dat_variogram_final <- rbind(dat_variogram_final, dat_variogram)
	
}

pdf("./Figures/Variogram.pdf", height=8, width=10)
tmp <- ggplot(dat_variogram_final, aes(x=x/1000, y=y, z=z)) + geom_ribbon(aes(ymin=y_lower, ymax=y_upper), fill=paste0("#E86850", "4D")) + geom_line(colour="#E86850", size=0.5) + geom_point(shape=21, size=1, fill="white", colour="#E86850")
tmp + facet_wrap(~z, scales="free", nrow=3) + theme_bw() + scale_x_continuous(limits=c(0, NA)) +
theme(axis.title.x=element_blank(), axis.title.y=element_blank(), 
	  panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
	  legend.position="none",
	  strip.background=element_rect(fill="grey90", colour="black"))
dev.off()	

################################################################################