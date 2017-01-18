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

## Text: Supplementary Fig. 2a
## Plot age distribution of survey participants by country

load("./Results/AgeDistribution/All_countries.RData")

pdf("./Figures/AgeDistribution.pdf", height=8, width=10)
tmp <- ggplot(dat_AgeDist, aes(AgeDist)) + geom_histogram(bins=60, col="slategray", fill="slategray3", alpha=0.2) + scale_x_continuous(breaks=c(6,12,24,36,48,60))   
tmp + facet_wrap(~country, scales="free", nrow=3) + theme_bw() +
theme(axis.title.x=element_blank(), axis.title.y=element_blank(), 
	  panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
	  legend.position="none",
	  strip.background=element_rect(fill="grey90", colour="black"))
dev.off()

################################################################################

## Text: Supplementary Fig. 2b
## This is to make an ROC-type curve of age by country

load("./Results/AgeDistribution/All_countries_ROC.RData")

pdf("./Figures/AgeDistribution_ROC.pdf", height=8, width=9)
tmp <- ggplot(dat_AgeDist, aes(x=month, y=ROC, colour=country)) + 
	   scale_x_continuous(limits=c(6,60), breaks=c(6,12,24,36,48,60)) + 
	   scale_y_continuous(limits=c(0,1), breaks=seq(0,1,length=6)) + 
	   geom_vline(xintercept=c(6,12,24,36,48,60), linetype="dotted", colour="gray", size=0.2) + 
	   geom_hline(yintercept=seq(0,1,length=6), linetype="dotted", colour="gray", size=0.2) + 
	   geom_line(size=0.7) + scale_colour_manual("Country", values=paste0(brewer.pal(10,"Paired"), "CC"))
tmp + theme_bw() +
theme(axis.title.x=element_blank(), axis.title.y=element_blank(), 
	  panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
	  strip.background=element_rect(fill="grey90", colour="black"))	
dev.off()

################################################################################

## Text: Supplementary Fig. 3
## This is to plot empirical CDF (eCDF) by country, and SIA lines

load("./Results/eCDF/All_countries.RData")

## Get rid of SIAs with 0 eligs
dat_SIA <- dat_SIA[-(which(is.infinite(dat_SIA$lb)==TRUE)),]

## Set the y-axis values for each SIA campaign for each country
dat_SIA$yval <- 0
for(i in 2:nrow(dat_SIA)) {

	if(dat_SIA$country[i]==dat_SIA$country[i-1]) { dat_SIA$yval[i] <- dat_SIA$yval[i-1] + 0.025 }

}

pdf("./Figures/eCDF.pdf", height=8, width=10)
tmp <- ggplot(dat_eCDF, aes(x=x, y=y)) + ylim(0,1) + geom_segment(aes(x=lb, y=yval, xend=ub, yend=yval, colour=factor(national)), data=dat_SIA, size=0.8) + scale_colour_manual(values=c(paste0("#E3A6EC", "E6"),paste0("#99CC99", "E6"))) + geom_line(colour="grey20", size=0.5) + scale_x_continuous(breaks=c(6,12,24,36,48,60))   
tmp + facet_wrap(~zz, scales="free", nrow=3) + theme_bw() +
theme(axis.title.x=element_blank(), axis.title.y=element_blank(), 
	  panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
	  legend.position="none",
	  strip.background=element_rect(fill="grey90", colour="black"))
dev.off()

################################################################################