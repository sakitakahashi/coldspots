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

## Source code for analysis
source("./Code/FunctionsForAnalysis.R")

## Read in the master list of country codes
codes <- as.data.frame(read.csv("./Data/Master_List.csv"))

## If only Rwanda and Burundi
codes_RB <- codes[c(1,6),]
codes_RB$Country <- factor(codes_RB$Country)
rownames(codes_RB) <- 1:nrow(codes_RB)
	
################################################################################

## Text: Supplementary Fig. 4 
## Smoothed effect of survey age

load("./Results/GAM_plot/All_countries.RData")

pdf("./Figures/Smooth_age_transformed.pdf", height=8, width=10)
tmp <- ggplot(dat_smooth, aes(x=x, y=plogis(y))) + 
geom_ribbon(aes(ymin=plogis(y_lower), ymax=plogis(y_upper)), fill=paste0("#587498", "4D")) + geom_line(colour="#587498", size=0.5) + ylim(0,1) + scale_x_continuous(breaks=c(6,12,24,36,48,60))
tmp + facet_wrap(~zz, scales="free", nrow=3) + theme_bw() + 
theme(axis.title.x=element_blank(), axis.title.y=element_blank(), 
	  panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
	  legend.position="none",
	  strip.background=element_rect(fill="grey90", colour="black"))
dev.off()

################################################################################

## Text: Supplementary Fig. 16
## Sensitivity analysis on effect of DHS cluster displacement

load("./Results/GAM_plot/All_countries.RData")
truth <- dat_smooth

load("./Results/GAM_plot/All_countries_Jitter.RData")
dat_smooth$truth <- truth$y

pdf("./Figures/Smooth_age_transformed_Jitter.pdf", height=6, width=12)
tmp <- ggplot(dat_smooth, aes(x=x, y=plogis(y))) + 
geom_ribbon(aes(ymin=plogis(y_lower), ymax=plogis(y_upper)), fill=paste0("#587498", "4D")) + geom_line(colour="#587498", size=0.5) + ylim(0,1) + scale_x_continuous(breaks=c(6,12,24,36,48,60)) +
geom_line(aes(y=plogis(truth)), colour="black", linetype=3)
tmp + facet_wrap(~zz, scales="free", nrow=2) + theme_bw() + 
theme(axis.title.x=element_blank(), axis.title.y=element_blank(), 
	  panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
	  legend.position="none",
	  strip.background=element_rect(fill="grey90", colour="black"))
dev.off()

load("./Results/GAM_plot/All_countries_Jitter_Comparison.RData")

pdf("./Figures/xy_Jitter.pdf", height=6, width=12)
tmp <- ggplot(dat_smooth_Jitter, aes(x=x, y=y))
tmp + geom_abline(intercept=0, slope=1, size=0.1, colour="red") + geom_point(size=0.1) + facet_wrap(~zz, scales="free", nrow=2) + theme_bw() + 
theme(axis.title.x=element_blank(), axis.title.y=element_blank(), 
	  panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
	  legend.position="none",
	  strip.background=element_rect(fill="grey90", colour="black"))
dev.off()

################################################################################

## Text: Supplementary Fig. 17
## Sensitivity analysis on effect of DHS weights

load("./Results/GAM_plot/All_countries.RData")
truth <- dat_smooth

load("./Results/GAM_plot/All_countries_Weights.RData")
dat_smooth$truth <- truth$y

pdf("./Figures/Smooth_age_transformed_Weights.pdf", height=6, width=12)
tmp <- ggplot(dat_smooth, aes(x=x, y=plogis(y))) + 
geom_ribbon(aes(ymin=plogis(y_lower), ymax=plogis(y_upper)), fill=paste0("#587498", "4D")) + geom_line(colour="#587498", size=0.5) + ylim(0,1) + scale_x_continuous(breaks=c(6,12,24,36,48,60)) +
geom_line(aes(y=plogis(truth)), colour="black", linetype=3)
tmp + facet_wrap(~zz, scales="free", nrow=2) + theme_bw() + 
theme(axis.title.x=element_blank(), axis.title.y=element_blank(), 
	  panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
	  legend.position="none",
	  strip.background=element_rect(fill="grey90", colour="black"))
dev.off()

load("./Results/GAM_plot/All_countries_Weights_Comparison.RData")

pdf("./Figures/xy_Weights.pdf", height=6, width=12)
tmp <- ggplot(dat_smooth_Weights, aes(x=x, y=y))
tmp + geom_abline(intercept=0, slope=1, size=0.1, colour="red") + geom_point(size=0.1) + facet_wrap(~zz, scales="free", nrow=2) + theme_bw() + 
theme(axis.title.x=element_blank(), axis.title.y=element_blank(), 
	  panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
	  legend.position="none",
	  strip.background=element_rect(fill="grey90", colour="black"))
dev.off()

################################################################################

## Text: Supplementary Fig. 14
## Comparison between model with and without the sub-national SIA covariate

load("./Results/GAM_plot/All_countries.RData")
truth <- dat_smooth
truth <- truth[which(truth$z==1 | truth$z==7),]

load("./Results/GAM_plot/Two_countries_NoSubnationals.RData")
dat_smooth_NoSubnationals$truth <- truth$y

pdf("./Figures/Smooth_age_transformed_NoSubnationals.pdf", height=3, width=6)
tmp <- ggplot(dat_smooth_NoSubnationals, aes(x=x, y=plogis(y))) + 
geom_ribbon(aes(ymin=plogis(y_lower), ymax=plogis(y_upper)), fill=paste0("#587498", "4D")) + geom_line(colour="#587498", size=0.5) + ylim(0,1) + scale_x_continuous(breaks=c(6,12,24,36,48,60)) +
geom_line(aes(y=plogis(truth)), colour="black", linetype=3)
tmp + facet_wrap(~zz, scales="free", nrow=1) + theme_bw() + 
theme(axis.title.x=element_blank(), axis.title.y=element_blank(), 
	  panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
	  legend.position="none",
	  strip.background=element_rect(fill="grey90", colour="black"))
dev.off()

load("./Results/GAM_plot/Two_countries_NoSubnationals_Comparison.RData")

pdf("./Figures/xy_NoSubnationals.pdf", height=3, width=6)
tmp <- ggplot(dat_smooth_NoSubnationals, aes(x=x, y=y))
tmp + geom_abline(intercept=0, slope=1, size=0.1, colour="red") + geom_point(size=0.1) + facet_wrap(~zz, scales="free", nrow=1) + theme_bw() + 
theme(axis.title.x=element_blank(), axis.title.y=element_blank(), 
	  panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
	  legend.position="none",
	  strip.background=element_rect(fill="grey90", colour="black"))
dev.off()

################################################################################