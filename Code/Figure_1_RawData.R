library(foreign)
library(maptools)
library(RColorBrewer)
library(ggplot2)

## Source code for analysis
source("./Code/Functions_For_Analysis.R")
source("./Code/Functions_For_Maps.R")

## Read in the master list of country codes
codes <- as.data.frame(read.csv("./Data/Master_List.csv"))

## Remove Rwanda and Burundi
codes_noRB <- codes[-c(1,6),]
codes_noRB$Country <- factor(codes_noRB$Country)
rownames(codes_noRB) <- 1:nrow(codes_noRB)
	
## If only Rwanda and Burundi
codes_RB <- codes[c(1,6),]
codes_RB$Country <- factor(codes_RB$Country)
rownames(codes_RB) <- 1:nrow(codes_RB)
	
################################################################################

## Text: Supplementary Fig. 1
## Make maps of DHS data points

pdf("./Figures/Map_DHS_1.pdf", height=8, width=6)
MainFigure_DHS()
dev.off()

pdf("./Figures/Map_DHS_2.pdf", height=4, width=3)
SubFigure_DHS()
dev.off()

################################################################################