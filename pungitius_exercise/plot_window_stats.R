#!/usr/bin/env Rscript
#plot_window_stats.R

# #install packages if you need them:
# install.packages(c('vcfR', 'adegenet', 'ggplo2', 'tidyverse', 'cowplot'))

#SET WORKING DIRECTORY
setwd("~/Introduction-to-RAD/pungitius_exercise/")

#LOAD PACKAGES
library(ggplot2)
library(tidyverse)
library(cowplot)


#-------- Plot the windowed Fst between male and female Pun samples
readIn = function(fileName){
	df=read.table(fileName, header = T)
	df$mb = (df$BIN_END + df$BIN_START) / 2e6
	return(df)
}

fst1 = readIn('chrI_female_v_male.windowed.weir.fst')
fst2 = readIn('chrII_female_v_male.windowed.weir.fst')
fst12 = readIn('chrXII_female_v_male.windowed.weir.fst')

#plot each
plot(WEIGHTED_FST ~ mb, data = fst1, main='chr1', xlab='Position (Mb)')
plot(WEIGHTED_FST ~ mb, data = fst2, main='chr2', xlab='Position (Mb)')
plot(WEIGHTED_FST ~ mb, data = fst12, main='chr12', xlab='Position (Mb)')

#scale y limits based on chr12
YLIM=c(-0.05,0.3)

#make function to plot with loess smooth line
plot_smooth = function(df, title){
	plot(WEIGHTED_FST~BIN_START, data = df, ylim=YLIM, main=title)
	loess_fit <- loess(WEIGHTED_FST ~ BIN_START, df, span = 0.2, se = F)
	lines(df$BIN_START, predict(loess_fit),col='red',lwd=1.5)
}

#plot together
par(mfrow=c(1,3))
plot_smooth(fst1, 'chr1')
plot_smooth(fst2, 'chr2')
plot_smooth(fst12, 'chr12')


#repeat with tidyverse/ggplot
p1 <- fst1 %>%
	ggplot(aes(x=mb, y=WEIGHTED_FST)) + 
		geom_smooth() +
		geom_point() +
		lims(y=YLIM) +
		labs(x='Position (Mb)', subtitle='chr1')

p2 <- fst2 %>%
	ggplot(aes(x=mb, y=WEIGHTED_FST)) + 
		geom_smooth(span=0.2) +
		geom_point() +
		lims(y=YLIM) +
		labs(x='Position (Mb)', subtitle='chr2')

p3 <- fst12 %>%
	ggplot(aes(x=mb, y=WEIGHTED_FST)) + 
		geom_smooth(span=0.2) +
		geom_point() +
		lims(y=YLIM) +
		labs(x='Position (Mb)', subtitle='chr12')

plot_grid(p1,p2,p3, nrow=1)


#-------- write your own code to repeat for the window pi data


