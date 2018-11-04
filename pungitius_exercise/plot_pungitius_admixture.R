#plot_pungitius_admixture.R
library(ggplot2)
library(cowplot)
library(tidyverse)

setwd('~/Introduction-to-RAD/pungitius_exercise')

#load data
names = read.table('admixNames.txt')$V1

readin = function(fileName){
	df = read.table(fileName)
	colnames(df) = c('Tym', 'Pun', 'Sin')
	df$sample = names
	return(df)
}


chr1 = readin('chr1.3.Q')
par = readin('PAR.3.Q')
sdr = readin('SDR.3.Q')



#load sample information
sdat = read.table('sample_data.txt')
colnames(sdat) = c('sample', 'sex')
malePun = as.character(sdat$sample[grepl('Pun', sdat$sample) & sdat$sex=='M'])
femalePun = as.character(sdat$sample[grepl('Pun', sdat$sample) & sdat$sex=='F'])


#build plots

p1 <- chr1 %>% 
	gather(key='spp', value='mix', Tym, Pun, Sin) %>%
	mutate(sampleLab = if_else(sample %in% malePun, paste('M', sample, sep='_'), paste(sample))) %>% 
	mutate(sampleLab = if_else(sampleLab %in% femalePun, paste('F', sampleLab, sep='_'), paste(sampleLab))) %>% 
	arrange(sampleLab) %>%
	mutate(lab=factor(sampleLab, levels=unique(sampleLab))) %>%
	ggplot(aes(x= lab, y=mix, fill=spp)) +
		geom_bar(stat='identity') +
		labs(x='Sample', y='Admixture', subtitle='chr1') +
		theme(axis.text.x = element_blank(), legend.position='none')
		

ppar <- par %>% 
	gather(key='spp', value='mix', Tym, Pun, Sin) %>%
	mutate(sampleLab = if_else(sample %in% malePun, paste('M', sample, sep='_'), paste(sample))) %>% 
	mutate(sampleLab = if_else(sampleLab %in% femalePun, paste('F', sampleLab, sep='_'), paste(sampleLab))) %>% 
	arrange(sampleLab) %>%
	mutate(lab=factor(sampleLab, levels=unique(sampleLab))) %>%
	ggplot(aes(x= lab, y=mix, fill=spp)) +
		geom_bar(stat='identity') +
		labs(x='Sample', y='Admixture', subtitle='PAR') +
		theme(axis.text.x = element_blank(), legend.position='none')
		
psdr <- sdr %>% 
	gather(key='spp', value='mix', Tym, Pun, Sin) %>%
	mutate(sampleLab = if_else(sample %in% malePun, paste('M', sample, sep='_'), paste(sample))) %>% 
	mutate(sampleLab = if_else(sampleLab %in% femalePun, paste('F', sampleLab, sep='_'), paste(sampleLab))) %>% 
	arrange(sampleLab) %>%
	mutate(lab=factor(sampleLab, levels=unique(sampleLab))) %>%
	ggplot(aes(x= lab, y=mix, fill=spp)) +
		geom_bar(stat='identity') +
		labs(x='Sample', y='Admixture', subtitle='SDR') +
		theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position='bottom')


plot_grid(p1, ppar, psdr, nrow=3, rel_heights=c(1,1,1.6))
