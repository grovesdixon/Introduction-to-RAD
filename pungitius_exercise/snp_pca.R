#!/usr/bin/env Rscript
#snp_pca.R

# #install packages if you need them:
# install.packages(c('vcfR', 'adegenet', 'ggplo2', 'tidyverse', 'cowplot'))

#SET WORKING DIRECTORY
setwd("~/Introduction-to-RAD/pungitius_exercise/")


#LOAD PACKAGES
library(vcfR)
library(adegenet)
library(ggplot2)
library(tidyverse)
library(cowplot)


# BUILD PCA FOR CHR1

#read in the vcf
vcf_input = 'chrI.vcf'
gll=vcfR2genlight(read.vcfR(vcf_input))


#convert to genlight object
x=as.matrix(gll)
gi=as.genind(x)

#run pca to get 6 principal components
pca=glPca(gll,nf=6)
p1=data.frame(pca$scores)


#add a species column based on the sample ids
p1$species = factor(substr(start=1, stop=3, rownames(p1)), levels=c('Pun', 'Sin', 'Tym'))

#build plot with normal R
plot(PC2~PC1, data=p1, col=as.numeric(p1$species), pch=19)
legend('topright', legend=unique(p1$species), pch=19, col=c('black', 'green', 'red'))


#build plot with ggplot and tidyverse
p1 %>%
	ggplot() +
		geom_point(aes(x=PC1, y=PC2, color=species), size=2)
		
		
#REPEAT PCA FOR CHR2 AND CHR12

#make a funciton to do the above 
run_pca = function(vcf_input, chr){
	gll=vcfR2genlight(read.vcfR(vcf_input))
	x=as.matrix(gll)
	gi=as.genind(x)
	pca=glPca(gll,nf=6)
	p=data.frame(pca$scores)
	p$species = factor(substr(start=1, stop=3, rownames(p1)), levels=c('Pun', 'Sin', 'Tym'))
	p$chr = chr
	return(p)
}

#run the function for each input vcf
pca1 = run_pca('chrI.vcf', 'chr1')
pca2 = run_pca('chrII.vcf', 'chr2')
pca12 = run_pca('chrXII.vcf', 'chr12')


#make plots for all three
plot1 <- pca1 %>%
	ggplot() +
		geom_point(aes(x=PC1, y=PC2, color=species), size=2) +
		labs(title='chr1') + 
		theme(legend.position='none')

plot2 <- pca2 %>%
	ggplot() +
		geom_point(aes(x=PC1, y=PC2, color=species), size=2) +
		labs(title='chr2') + 
		theme(legend.position='none')

plot12 <- pca12 %>%
	ggplot() +
		geom_point(aes(x=PC1, y=PC2, color=species), size=2) +
		labs(title='chr12') 


#plot them together
plot_grid(plot1, plot2, plot12, rel_widths = c(1,1,1.25), nrow=1)



#WHAT'S GOING ON WITH CHR12?

#upload more sample information
sdf = read.table('sample_data.txt', row.names=1)
colnames(sdf) = c('sex')

#merge with the pc results
mp1 = merge(pca1, sdf, by = 0)
mp2 = merge(pca2, sdf, by = 0)
mp12 = merge(pca12, sdf, by = 0)


#plot again
#make plots for all three
plot1 <- mp1 %>%
	ggplot() +
		geom_point(aes(x=PC1, y=PC2, color=species, shape=sex), size=2) +
		labs(title='chr1') + 
		theme(legend.position='none')

plot2 <- mp2 %>%
	ggplot() +
		geom_point(aes(x=PC1, y=PC2, color=species, shape=sex), size=2) +
		labs(title='chr2') + 
		theme(legend.position='none')

plot12 <- mp12 %>%
	ggplot() +
		geom_point(aes(x=PC1, y=PC2, color=species, shape=sex), size=2) +
		labs(title='chr12')


#plot them together
plot_grid(plot1, plot2, plot12, rel_widths = c(1,1,1.4), nrow=1)






