#dapc_from_snps.R

#The first part is identical to the steps for PCA

#upload libraries
library(vcfR)
library(adegenet)
library(scales)


#set working directory
workingDir = "~/Introduction-to-RAD/DAPC_from_vcf"
setwd(workingDir)


#upload the vcf
gll=vcfR2genlight(read.vcfR("nonDeep_corals.vcf"))#New SNPs generated using mpileup and A.dig reference 8/28/17
class(gll)


#look at genlight object
x=as.matrix(gll)
gi=as.genind(x)


#assign populations (either Deep "D", Nearshore "N", or Offshore "O")
pop=substr(gll@ind.names,3,3)
pop(gll)=pop
popnum=as.numeric(factor(pop,levels=c("N", "O")))


#run principal component analysis
pca=glPca(gll,nf=2)


#plot the PCA results
quartz()
col=c("darkorange", "forestgreen")
s.class(pca$scores,pop(gll),col=col,axesell=F,cstar=0,grid=F)


# adegenet: finding clusters (even though we know what clusters we want) - choose 4 PCs and 2 groups
clus=find.clusters(gll,max.n.clus=15, n.clust=2, n.pca=4)

#now reassign the clusters based on our known population identities
clus$grp=pop

######## BUILD DISCIMINATE FUNCTION ########
#run dapc function on the snp data, differentiating between the populations "N" and "O"
dp=dapc(gi, pop=pop, n.da=1, perc.pca=80) 


#plot distributions of the samples along the Discriminate Axis
quartz()
scatter(dp,bg="white",scree.da=FALSE,legend=TRUE,solid=.4, col=col)



#for contrast, look at the distribution of Nearshores and Offshores on PC1

#set up the PC1 density data
scores=pca$scores
ldens=tapply(scores[,"PC1"], pop, density)
allx <- unlist(lapply(ldens, function(e) e$x))
ally <- unlist(lapply(ldens, function(e) e$y))

#build paired plot
quartz()
par(mfrow=c(1,2))

#pca
plot(allx, ally, type = "n", xlab ="PC1", ylab = "Density", axes=T)
for (i in 1:length(ldens)) {
	polygon(c(ldens[[i]]$x, rev(ldens[[i]]$x)), c(ldens[[i]]$y, 
	                  rep(0, length(ldens[[i]]$x))), col = alpha(col[i], 0.6), 
	                  lwd = 1, border = col[i])
}

#dapc
scatter(dp,bg="white",scree.da=FALSE,legend=F, solid=.4, col=col, main='DAPC')
legend('topleft', c('Nearshore', 'Ofshore'), pch=22, pt.bg = col)

#Note the the DAPC, as it is designed to do, provides 
#much better discrimination between nearshore and offshore corals

