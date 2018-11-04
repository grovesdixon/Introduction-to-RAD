#pca_from_snps.R

# #install the packages if necessary
# install.packages(c('vcfR', 'adegenet'))

#upload libraries
library(vcfR)
library(adegenet)

#if you need packages uncomment and execulte the command below:
# install.packages(c("vcfR", "adegenet"))



#set working directory
workingDir = "~/Introduction-to-RAD/PCA_from_vcf"
setwd(workingDir)


#upload the vcf
gll=vcfR2genlight(read.vcfR("corals.vcf"))#New SNPs generated using mpileup and A.dig reference 8/28/17
class(gll)



#look at genlight object
x=as.matrix(gll)
gi=as.genind(x)


#assign populations (either Deep "D", Nearshore "N", or Offshore "O")
pop=substr(gll@ind.names,3,3)
pop(gll)=pop
popnum=as.numeric(factor(pop,levels=c("D","N", "O")))


#run principal component analysis

pca=glPca(gll,nf=2)


#plot the results 
quartz()
col=c("dodgerblue", "firebrick", "orange")
s.class(pca$scores,pop(gll),col=col,axesell=F,cstar=0,grid=F)

#plot manually
pcdf = data.frame(pca$scores)
pcdf$reef_type = factor(substr(rownames(pcdf), start=3, stop=3), levels=c('D', 'N', 'O'))
colors = col[as.numeric(pcdf$reef_type)]
plot(PC2~PC1, data=pcdf, col='black', pch=21, bg=colors, cex=1.5)
legend('bottomleft', c('Deep', 'Nearshore', 'Offshore'), pch=21, pt.bg=col)








