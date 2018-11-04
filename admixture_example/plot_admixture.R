#plot_admixture.R

##if you need packages
install.packages(c('ggplot2', 'cowplot', 'tidyverse'))


setwd('~/intro_to_rad_2018/admixture_example/')


#basic barplot for admixture proprotions
tbl=read.table("corals.3.Q")
colnames(tbl) = c('d', 'n', 'o')
names = read.table("names.txt")[,1]
bp=barplot(t(as.matrix(tbl)), col=rainbow(3),xlab="", ylab="Ancestry", border=NA)
text(cex=1, x=bp, y=-.08, names, xpd=TRUE, srt=45)

#plot with ggplot
library(ggplot2)
library(cowplot)
library(tidyverse)
tbl$names=names
tbl2=data.frame(melt(tbl, id.vars="names"))


tbl %>%
	mutate(samples=names) %>%
	gather(key='type', value='mix', d, n, o) %>%
	mutate(`Reef Type` = if_else(type=='d', 'Deep', 'Nearshore')) %>%
	mutate(`Reef Type` = if_else(type=='o', 'Offshore', `Reef Type`)) %>%
	mutate(`Reef Type`=factor(`Reef Type`, levels=c('Nearshore', 'Deep', 'Offshore'))) %>%
	ggplot(aes(x=samples, y=mix, fill=`Reef Type`)) +
		geom_bar(stat="identity") + 
		theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
		labs(x='Sample', y='Admixture')



