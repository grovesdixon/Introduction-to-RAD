#plot_admixture.R

##if you need packages
# install.packages(c('ggplot2', 'cowplot', 'tidyverse'))


library(ggplot2)
library(cowplot)
library(tidyverse)
setwd('~/Introduction-to-RAD/admixture_example/')


#basic barplot for admixture proprotions
tbl=read.table("corals.3.Q")
colnames(tbl) = c('Nearshore_preferring', 'Offshore_preferring', 'Deep_preferring')
names = read.table("names.txt")[,1]

#plot regular R style
quartz()
bp=barplot(t(as.matrix(tbl)), col=rainbow(3),xlab="", ylab="Ancestry", border=NA)
text(cex=1, x=bp, y=-.08, names, xpd=TRUE, srt=45)



#plot with tidyverse style
tbl$samples = names
tbl %>%
	gather(key='Type', value='mix', Nearshore_preferring, Offshore_preferring, Deep_preferring) %>%
	mutate(Type=factor(Type, levels=c('Nearshore_preferring', 'Offshore_preferring', 'Deep_preferring'))) %>%
	ggplot(aes(x=samples, y=mix, fill=Type)) +
		geom_bar(stat="identity") + 
		theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
		labs(x='Sample', y='Admixture')



