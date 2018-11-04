#pi window solution

punFiles = list('chrI_punOnly.windowed.pi', 'chrII_punOnly.windowed.pi', 'chrXII_punOnly.windowed.pi')
punList = lapply(punFiles, readIn)
names(punList) = c('chr1', 'chr2', 'chr12')
plot_pi = function(x, title){
	ggplot(x, aes(x=mb, y=PI)) + 
		geom_smooth(span=0.2) +
		lims(y=c(0,0.00015)) +
		geom_point() +
		labs(x='Position (Mb)', subtitle=title)
}


plotlist = list(plot_pi(punList$chr1, 'chr1'),
				plot_pi(punList$chr2, 'chr2'),
				plot_pi(punList$chr12, 'chr12')
)

plot_grid(plotlist = plotlist, nrow=1)