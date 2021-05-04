
#' @export
theme_ts_space<-theme_grey() +
		theme(
#		panel.grid.major = element_blank(),
#		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill="white", colour="black", size=2),
#		legend.key       = element_blank(),
#		legend.text      = element_text(size=20.5),
#		legend.text      = element_blank(),
#		legend.title     = element_text(size=20.5),
		axis.text.x = element_text(size=22,colour="black",hjust=1,angle=45),
		axis.text.y = element_text(size=22,colour="black",vjust=.3),
		axis.title.x = element_text(size=22),
		axis.title.y = element_text(size=22,vjust=-1),
		plot.title = element_text(hjust = 0.5,size=22,face="bold"),		
#		legend.position  = c(0.8,0.8)
		legend.position  = "none",
		panel.border = element_rect(colour = "black", fill=NA, size=2),
		plot.margin = unit(c(1,1,1,1), "cm")
		)


