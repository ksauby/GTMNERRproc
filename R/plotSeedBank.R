#' Plot Seed Bank Data
#'
#' @param dat Dataset
#' @param x_variable
#' @param  grouping_variable
#' @param facet_grid_var
#' @param legend_title Default value is "Parent Assignment\nMethod."
#' @param xlab_label
#'
#' @export

plotSeedBank <- function(dat, x_variable, grouping_variable, facet_grid_var, legend_title="Parent Assignment\nMethod", xlab_label) {
	ggplot(
		dat,
		aes(
			x = eval(parse(text=x_variable)),
			y = lambda,
			colour=factor(eval(parse(text=grouping_variable))),
			linetype=factor(eval(parse(text=grouping_variable)))
		)
	) +
	KEStheme() +
	geom_line(size=2) +
	facet_grid(eval(parse(text=facet_grid_var))) +
	theme(
		legend.position="bottom", 
		axis.text.x=element_text(angle=45, hjust=1),
		panel.spacing = unit(0, "lines"), 
	    strip.background = element_blank(),
	    strip.placement = "outside"
	) +
	ylab(expression(lambda)) +
	xlab(xlab_label) +
	guides(
		colour=guide_legend(title=legend_title),
		linetype=guide_legend(title=legend_title)
		) +
	coord_trans(x="log10") +
	scale_x_continuous(breaks=c(1,10,100,1000,10000,1000000)) +
	scale_y_continuous(breaks=c(1:5), limits=c(1,5)) +
	scale_linetype_manual(values=c("solid","dotdash","dashed")) +
	scale_colour_manual(values=c("black","purple","orange"))
}
