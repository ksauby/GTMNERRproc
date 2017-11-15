#' Plot Elasticity and Sensitivity Data
#'
#' @param dat Dataset
#' @param x_variable
#' @param y_variable
#' @param grouping_variable
#' @param facet_grid_var
#' @param xlab_label
#' @param ylab_label
#' @param n_transitions_per_stage
#'
#' @export


plotElasticitySensitivity <- function(dat, x_variable, y_variable, grouping_variable, facet_grid_var, xlab_label, ylab_label, n_transitions_per_stage, strip.text.x.size, strip.text.y.size, legend.text.size, axis.text.x.size, axis.title.x.size, axis.title.y.size, axis.text.y.size, strip.text.y.angle=270, color_values =c(
	black.red(n_transitions_per_stage),
	black.yellow(n_transitions_per_stage),
	black.blue.orange(n_transitions_per_stage),
	black.red.yellow(n_transitions_per_stage),
	black.purple.yellow(n_transitions_per_stage),
	black.darkblue.yellow(n_transitions_per_stage),
	black.brown.yellow(n_transitions_per_stage),
	black.brown.grey(n_transitions_per_stage),
	black.blue.brown(n_transitions_per_stage)),
	facet_grid_scales="fixed",
	coord_trans_y = "identity"
)  {
	ggplot(
		dat,
		aes(
			factor(eval(parse(text=x_variable))),
			eval(parse(text=y_variable)),
			fill=factor(eval(parse(text=grouping_variable)))
		)
	) +
		geom_bar(stat="identity") +
		KEStheme() +
		facet_grid(eval(parse(text=facet_grid_var)), scales=facet_grid_scales) +
		xlab(xlab_label) +
		ylab(ylab_label) +
		scale_fill_manual(values=c(color_values)) +
		theme(
			legend.position="bottom",
			legend.text = element_text(size = legend.text.size),
			axis.text.x = element_text(angle=45, hjust=1, size = axis.text.x.size),
			axis.text.y = element_text(size = axis.text.y.size),
			strip.text.y=element_text(size=strip.text.y.size, angle=strip.text.y.angle),
			strip.text.x=element_text(size=strip.text.x.size),
			axis.title.x=element_text(size=axis.title.x.size),
			axis.title.y=element_text(size=axis.title.y.size),
			text=element_text(family="serif")
		) +
		guides(
			fill=guide_legend(
				title="Transitions", 
				ncol=n_transitions_per_stage
			)
		) +
		coord_trans(y=coord_trans_y)
}



#' Plot Reproductive Values for LTRE
#'
#' @param dat Dataset
#' @param x_variable
#' @param y_variable
#' @param grouping_variable
#' @param facet_grid_var
#' @param xlab_label
#' @param ylab_label
#' @param n_transitions_per_stage
#'
#' @export


plotLTREReproductiveValues <- function(dat, x_variable, y_variable, grouping_variable, facet_grid_var, xlab_label, ylab_label, n_transitions_per_stage, strip.text.x.size, strip.text.y.size, legend.text.size, axis.text.x.size, axis.title.x.size, axis.title.y.size, axis.text.y.size, strip.text.y.angle=270, color_values =c(
	black.red(n_transitions_per_stage),
	black.yellow(n_transitions_per_stage),
	black.blue.orange(n_transitions_per_stage),
	black.red.yellow(n_transitions_per_stage),
	black.purple.yellow(n_transitions_per_stage),
	black.darkblue.yellow(n_transitions_per_stage),
	black.brown.yellow(n_transitions_per_stage),
	black.brown.grey(n_transitions_per_stage),
	black.blue.brown(n_transitions_per_stage)),
	facet_grid_scales="fixed",
	coord_trans_y = "identity"
)  {
	ggplot(
		dat,
		aes(
			factor(eval(parse(text=x_variable))),
			eval(parse(text=y_variable)),
			fill=factor(eval(parse(text=grouping_variable)))
		)
	) +
		geom_bar(stat="identity") +
		KEStheme() +
		facet_grid(eval(parse(text=facet_grid_var)), scales=facet_grid_scales) +
		xlab(xlab_label) +
		ylab(ylab_label) +
		scale_fill_manual(values=c(color_values)) +
		theme(
			legend.position="bottom",
			legend.text = element_text(size = legend.text.size),
			axis.text.x = element_text(angle=45, hjust=1, size = axis.text.x.size),
			axis.text.y = element_text(size = axis.text.y.size),
			strip.text.y=element_text(size=strip.text.y.size, angle=strip.text.y.angle),
			strip.text.x=element_text(size=strip.text.x.size),
			axis.title.x=element_text(size=axis.title.x.size),
			axis.title.y=element_text(size=axis.title.y.size), 
			text=element_text(family="serif")
		) +
		guides(
			fill=guide_legend(
				title="Transitions", 
				ncol=n_transitions_per_stage
			)
		) +
		coord_trans(y=coord_trans_y)
}
