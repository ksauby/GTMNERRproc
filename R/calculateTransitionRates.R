#' Create Transition Rate Matrix

#' @param transition_counts Matrix of transition counts (representing the number of individuals observed to transition from the stage represented by the column name to the stage represented by the row name).
#' @param n_per_stage Observed number of individuals per stage

#' @return Returns A Matrix of the same dimensions as transition_counts that gives the rates at which individuals transition from one stage to another. This is calculated as the number in a given transition_counts cell divided by the number of individuals observed in that stage (the stage given by the column name of that transition_counts cell).

#' @export

calculateTransitionRates <- function(transition_counts, n_per_stage) {
	x <- t(t(transition_counts) / n_per_stage)
	x[which(x=="NaN")] <- 0
	return(x)
}