#' Create Vector of the Number of Individuals per Stage
#'

#' @param TMdata Data used to create the transition matrix.

#' @export

calculateNumberIndivperStage <- function(TMdata) {
		n_per_stage <- TMdata %>%
			group_by(stage) %>%
			summarise(n = length(PlantID))
	}
	
