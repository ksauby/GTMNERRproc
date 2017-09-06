#' Create Vector of the Number of Individuals per Stage
#'

#' @param TMdata Data used to create the transition matrix.
#' @param stages vector of stage classes.

#' @export

calculateNumberIndivperStage <- function(TMdata, stages) {
		n_per_stage <- TMdata %>%
			group_by(stage) %>%
			summarise(n = length(PlantID)) %>%
			as.data.frame %>%
			# remove NAs
	   		filter(stage %in% stages) %>%
			# add missing stages
			rbind.fill(
				.,
				data.frame(
					stage = stages[!(stages %in% .$stage)],
					n = rep(0, length(stages[!(stages %in% .$stage)]))
				)
			)
		n_per_stage$stage %<>% factor(., levels=stages, ordered=T)
		n_per_stage %<>% arrange(stage)
		return(n_per_stage)	
	}
	
