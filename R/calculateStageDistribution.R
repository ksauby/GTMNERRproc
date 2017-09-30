#' Calculate Observed Stage Distribution
#'
#' @param dataset
#'
#' @export

calculateStageDistribution <- function(dataset) {
	dataset %>% map(
		~ .$trans01 %>% 
			group_by(stage) %>%
			summarise(count = n()) %>%
			mutate(
				`Stable Stage Value` = count/sum(count),
				SeedBankSize = "Observed", 
				SeedsPerFruit = "Observed", 
				SeedSurvival = "Observed"
			)
	)
}