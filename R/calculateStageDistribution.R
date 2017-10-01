#' Calculate Observed Stage Distribution
#'
#' @param dataset
#'
#' @export

calculateStageDistribution <- function(dataset, n.seeds) {
	dataset %<>% map(
		~ .$trans01 %>% 
			group_by(stage) %>%
			summarise(count = n()) %>%
			filter(!(is.na(stage))) %>%
			rbind.fill(data.frame(stage="Seed", count=n.seeds)) %>%
			mutate(
				`Stable Stage Value` = count/sum(count),
				SeedBankSize = "Observed", 
				SeedsPerFruit = "Observed", 
				SeedSurvival = "Observed",
				Distribution = "Observed"
			) %>%
			setnames("stage", "transition")
	)	
	dataset[[1]]$`Parent Assignment` <- largest.parent
	dataset[[2]]$`Parent Assignment` <- random.parent
	dataset[[3]]$`Parent Assignment` <- random.largest.parent
	return(dataset)
}