#' Determine Fruit and Flower Presence
#'
#' @description Create separate variables indicating whether fruit and fruit and flowers were present.
#' @param Plant_Surveys Plant Survey Dataset
#'
#' @export

calculateFruitPresence <- function(Plant_Surveys) {
	Plant_Surveys %>% 
		mutate(
			FruitPres_t 		= ifelse(Fruit_t > 0, 1, 0),
			FruitFlowerPres_t 	= ifelse(Fruit_Flowers_t > 0, 1, 0),
			FruitPres_t = replace(
				FruitPres_t,
				which(is.na(Fruit_t)),
				NA
			),
			FruitPres_t = replace(
				FruitPres_t,
				which(is.na(Fruit_Flowers_t)),
				NA
			)
		)
}
