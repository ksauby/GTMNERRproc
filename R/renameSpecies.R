#' Rename Species levels
#'
#' @param Plant_Surveys Plant Survey Dataset
#'
#' @export

renameSpecies <- function(Plant_Surveys) {
	Plant_Surveys %>% 
	mutate(
		Species	= replace(
			Species,
			which(Species=="pusilla"),
			"Opuntia pusilla"
		),
		Species	= replace(
			Species,
			which(Species=="stricta"),
			"Opuntia stricta"
		)
	)
}
