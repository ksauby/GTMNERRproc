#' Rename Species levels
#'
#' @param Plant_Surveys Plant Info
#'
#' @export

renameSpecies <- function(dat) {
	dat %>% 
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
