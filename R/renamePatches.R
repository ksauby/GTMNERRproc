#' Rename Patches
#'
#' @param Data Dataset
#'
#' @export

renamePatches <- function(Data) {
	Data$Island %<>% as.factor
	Data$IslandFullNames <- Data$Island
	levels(Data$IslandFullNames) <- c(
		 "Island 1",
		 "Island 2",
		 "Island 3",
		 "Island 4",
		 "Island 5",
		 "Coastal\nGrassland",
		 "Beach\nDunes"
	)
	return(Data)
}
