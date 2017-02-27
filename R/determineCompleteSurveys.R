#' Determine whether a survey was complete
#'
#' @description Determine whether all data was collected during a survey.
#' Determine complete surveys for:
#' \itemize{
#'  \item insect surveys
#'  \item plant size
#'  \item size measurements (cm)
#'  \item fruit, flowers, and flower buds
#'  \item all data
#' 	}
#' @param Plant_Surveys Plant Survey Dataset
#'
#' @export

determineCompleteSurveys <- function(Plant_Surveys) {
	Plant_Surveys$complete_insect_surveys <- ifelse(rowSums(is.na(
		Plant_Surveys[, 
		c(
			"CA_t",
			"ME_t",
			"Unknown_Moth_t",
			"Old_Moth_Evidence_t"
		)]))==0, 1, 0
	)
	Plant_Surveys$complete_segments_surveys <- ifelse(rowSums(is.na(
		Plant_Surveys[, 
		c(
			"Plant_Segments_w_leaves",
			"Plant_Segments_wo_leaves",
			"Plant_Segments_woody"
		)]))==0, 1, 0
	)
	Plant_Surveys$complete_size_surveys <- ifelse(rowSums(is.na(
		Plant_Surveys[, 
		c(
			"Height_t",
			"Width_t",
			"Perpen_Width"
		)]))==0, 1, 0
	)
	Plant_Surveys$complete_fruit_surveys <- ifelse(rowSums(is.na(
		Plant_Surveys[, 
		c(
			"Num_FlowerBuds",
			"Num_Fruit_red",
			"Num_Fruit_green",
			"Num_Flowers"
		)]))==0, 1, 0
	)
	Plant_Surveys$complete_surveys <- ifelse(rowSums(is.na(
		Plant_Surveys[, 
		c(
		# insects
		"CA_t",
		"ME_t",
		"Unknown_Moth_t",
		"Old_Moth_Evidence_t",
		# number of segments
		"Size_t",
		"Plant_Segments_w_leaves",
		"Plant_Segments_wo_leaves",
		"Plant_Segments_woody",
		# size in cm
		"Height_t",
		"Width_t",
		"Perpen_Width",
		# flowers and fruit
		"Fruit_t" 
		)]))==0, 1, 0
	)
	return(Plant_Surveys)
}