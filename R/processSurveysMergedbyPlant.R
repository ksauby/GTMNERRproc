#' Process Plant Survey Data, per plant
#'
#' @description Steps:
#' \itemize{
#'  	\item merge records for the same plant from multiple plots; the data to be merged must have been collected on the same day.
#'  	\item Create separate variables indicating whether fruit and fruit and flowers were present
#'  	\item Calculate plant volume as a cone, cylinder, and elliptic cylinder
#'  	\item Determine if an insect species was ever detected during the study period
#'  	\item Rename species levels ("Opuntia stricta" instead of "stricta" and "Opuntia pusilla" instead of "pusilla")
#' 	 	\item Make new insect variables with "yes"/"no" values instead of 0/1, named "C_cactorum" and "M_prodenialis"
#'  	\item format the variables "ClusterID", "Network", "Island", "Species", "DemographicSurvey", "FecundityYear", "Year", "Season", "C_cactorum", "M_prodenialis" as factors
#' 	}
#'
#' @export
	
processSurveysMergedbyPlant <- function(Plant.Surveys, Plant.Info) {
	Plant.Surveys %>%
		mergePlantRecordsfromMultiplePlots(Plant.Info) %>%
		calculateFruitPresence %>%
		calculatePlantVolume %>%
		determineInsectPresenceDuringStudy %>%
		createInsectFactorVariables %>%
		formatasFactors(
			factors <- c(
				"Year", 
				"Season", 
				"C_cactorum", 
				"M_prodenialis"
			)
		)
}